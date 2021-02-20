#Aim is the categorise endemics and nonendemics for Hawaii

#requires:
#   1: boundary box/ sample buffer around islands
#   2: list of species
#   3: range data for list of species

#get boundary box for the Hawaiian archipelago (from map_stuff.R scipt xo Pratik)
NOAA <- read.csv("./Friedlander_data/Hawaii_NOAA_fish_data.csv")
sample_points = distinct(NOAA, Lat, Long, Island, Region)
  #get data
states = sf::st_read("./geo_needs/ne_50m_admin_1_states_provinces/")
hwi = dplyr::filter(states, name_id == "Hawaii")
  # make sample points sf
sample_points_sf = sf::st_as_sf(sample_points,
                                coords = c("Long", "Lat"))
  # check crs
st_crs(hwi)
st_crs(sample_points_sf)
  # set crs
st_crs(sample_points_sf) = st_crs(hwi)
  # transform data to 2782
sample_pts_utm = st_transform(sample_points_sf, 2782)
  # flatten the layer
sample_pts_utm = st_combine(sample_pts_utm)
  #create bounding box
boun_box <- st_bbox(sample_points_sf)
  #plot with all sampling points and boundary box
ggplot()+
  geom_sf(data = hwi, colour = "black")+
  geom_sf(data = sample_points_sf,
          colour = "red",
          size = 0.2)+
  coord_sf(xlim = c(-178.3843, -154.8066),
           ylim = c(18.9192, 28.4595))+
  theme_bw()

#try with two different lists: molecular tree list and non-molecular tree list
fish_data <- read.csv("./trimmed_NOAA_taxa.csv")

molecular_fish <- fish_data %>% filter(molec_data == 1) %>%
    select(Taxa)
all_fish <- fish_data %>% select(Taxa,Genera)
all_taxa <-all_fish$Taxa
#check taxa against ICUN search
#Hawaii, Actinopterygii, Marine Neritic : reef
iucn_list <-
  read.csv("../../redlist_species_data_a5756fa4-1aab-43d7-a26e-d7f1dae58be5/taxonomy.csv")

iucn_list$scientificName <- gsub(" ", "_", iucn_list$scientificName)
iucn_fish <- iucn_list$scientificName

differ <-setdiff(iucn_fish, all_fish$Taxa)
#iucn_fish[!iucn_fish %in% all_taxa]
#also load in synonyms csv
syn_iucn <-
  read.csv("../../redlist_species_data_a5756fa4-1aab-43d7-a26e-d7f1dae58be5/synonyms.csv")

syn_iucn$scientificName <- gsub(" ", "_", syn_iucn$scientificName)
#download in actinopetrygian range data from IUCN
install.packages("rgdal")
library(rgdal)

fish <- readOGR(dsn = "./actino", layer = "data_0")
