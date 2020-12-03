install.packages("maps")
install.packages("ggmap")
library(ggmap)
library(maps)
library(dplyr)

NOAA <- read.csv("./Friedlander_data/Hawaii_NOAA_fish_data.csv")
# ll <- unique(NOAA[,5:8]) # NEVER SUBSET BY COL NUMBER -PG

sample_points = distinct(NOAA, Lat, Long, Island, Region)
# MHI_ll <- ll %>% filter(Region == "MHI")
#install.packages("usmap")
#library(usmap)

#usmap::us_map(include = c("HI"))
#points(MHI_ll$Long, MHI_ll$Lat, col = 2, pch = 16)


#maps::map(database = "world", wrap = c(0,360))
#points(ll$Long, ll$Lat, col = 2, pch = 16)
#usa = rnaturalearth::ne_states(country = "united states of america",
#                                 returnclass = "sf")

# get data
library(sf)
states = sf::st_read("ne_50m_admin_1_states_provinces/")
hwi = dplyr::filter(states, name_id == "Hawaii")

# make sample points sf
sample_points_sf = sf::st_as_sf(sample_points,
                                coords = c("Long", "Lat"))

# check crs
st_crs(hwi)
st_crs(sample_points_sf)

# set crs
st_crs(sample_points_sf) = st_crs(hwi)

# plot with ggplot
library(ggplot2)

# transform data to 2782
sample_pts_utm = st_transform(sample_points_sf, 2782)

# flatten the layer
sample_pts_utm = st_combine(sample_pts_utm)

# buffer around points
sample_buffer = st_buffer(sample_pts_utm, dist = 500000)

# retransform
sample_buffer = st_transform(sample_buffer, 4326)

ggplot()+
  geom_sf(data = hwi, colour = "black")+
  geom_sf(data = sample_points_sf,
          colour = "red",
          size = 0.2)+
  geom_sf(data = sample_buffer,
          fill = "blue",
          alpha = 0.2)+
  theme_bw()

