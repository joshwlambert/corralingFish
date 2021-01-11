#what do I need to do?
#load packages
library(sf)
library(endemicr)
library(dplyr)
#necessary for proper functioning of script
setwd("../corralingfish/endemicr_testdata")


#load in csv data to pull fish names from
fish_data <- read.csv("../trimmed_NOAA_taxa.csv")
View(fish_data)

#create fish lists to pull from based on iucn_shapefile, molecular_data
molecular_data <- fish_data %>% filter(molecular_data == 1)
molecular_shapefiles <- fish_data %>% filter(iucn_shapefile == 1,
                                             molecular_data == 1)
shapefiles_data <- fish_data %>% filter(iucn_shapefile ==1)

molecular_shapefile_endemics <- fish_data %>% filter(Endemic_Revised == 1,
                                                     molecular_data == 1,
                                                     iucn_shapefile == 1)

#assign the filtered dataset you want to use to fishes vector for downstream
fishes <- molecular_shapefiles$Taxa

#1. Prep data from endemic r
#extract .shp files for all the fish
fis <- vector()
for(i in fishes){
  species_folder <- file.path("./fish_shapefiles",i , "data_0.shp")
  fis <- c(fis, species_folder)
}

#load in hawaiian objects, make sure to set up buffer around the archipelago
# prepare hawaii boundaries
hawaii_boundaries <- list("./inst/boundary_main_islands/",
                          "./inst/boundary_nwhi/")

# get a buffer around hawaii boundaries
# this is done automatically in the function check_hawaii_endemic
hawaii_buffer <- lapply(hawaii_boundaries, function(x) {
  x <- st_read(x)
  x <- st_transform(x, 2782)
  x <- st_buffer(x, 50 * 1000)
  x <- st_union(x)
  x <- st_transform(x, 4326)
})

# Reduce and join
hawaii_buffer <- Reduce(c, hawaii_buffer)

# notice the overlap
plot(hawaii_buffer,
     col = "grey90",
     main = "Notice the overlap"); axis(1); axis(2)


# the demarcation function defaults to three, need to put in values to split
    #nwhi and mhi
hawaii_regions <- demarcate_regions(
  area_of_interest = hawaii_buffer,
  region_demarcation_points = list(
    main_islands = c(-160.1, 21.8),
    nwhi = c(-161.9, 23))
)

# plot for sanity check
plot(hawaii_regions$regions,
     reset = F,
     main = "Does this look alright?"); axis(1); axis(2)
plot(hawaii_regions$demarcation_areas,
     col = NA,
     add = T)

### Get _p_(global range) in each area
#combine earlier steps to create file_path with this so
## ex: i = Acanthurus_blochii instead of ./shapefiles/Acanthururus_blochii/data_0.shp
# proportionof the global range in each area of hawaii
range_in_hawaii <- list()
for(i in fis){
range_in_hawaii[[i]] <- check_endemic_hawaii(
  hawaii_land_features = hawaii_boundaries,
  buffer_distance_km = 50,
  species_range = i)
# print this
print(i)
}

#flatten list to create a single dataframe

#use histogram to visualize data

#find correct level of p_range to delineate endemic vs. nonendemic
