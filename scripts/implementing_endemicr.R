#what do I need to do?
#load packages
library(sf)
library(endemicr)
library(dplyr)
#necessary for proper functioning of script
setwd("../corralingfish/endemicr_testdata")

#1.load in hawaiian objects, make sure to set up buffer around the archipelago
#1.1prepare hawaii boundaries
hawaii_boundaries <- list("./inst/boundary_main_islands/",
                          "./inst/boundary_nwhi/")

#1.2 get a buffer around hawaii boundaries
# this is done automatically in the function check_hawaii_endemic
hawaii_buffer <- lapply(hawaii_boundaries, function(x) {
  x <- st_read(x)
  x <- st_transform(x, 2782)
  x <- st_buffer(x, 50 * 1000)
  x <- st_union(x)
  x <- st_transform(x, 4326)
})

#1.3 Reduce and join
hawaii_buffer <- Reduce(c, hawaii_buffer)

#1.4 notice the overlap
plot(hawaii_buffer,
     col = "grey90",
     main = "Notice the overlap"); axis(1); axis(2)


#2.1 the demarcation function defaults to three, need to put in values to split
    #nwhi and mhi
hawaii_regions <- demarcate_regions(
  area_of_interest = hawaii_buffer,
  region_demarcation_points = list(
    main_islands = c(-160.1, 21.8),
    nwhi = c(-161.9, 23))
)

# 2.2 plot for sanity check
plot(hawaii_regions$regions,
     reset = F,
     main = "Does this look alright?"); axis(1); axis(2)
plot(hawaii_regions$demarcation_areas,
     col = NA,
     add = T)

# 3 load in csv data to pull fish names from
fish_data <- read.csv("../depth_trimmed_taxa.csv")
View(fish_data)

###### choose 3.1 if working with trimmed_NOAA_taxa.csv; choose 3.2 if using a
###### depth-filtered csv. Ex: depth_trimmed_molecular_taxa.csv.
###### these csv files have already gone through this filtereing.

#3.1.create fish lists to pull from based on iucn_shapefile, molecular_data
molecular_data <- fish_data %>% filter(molecular_data == 1)
# molecular_shapefiles <- fish_data %>% filter(iucn_shapefile == 1,
#                                              molecular_data == 1)
# shapefiles_data <- fish_data %>% filter(iucn_shapefile ==1)

#3.2 filtering for depth; shallow/mesophotic reef fish (0-150m)

SM_fishes <- fish_data %>% filter(depth_lower <= 150)

write.csv(SM_fishes, "./shalmeso_shapefile_fish.csv")

molecular_data <- SM_fishes%>% filter(molecular_data == 1)

write.csv(molecular_data , "./shalmeso_molec_shapefile_fish.csv")

### 4. Get _p_(global range) in each area
#combine earlier steps to create file_path with this so
## ex: i = Acanthurus_blochii instead of ./shapefiles/Acanthururus_blochii/data_0.shp
# proportionof the global range in each area of hawaii
#4.1 assign the filtered dataset you want to use to fishes vector for downstream
fishes <- SM_fishes$Taxa

# Prep data from endemic r
#extract .shp files for all the fish
#fis <- vector()
range_in_hawaii <- list()
# for(i in fishes){
#   species_folder <- file.path("./fish_shapefiles",i , "data_0.shp")
#   fis <- c(fis, species_folder)
# }

for(i in fishes){
range_in_hawaii[[i]] <- check_endemic_hawaii(
  hawaii_land_features = hawaii_boundaries,
  region_demarcation_points = list(
    main_islands = c(-160.1, 21.8),
    nwhi = c(-161.9, 23)),
  buffer_distance_km = 150,
  species_range = file.path("./fish_shapefiles",i , "data_0.shp"))
# print this
print(i)
}


#flatten list to create a single dataframe
# df_hawaii_range <- data.frame(matrix(unlist(range_in_hawaii),
#                                      nrow= length(range_in_hawaii),
#                                      byrow=T, ncol = 3),stringsAsFactors=FALSE)

install.packages("tidyverse")
library(tidyverse)

df_hawaii_range_150 <- bind_rows(range_in_hawaii, .id = "id")
#save as a csv file, as it will be easier to manipulate in excel
write.csv(df_hawaii_range, "../shapefile_ranges_150kbuff.csv")

write.csv(df_hawaii_range, "../shapefile_150_depth.csv")
#use histogram to visualize data

onefiveoh <- read.csv("../shapefile_ranges_150kbuff.csv")
onefiveoh <- onefiveoh[-c(200:408),]

#find correct level of p_range to delineate endemic vs. nonendemic

first_places <- ggplot(onefiveoh, aes(onefiveoh$total)) +
  geom_histogram(binwidth = 0.02)

first_mean <- ggplot(onefiveoh, aes(onefiveoh$total)) +
  geom_histogram(binwidth = 0.02) +
  geom_vline(aes(xintercept = mean(onefiveoh$total)), color = "blue",
             linetype = "dashed") +
  labs(title = "Total proportion range of fishes",
       x = "total proportion range")+
  geom_text(x = mean(onefiveoh$total), y = 0,
            label = "mean proportion range", vjust = -10, hjust = -0.1)

first_mean_limited <- ggplot(onefiveoh, aes(onefiveoh$total)) +
  geom_histogram(binwidth = 0.02) +
  geom_vline(aes(xintercept = mean(onefiveoh$total)), color = "blue",
             linetype = "dashed") +
  labs(title = "Total proportion range of fishes",
       x = "total proportion range")+
  geom_text(x = mean(onefiveoh$total), y = 0,
            label = "mean proportion range", vjust = -10, hjust = -0.1) +
  coord_cartesian(xlim = c(-0.1, 1))



#for the hundred k plots

View(hundredK_plot)
View(meanhundredK_plot)
View(thinner_hundred)


