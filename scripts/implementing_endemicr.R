#what do I need to do?

#require for endemicr

#list of hawaiian shapefiles
#list of species
#shapefiles for the species
library(sf)
library(endemicr)
###########FIGURE OUT WHERE we want working directory to be?


#extract .shp files for all the fish
#creates vectors will file path for shapefiles of each species
#fishes$fish_TEST defines shallow water reef fish that has both
#iucn_shapefiles and molecular tree data. This can be changed.
fi <- vector()
for(i in fishes$fish_TEST){
 species_name <- file.path("./2_shapefiles", i);
 species_name <- paste0(species_name, ".shp")
 fi <- c(fi, species_name )
}

#check fish ranges to see if they look all right
#use fi file paths to generate plots for each of the fish
#make sure that the files are correct/aren't corrupted.

# read in the example fish Chaetodon ornatissimus
fish_range_sf <- st_read(fish_range)

# plot
plot(fish_range_sf$geometry,
     col = "red",
     border = NA,
     main = "The global range of this fish", xaxs = "r"); axis(1); axis(2)
