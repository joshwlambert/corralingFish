#load packages
library(rredlist)
library(knitr)
library(dplyr)

############ vignette example ###############
#create output vector for a single species (pedro vignette)
search_output <- rredlist::rl_search("Chaetodon ornatissimus")
knitr::kable(search_output$result)

#extract depth range for single species
depth_range <- c(
  "depth_upper" = as.numeric(search_output$result["depth_upper"]),
  "depth_lower" = as.numeric(search_output$result["depth_lower"])
  )

depth_range

#################try with dataset##############
#load in csv data
fish_data<- read.csv("trimmed_NOAA_taxa.csv")

#subset data based on type of data availability
molecular_data <- fish_data %>% filter(molecular_data == 1)
molecular_shapefiles <- fish_data %>% filter(iucn_shapefile == 1,
                                             molecular_data == 1)
shapefiles_data <- fish_data %>% filter(iucn_shapefile ==1)

#create output list for multiple species
output_seach<- list()
for(i in molecular_shapefiles$Taxa){
output_seach[[i]] <- rredlist::rl_search(i)

#extract depth ranges and input into new dataframe
depth_range <- c(
  "depth_upper" = as.numeric(search_output$result["depth_upper"]),
  "depth_lower" = as.numeric(search_output$result["depth_lower"])
)

depth_range
}
#combine depth range dataframe with input dataframe to create new reference
#!!!!!!!!!!!!!!!!!!!!!!!!some names with not translate, fucking taxonomy!!!!!!
