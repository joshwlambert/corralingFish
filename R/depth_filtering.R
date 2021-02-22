

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
depth_range <- data.frame()

#filtering loop
for(i in shapefiles_data$iucn_names){
  output_seach[[i]] <- rredlist::rl_search(i)

  #population  new df
  #create row data
  iucn_name <- as.character(i)
  depth_upper <- as.numeric(unlist(output_seach[[i]][["result"]]["depth_upper"]))
  depth_lower <-as.numeric(unlist(output_seach[[i]][["result"]]["depth_lower"]))

  #create the new row
  new.row <- data.frame(iucn_names = iucn_name,
                        depth_upper = depth_upper, depth_lower = depth_lower)
  #bind to beginning dataframe
  depth_range<- rbind(depth_range, new.row)
}


#combine depth range dataframe with input dataframe to create new reference
fishes_df <- data.frame()
fishes_df <- merge(fish_data, depth_range)

write.csv(fishes_df, "depth_trimmed_taxa.csv")
#!!!!!!!!!!!!!!!!!!!!!!!!some names with not translate, fucking taxonomy!!!!!!
