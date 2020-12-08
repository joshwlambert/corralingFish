#Aim is the categorise endemics and nonendemics for Hawaii

#requires:
#   1: boundary box/ sample buffer around islands
#   2: list of species
#   3: range data for list of species

#get boundary box for the Hawaiian archipelago

#try with two different lists: molecular tree list and non-molecular tree list
fish_data <- read.csv("./trimmed_NOAA_taxa.csv")

molecular_fish <- fish_data %>% filter(molecular_data == 1) %>%
    select(Taxa)
all_fish <- fish_data %>% select(Taxa)

#download in actinopetrygian range data
