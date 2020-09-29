#plot a tree with a binary trait at the tips to represent galapagos endemics

################load libraries
library(fishtree)
library(rfishbase)
library(dplyr)
library(ape)
library(maps)
library(phytools)


##################load data
reef_fish_data <- read.csv("../GalapagosFishes_AA (2).csv")

################## create a family list of endemics

family_endlist <- c("Atherinopsidae", "Bythitidae", "Chaenopsidae", "Chlopsidae",
                    "Clupeidae", "Dactyloscopidae", "Gobiidae", "Haemulidae",
                    "Kyphosidae", "Labrisomidae", "Mugilidae", "Ogcocephalidae",
                    "Ophichthidae", "Paralichthyidae", "Pomacentridae",
                    "Sciaenidae", "Scorpaenidae", "Serranidae", "Sparidae",
                    "Triglidae", "Tripterygiidae")

all_famend <- list()

for (i in family_endlist) {
  all_famend[[i]] <- rfishbase::species_list(Family = i )
}

####################extract species from OG csv into a dataframe
####################with only endemic species

gala_endemic_data <-reef_fish_data %>%
    dplyr::filter( Family %in% family_endlist & Galapagos_Only == 1 )

# gala_endemic <- list()
#
# for (i in family_endlist) {
#   gala_end[[i]] <-
#   }

############################extract moe





