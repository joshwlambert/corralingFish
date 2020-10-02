#plot a tree with a binary trait at the tips to represent reef fish endemics

################load libraries
library(fishtree)
library(rfishbase)
library(dplyr)
library(ape)
library(maps)
library(phytools)


##################load data

reef_fish_data <- read.csv("../GalapagosFishes_AA (2).csv")

################create a family list of endemics and get all the names

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

#########extract species from OG csv into a dataframe with only endemic species

gala_endemic_data <-reef_fish_data %>%
    dplyr::filter( Family %in% family_endlist & Galapagos_Only == 1 )

##########extract just the names of the species endemic per family
reef_all <- list()
reef_famall <- list()

for ( i in family_endlist) {

  reef_all[[i]] <- gala_endemic_data[which(gala_endemic_data$Family == i),]
  reef_famall[[i]] <- reef_all[[i]][,5]
  reef_famall[[i]] <- gsub(x = reef_famall[[i]] , pattern = " ", replacement = "_")
}

##############extract molecular phylogeny of species

all_mol_trees <- list()

for (i in family_endlist) {
  all_mol_trees[[i]] <-
    fishtree::fishtree_phylogeny(species = (unlist(all_famend[[i]])))
  }

###########create trimmed species lists based on molecular tree data

trimmed_mol_trees <- list()

for ( i in family_endlist) {
  trimmed_mol_trees[[i]] <- all_mol_trees[[i]]["tip.label"]
}

#############split out the families, and categorise as reef associated within

endemic_trait_data <- list()


for (i in family_endlist) {
  endemic_trait_data[[i]] <- data.frame(trimmed_mol_trees[[i]])
  endemic_trait_data[[i]]["trait"] <- rep("nonendemic",
                                          length(endemic_trait_data[[i]]))
  endemic_trait_data[[i]][which(endemic_trait_data[[i]][["tip.label"]]
                                %in% reef_famall[[i]]), 2] <- "endemic"
  #endemic_trait_data[[i]] <- row.names(endemic_trait_data[[i]]["tip.label"])
}

############ create fmode vectors?

fmode_all <- list()

for (i in family_endlist) {

  fmode_all[[i]] <- as.factor(setNames(endemic_trait_data[[i]]$trait,
                                   endemic_trait_data[[i]]$tip.label))
}

########## create dotTree for each family, and save output at pdf file

for (i in family_endlist) {

  dotTree(all_mol_trees[[i]], fmode_all[[i]],
          colors=setNames(c("blue", "red"), c("nonendemic", "endemic")),
          ftype= "i", fsize = 0.1, no.margin = TRUE)
  legend(x = 5 , legend = i )
  }


#how to actually assign names to vectors in a foor loop
# for (i in 1:10) {
# print(paste("fmode_", i, sep = ""))
# }




