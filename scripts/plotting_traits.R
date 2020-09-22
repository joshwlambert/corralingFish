#plot a tree with a binary trait at the tips to represent reef associated

#load libraries
library(fishtree)
library(rfishbase)

#load data
reef_fish_data <- read.csv("data/GalapagosFishes.csv")

#extract all species from fishbase
all_perciformes <- rfishbase::species_list(Order = "Perciformes")
all_tetraodontiformes <- species_list(Order = "Tetraodontiformes")

#extract species from our reef data
reef_perciformes_data <- reef_fish_data[which(reef_fish_data$Order == "Perciformes"), ]
reef_perciformes <- reef_perciformes_data$species_SFTEP

reef_tetraodontiformes_data <- reef_fish_data[which(reef_fish_data$Order == "Tetraodontiformes"), ]
reef_tetraodontiformes <- reef_tetraodontiformes_data$species_SFTEP

#extract molecular phylogeny of species
mol_phy_all_perciformes <- fishtree_phylogeny(species = all_perciformes)
mol_phy_all_tetraodontiformes <- fishtree_phylogeny(species = all_tetraodontiformes)

#plot phylogeny
plot(mol_phy_all_perciformes, show.tip.label = FALSE)
plot(mol_phy_all_tetraodontiformes, show.tip.label = FALSE, no.margin = TRUE)

#categorise species as reef associated
reef_tetraodontiformes_trait_data <- data.frame(all_tetraodontiformes)
reef_tetraodontiformes_trait_data$trait <- rep("non_reef", length(reef_trait_data$all_tetraodontiformes))
reef_tetraodontiformes_trait_data[which(reef_tetraodontiformes_trait_data$all_tetraodontiformes %in% reef_tetraodontiformes), 2] <- "reef"

#plot traits on phylogeny

fmode<-as.factor(setNames(reef_tetraodontiformes_trait_data$trait, reef_tetraodontiformes_trait_data$all_tetraodontiformes))
dotTree(mol_phy_all_tetraodontiformes, fmode, colors=setNames(c("blue","red"),
                                       c("non_reef","reef")),ftype="i",fsize=0.1)
