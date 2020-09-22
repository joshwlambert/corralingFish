#install packages and download them into the working environment
install.packages("fishtree")
install.packages("ape")
install.packages("geiger")
install.packages("phytools")
install.packages("diversitree")
library(fishtree)
library(ape)
library(geiger)
library(phytools)
library(diversitree)

#1 download TEP reef fish data and isolate just the species names to be used
reef_fish_data <- read.csv("data/GalapagosFishes.csv")
species_names <- reef_fish_data$species_SFTEP

#2 use fishtree compare and isolate the tree for just the TEP reef fish

#2.1       comparing the species_names vector to the whole phylogeny (with stochastic species resolution ~32000 species)
phy_1 <- fishtree::fishtree_complete_phylogeny(species = species_names)
phy_1
#if phy_1 creates a list of a lot of different trees from a posterior distribution
#could just use a random number generator or just pick one?
phy_1tree <- phy_1[[52]]
#par(mfrow=2)
plot(phy_1tree, show.tip.label = FALSE)
ltt.plot(phy_1tree)

#2.2       comparing the species_names vector to the Fish Tree of Life phylogeny (~11, 000 species with genentic data)
phy_2 <- fishtree::fishtree_phylogeny(species = species_names)

plot(phy_2, show.tip.label = FALSE)
ltt.plot(phy_2)
#par #### I think this is witchcraft I still dont know how to properly size these things


########################
#taken as the species list from phy_2, created to vectors to show the species not listed
#therefore all these swimmy swam boys have ecological data but no genetic data?
trimmed_species <- phy_2[["tip.label"]]
species_not_included <- setdiff(species_names, trimmed_species)



#2.3     comparing the trimmed_species vector to Fish Tree of Life phylogeny (~11000 species)
phy_3 <- fishtree::fishtree_phylogeny(species = trimmed_species)
#par(mfrow=c(2,1)) ------again, witchcraft, who believes in readable trees anyways?
plot(phy_3, show.tip.label = TRUE)
ltt.plot(phy_3)

reef_fish_data_V2 <- reef_fish_data[-c(32, 123, 129, 230, 292, 298, 354, 373, 376, 404, 406, 433, 445), ]

reef_endemics <- reef_fish_data$Galapagos_Only
rownames(reef_endemics) <- species_names

phy_2$tip.label

name.check(phy = phy_2$tip.label, data = reef_endemics)

reef_endemics <- reef_endemics[, -2]



fmode <- as.factor(setNames(reef_fish_data_V2$Galapagos_Only, reef_fish_data_V2$species_SFTEP))

