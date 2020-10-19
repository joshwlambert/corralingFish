install.packages("fistree")
install.packages("ape")
install.packages("maps")
install.packages("phytools")

library(fishtree)
library(ape)
library(maps)
library(phytools)

#load in dataset
Hawaii_NOAA_fish_data <-
  read_csv("~/Desktop/S3/corralingfish/Hawaii_NOAA_fish_data.csv")

#isolate taxon list and prune to only uniqe hits
taxon_hits <- Hawaii_NOAA_fish_data$Taxon
taxon_list <- data.frame(unique(taxon_hits))
View(taxon_list)

#prune out unique hits that are missing genus and species
#figure out a better way to do this, sorry Josh and Pedro
new_taxon_list <- taxon_list[-c(2, 58, 97, 122, 123, 143, 145, 163, 165, 166,
                                170, 173, 179, 186, 189, 194, 226, 227, 245,
                                246, 254, 257, 266, 267, 268, 272, 274, 281,
                                284, 285, 287),]

#create a tree from molecular fishtree phylogeny
hawaii_tree <- fishtree::fishtree_phylogeny(species = new_taxon_list)

phytools::plotTree(tree = hawaii_tree, fsize = 0.2)
