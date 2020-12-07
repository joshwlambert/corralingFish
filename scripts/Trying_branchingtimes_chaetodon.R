#rm(list = ls())


#all of the genus tre
library(ape)
library(phytools)
library(stringr)
library(fishtree)

#load in the datasheet!
Hawaii_NOAA_fish_data <-
  read.csv("~/Desktop/S3/corralingfish/trimmed_NOAA_taxa.csv")

trimmed_taxa_haw <- hawaii_tree[["tip.label"]]

#create the trees need to make luis's thing work?
#look at all of Chaetodontidae order in order to focus on Chaetodon genus

chaeto_tree <- fishtree::fishtree_phylogeny(rank = "Chaetodontiformes")
write.tree(chaeto_tree, file = "cheato_molecular_tree.nex")


#doing this will allow for a sister taxa?? Though I don't know which on to choose.
#looks like the true sister taxa isn't present on hawaii??? does that matter???

#working only with DNA trees right now. Luis has a loop set up, but because I am
#only using the fishtree_molecular trees that don't have any posterior
#distribution, I can change it to just be to go through one tree.

## EXAMPLE 2 - Extract stem age and branching times of entire clade

ingroup <- str_subset(chaeto_tree$tip.label,'Chaetodon*')
## for one species from sister clade, used one from sister clade as seen on the
##Chaetodontidae tree, but it isn't found on Hawaii. Don't know if that was
##correct or not.
one_species_from_sister_clade <- 'Prognathodes_aya'
wanted_tips <- c(ingroup,one_species_from_sister_clade)
subclade <- keep.tip(chaeto_tree, wanted_tips)
btimes <- as.vector(branching.times(subclade))
btimes_cheatodon <- sort(btimes,decreasing = T)

Chaeto_branches <- chaeto_tree$tip.label
Chaeto_branches <- cbind(Chaeto_branches, btimes)
#need to find a way to incorporate which are on the island and which are endemic

######lets try to make it for everybody!
######
taxon_list <- Hawaii_NOAA_fish_data$Taxa
hawaii_tree <- fishtree::fishtree_phylogeny(species = taxon_list)
trimmed_taxa_haw <- hawaii_tree[["tip.label"]]

full_hawaii_tree <- fishtree::fishtree_complete_phylogeny(species = taxon_list)
full_trimmed_taxa_haw <- unlist(full_hawaii_tree[[44]]["tip.label"])
lost_species <- setdiff(full_trimmed_taxa_haw, trimmed_taxa_haw)
molecular_haw_NOAA_data <-
  Hawaii_NOAA_fish_data[!Hawaii_NOAA_fish_data$Taxa %in% lost_species,]
droplevels(molecular_haw_NOAA_data)
order_haw <- molecular_haw_NOAA_data$Order
order_list <- as.factor(unique(order_haw))


######lets try to make a loop? in order to get trees for all of these orders

order_trees  <- list()

for (i in order_list){
  order_trees[[i]] <- fishtree::fishtree_phylogeny(rank = i)

}
