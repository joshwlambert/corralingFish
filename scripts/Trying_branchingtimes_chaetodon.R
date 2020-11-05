#rm(list = ls())


#all of the genus tre
library(ape)
library(phytools)
library(stringr)
library(fishtree)

#create the trees need to make luis's thing work?
#look at all of Chaetodontidae order in order to focus on Chaetodon genus

chaeto_tree <- fishtree::fishtree_phylogeny(rank = "Chaetodontidae")
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
