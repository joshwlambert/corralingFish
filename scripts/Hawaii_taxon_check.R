install.packages("fistree")
install.packages("ape")
install.packages("maps")
install.packages("phytools")
install.packages("rfishbase")

library(fishtree)
library(ape)
library(maps)
library(phytools)
library(rfishbase)
library(dplyr)

#load in dataset
# Hawaii_NOAA_fish_data <-
#   read_csv("~/Desktop/S3/corralingfish/Hawaii_NOAA_fish_data.csv")

Hawaii_NOAA_fish_data <-
  read.csv("~/Desktop/S3/corralingfish/trimmed_NOAA_taxa.csv")

#isolate taxon list and prune to only uniqe hits
taxon_list <- Hawaii_NOAA_fish_data$Taxa
# taxon_list <- data.frame(unique(taxon_hits))
View(taxon_list)

#create a tree from molecular fishtree phylogeny
# hawaii_tree <- fishtree::fishtree_phylogeny(species = new_taxon_list)

hawaii_tree <- fishtree::fishtree_phylogeny(species = taxon_list)
write.tree(hawaii_tree, file = "molecular_hawaii_tree.nex")
full_hawaii_tree <- fishtree::fishtree_complete_phylogeny(species = taxon_list)
write.tree(full_hawaii_tree, file = "full_hawaii_tree.nex")

#visualise the trees!
phytools::plotTree(tree = hawaii_tree, fsize = 0.2)
phytools::plotTree(tree = full_hawaii_tree[[67]], fsize = 0.2, no.margin = TRUE)

#look at trimmed taxa list and count genera
trimmed_taxa_haw <- hawaii_tree[["tip.label"]]
#View(trimmed_taxa_haw)

full_trimmed_taxa_haw <- unlist(full_hawaii_tree[[44]]["tip.label"])
#View(full_trimmed_taxa_haw)

#find out which species are missing in the
lost_species <- setdiff(full_trimmed_taxa_haw, trimmed_taxa_haw)

#creat a list of taxa with more than one species per genus
#molecular_haw_NOAA_data <- subset(Hawaii_NOAA_fish_data,
                                  #Hawaii_NOAA_fish_data$Taxa %in% trimmed_taxa_haw)
molecular_haw_NOAA_data <-
  Hawaii_NOAA_fish_data[!Hawaii_NOAA_fish_data$Taxa %in% lost_species,]
droplevels(molecular_haw_NOAA_data)
#molecular_haw_NOAA_data <- levels(droplevels(molecular_haw_NOAA_data$Taxa))
all_trimmed_genus_list <- unique(molecular_haw_NOAA_data$Genera)
all_trimmed_genus_list <- as.character(all_trimmed_genus_list)
View(all_trimmed_genus_list)

#extract all species for each in genus_list

all_species_list <- list()

for( i in all_trimmed_genus_list) {
  all_species_list[[i]] <- rfishbase::species_list(Genus = i)
  }
#trim out species that are singletons in their genera, as this will make
#fishtree fail in a loop
all_trimmed_trimmed_genus_list <-
  all_trimmed_genus_list[-c (2, 3, 35, 38, 43, 52, 54, 68, 73, 77, 80)]
all_species_list <-
  all_species_list[-c (2, 3, 35, 38, 43,52, 54, 68, 73, 77, 80)]
#create phylogenetic trees for all of the species!
all_genus_trees <- list()

for (i in all_trimmed_trimmed_genus_list) {
  all_genus_trees[[i]] <-
    fishtree::fishtree_phylogeny(species = (unlist(all_species_list[[i]])))

    plotTree(all_genus_trees[[i]], ftype = "i", fsize = 0.5)
    #legend(x =5, legend = i )
  }

#filter out original dataset to get an endemics list

NOAA_endemics <- Hawaii_NOAA_fish_data %>%
  filter(Endemic_Revised == 1)

