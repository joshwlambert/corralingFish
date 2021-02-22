#libraries
library(dplyr)
library(fishtree)
library(tibble)
library(phylobase)
library(tidyverse)
library(stringr)

#######---------read in dataset and filter for molecular data and depth-----####
fishframe <- read.csv("../datasets/full_dataset_wmissingsp_moleculartwoo.csv")
molecular_fishframe <- fishframe %>% filter(molecular_data == 1)
molecular_fishframe <- fishframe %>% filter(molecular_data == 1, depth_lower <= 150)
View(molecular_fishframe)
clade_data <- molecular_fishframe[,c(5,13)]
clades_data <- clade_data %>% remove_rownames() %>%
  column_to_rownames(var = "Taxa")
View(clades_data)




cladeR <- function(df_clades, treetype, island_age){
#######----------pull phylogeny you want to use -------##########
#assign outside of the function to not have to use global assignment
#options are either "molecular" or "complete"
get_tree <- function(tree_type){
  if(tree_type == "molecular"){
    tree <- fishtree::fishtree_phylogeny()
    print("tree")
    return(tree)
  }
  if(tree_type == "complete"){
    trees <- fishtree::fishtree_complete_phylogeny()
    single_tree <- trees[[sample(1:100,1)]]
    print("single_tree")
    return(single_tree)
  }
}

tree <- get_tree(tree_type = treetype)
###--------Create combined tree object-----####
g1_tree <- as(tree, "phylo4")
#combine to create a phylo4d object. by setting missing data to "warn" should set
#any species not in the dataset e.g. #!1 to have an NA values
phylo <- phylo4d(g1_tree, df_clades, missing.data = "warn")

#######-----------write general function to extract branching times ----#####
get_leaf <- function(phy4object, leaf_name) {
  the_node <- which(phy4object@label == leaf_name)
  return(unname(edgeLength(phy4object)[getEdge(phy4object, the_node)]))
}
##########-------running extraction functions for non endemics/endemics----####
source("../corralingFish/scripts/extracting_endemics.R")
source("../corralingFish/scripts/extracting_nonendemics.R")
  output_nonendemics <- get_nonendemics(phy4object = phylo, island_max_Age = island_age)
  output_endemics <- get_endemics(phy4object = phylo , island_max_Age = islange_age)
#######-------merge dataframes to create final full dataframe-------########
island_df <- full_join(output_nonendemics, output_endemics, by = c("Taxa", "branching_times", "status"))
  Missing_species <- c(rep(0, length(island_df$Ta)))
island_df <- tibble::add_column(island_df, Missing_species = rep(0, length(island_df$Taxa)), .before = "status")

print("dataframe output is in DAISIE input format")
return(island_df)
}


output <- cladeR(clades_data, "molecular", 28)
