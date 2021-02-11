#libraries
library(dplyr)
library(fishtree)
library(tibble)

##read in dataset
fishframe <- read.csv("../datasets/full_dataset_wmissingsp_moleculartwoo.csv")
molecular_fishframe <- fishframe %>% filter(molecular_data == 1)
molecular_fishframe <- fishframe %>% filter(molecular_data == 1, depth_lower <= 150)
View(molecular_fishframe)
clade_data <- molecular_fishframe[,c(5,13)]
clades_data <- clade_data %>% remove_rownames() %>%
  column_to_rownames(var = "Taxa")
View(clades_data)


#create dataframe to estimate number of species in a Family and/or Genera
#present on Hawaii

# n_genera <- fishframe %>% arrange(Family) %>% group_by(Family) %>% count(Genera)


# create subset datasets based on the number of species in each genera
  # i.e. "this dataframe contains all Hawaiian genera with two species on the island

  #or, could begin each branch by creating df, with the conditional n_genera$n == ?


#load in tree to use: this function returns either the single molecular object,
#or a randomly selected large tree, which you select with character "molecular"
# or character "complete"

#loads in tree, based on type of tree you would like
  #should look how to develop this later for used the whole arrary of 100
  #complete trees, so that it can be used later.

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

get_tree(tree_type = "molecular")
#input into function, taxa with attached endemicitt/missing species status
#attach species to phylo tree, with data, four types:
#   1. not on hawaii
#   2. hawaii missing species   ---- > might not be nescessary if using only
#       molecular dataset, because no missing species will show up :(
#   3. Hawaii nonendemic
#   4. Hawaii endemic

#create a phylo4 object, and add in data?
names(tree)

g1_tree <- as(tree, "phylo4")
#data is loaded in as clade
View(clades_data)

#combine to create a phylo4d object. by setting missing data to "warn" should set
#any species not in the dataset e.g. #!1 to have an NA values

g2 <- phylo4d(g1_tree, clades_data, missing.data = "warn")

# this just gets ride of all other species in the phylo object using phylobase
g3 <- match.phylo.data(tree, clades_data)

#trying a different phylo4d method

g4 <- phylo4d(tree, tip.data =clade_data)

which(g2@label == "Abudefduf_abdominalis")
g2@label[2152]
g2@data["2152",]


#finds shortest path between two taxa
shortestPath(g2, "Abudefduf_abdominalis", "Abudefduf_sordidus")

#find out if a species has siblings
siblings(g2, 2152, include.self = TRUE)

#find out ancestors
ancestor(g2,2152)

ancestors(g2, 2152,type = "all")


#find children of a single node (internal!)
children(g2,13823)

#will find children of an internal node, and and return all new tips
descendants(g2,13823)
descendants(g2, 13822)
descendants(g2, 13821)
descendants(g2,13820)


