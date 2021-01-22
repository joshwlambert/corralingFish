#libraries
library(dplyr)
library(fishtree)


##read in dataset
fishframe <- read.csv("./datasets/depth_trimmed_taxa_UPDATED.csv")

#create dataframe to estimate number of species in a Family and/or Genera
#present on Hawaii

n_genera <- fishframe %>% arrange(Family) %>% group_by(Family) %>% count(Genera)


# create subset datasets based on the number of species in each genera
  # i.e. "this dataframe contains all Hawaiian genera with two species on the island

  #or, could begin each branch by creating df, with the conditional n_genera$n == ?


#load in tree to use: this function returns either the single molecular object,
#or a randomly selected large tree, which you select with character "molecular"
# or character "complete"

#I don't think this works properly
get_tree <- function(x){
  if(x == "molecular"){
    tree <- fishtree::fishtree_phylogeny()
    print("use molecular data only")
  }
  if(x == "complete"){
    tree <- fishtree::fishtree_complete_phylogeny()
    single_tree <- tree[[sample(1:100,1)]]
    print("use all available data")
  }
}
get_tree( x = "molecular")

