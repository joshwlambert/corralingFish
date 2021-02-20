library(ape)
library(dplyr)
library(fishtree)
library(data.table)
library(phylobase)
#read in dataset
fishdataset <- read.csv("./datasets/depth_trimmed_taxa_UPDATED.csv")
fishframe <- fishdataset %>% filter(molecular_data == 1)


############# funnel down branches based on #species present/absent on islands
####genera with one species == singletons?

#pull from Root_cladefindr_stuff, as this will be beginning outer shell work
#will establish how many genera present/absent on hawaii

#Create df that isolates Hawaiian Genera with 1 island represenative
single <- n_genera %>% filter(n == 1)

#Turn singleton Genera into Charactor String
singletons_by_genera <- as.factor(single$Genera)

#create subset dataset to use to extract branching times
df_single <- data.frame()
for(i in singletons_by_genera){

  df_middle <- data.frame(fishframe[grep(i, fishframe$Genera),])

  Family <- as.character(df_middle$Family)
  Genera <- as.character(i)
  Taxon <- as.character(df_middle$Taxa)
  endemicity <- as.numeric(df_middle$Endemic_Revised)
  molecular <- as.numeric(df_middle$molecular_data)
  ##missing_sp <- as.numeric(df_middle$missing_sp)

  df_newrow <- data.frame(Family = Family, Genera = Genera, Taxon = Taxon,
                          endemicity = endemicity, molecular = molecular )
  df_single <- rbind(df_single, df_newrow)
}

#extract branching times for species in inputed dataset and build onto new dataset

######recycle Luis's "get_leaf_age function"
## function to extract tip ages for single taxa
get_leaf <- function(phy4object, leaf_name) {
  the_node <- which(phy4object@label == leaf_name)
  return(edgeLength(phy4object)[getEdge(phy4object, the_node)])
  }

leaf <- list()
for(i in df_single$Taxon){
  leaf[[i]] <- get_leaf(tree, i)

}

#######or review options such as ape::getMRCA // ape::mrca functions

#NOTE## ape::mrca -> "returns for each pair of tips (and nodes) its most recent
####### common ancestor for a phylo object. if function part full = TRUE, however,
####### will retrun mrca values for nodes as well. Provides a mode matrix

#NOTE## ape::getMRCA -> "returns the MRCA of two or more tips": however the
####### "tip  = " can alse be node numbers. provides a single numeric value
  #good for two or more

#create final definitive singletons dataset

df_single_branch <- data.frame(unlist(leaf), stringsAsFactors = FALSE)
setDT(df_single_branch, keep.rownames = TRUE)[]
colnames(df_single_branch) <- c("Taxon", "branching_time")

singleton_df <- merge(df_single, df_single_branch)


#Use Singleton dataset to pull from and input into final dataset?

#or this could be another chunk, where you input datasets at the end.







single
