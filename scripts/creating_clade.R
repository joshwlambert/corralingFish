library(ape)
library(dplyr)
library(fishtree)
library(data.table)



#using the previously loaded fishdataset and fishframe


#funnel down branches with species present absent on islands
#####genera with two species === either sinlgeton/ a small clade

#seper
double <- n_genera %>% filter(n == 2)
double_by_genera <- as.factor(double$Genera)

df_double <- data.frame()
for(i in double_by_genera){

  df_middle <- data.frame(fishframe[grep(i, fishframe$Genera),])

  Family <- as.character(df_middle$Family)
  Genera <- as.character(i)
  Taxon <- as.character(df_middle$Taxa)
  endemicity <- as.numeric(df_middle$Endemic_Revised)
  molecular <- as.numeric(df_middle$molecular_data)
  ##missing_sp <- as.numeric(df_middle$missing_sp)

  df_newrow <- data.frame(Family = Family, Genera = Genera, Taxon = Taxon,
                          endemicity = endemicity, molecular = molecular )
  df_double <- rbind(df_double, df_newrow)
}

#use get leaf to get the closest leaf age for these
leaf_duo <- list()
for(i in df_double$Taxon){
  leaf_duo[[i]] <- get_leaf_age(tree, i)
}

df_double_branch <- data.frame(unlist(leaf_duo), stringsAsFactors = FALSE)
setDT(df_double_branch, keep.rownames = TRUE)[]
colnames(df_double_branch) <- c("Taxon", "branching_time")

doublton_df <- merge(df_double, df_double_branch)


#now that we have these times, we can find shared branching times
shared <- doublton_df %>% group_by(branching_time) %>% filter(n() >1)

unique_bt <- unique(shared$branching_time)

clade <- data.frame()
for(i in unique_bt){
  shared_taxa <- as.character(shared %>% filter(branching_time == i) %>% pull(Taxon))
  clade_er <- str_c(shared_taxa, collapse = ", ")
  bt <- as.numeric(i)
  # Family <- as.character(shared$Family)
  # Genera <-  as.character(shared$Genera)
  # endemicity <-  as.numeric(shared$endemicity)
  # molecular <- as.numeric(shared$molecular)
#create new small dataframe of n == 2 brancing times with new "clades

dt_newrow <- data.frame(Taxon = clade_er,
                        branching_time = bt)
clade <- rbind(clade, dt_newrow)
}

trying <- data.frame()
trying <- df_double_branch[!(df_double_branch$Taxon %in% shared$Taxon),]
trying <- rbind(trying, clade)






doubles <- doublton_df[!(doublton_df$Taxon %in% shared$Taxon),]
