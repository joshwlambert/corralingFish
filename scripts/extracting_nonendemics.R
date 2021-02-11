library(fishtree)
library(dplyr)
library(phylobase)

#loaded in objects from previous basic cladedindr stuff
View(g2)
View(clades_data)
island_max_Age <- 28
View( island_max_Age)

get_leaf <- function(phy4object, leaf_name) {
  the_node <- which(phy4object@label == leaf_name)
  return(unname(edgeLength(phy4object)[getEdge(phy4object, the_node)]))
}

get_nonendemics <- function(phy4object, island_max_Age){
#extract nodes from attached data, where endemicity = 0
nonend_nodes <-
  unname(phy4object@label[rownames(phy4object@data)[which(phy4object@data == 0)]])

#extract branching times for these names
leaf <- c()
for(i in 1:length(nonend_nodes)){
  leaf[i] <- get_leaf(phy4object, nonend_nodes[i])
}

#convert results into a dataframe
df_nonendemic <- data.frame(nonend_nodes, leaf, stringsAsFactors = FALSE)
#setDT(df_single_branch, keep.rownames = TRUE)[]
#df_nonendemic <- tibble::rownames_to_column(df_nonendemic)
colnames(df_nonendemic) <- c("Taxa" ,"branching_time")

#starting to incorporate island_MAxage, into branching times
df_nonendemic$branching_time[df_nonendemic$branching_time
                             >= island_max_Age] <- island_max_Age

#add new status column based on branching times
df_nonendemic <- df_nonendemic %>% mutate(status = case_when(
  branching_time == 28 ~ "Non_endemic_MaxAge",
  TRUE ~ "Non_endemic"))

#change dataframe to factors to match galapagos datatable from DAISIE
df_nonendemic$Taxa<- as.factor(df_nonendemic$Taxa)
df_nonendemic$status <- as.factor(df_nonendemic$status)
df_nonendemic$branching_time <- as.factor(df_nonendemic$branching_time)

#have the print/return show what the dataframe is!
print("dataframe is called df_nonendemics")
return(df_nonendemic)
}
