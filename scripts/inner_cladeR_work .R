
#loading in beginning cladeR stuff
View(g2)
View(clades_data)
island_max_Age <- 28.000000
View( island_max_Age)


endemics_df <- data.frame() #create an empty dataframe with same structure as
colnames(endemics_df) <- c("Taxa", "brancing_time")

#find taxa that are labelled as endemic in data
end_nodes <-
  unname(g2@label[rownames(g2@data)[which(g2@data == 1)]])
      #tipdf <- tipData(g2)

#create ids to help seperate internal and terminal nodes, with empty vectors
  #for the loop
tip_ids <- nodeId(g2, type = "tip")
tipdf <- tipData(g2)
checking_taxa <- c()
taxa_id <- c()
  #for loop to find sibling taxa for all endemics, and seperate vectors that checks
  #if siblings are a tip or not. Internal nodes == FALSE, terminal nodes == TRUE
for(i in 1:length(end_nodes)){
 checking_taxa[i] <- siblings(g2, end_nodes[i])
  taxa_id[i] <- checking_taxa[i] %in% tip_ids
}

#changing labels of TRUE/FALSE to tip/internal, creating dataframe of vectors,
  #with orginal endemics as rownames
taxa_id <- gsub(TRUE, "tip", taxa_id)
taxa_id <- gsub(FALSE, "internal", taxa_id)
sibling_taxa <- cbind(checking_taxa, taxa_id)
rownames(sibling_taxa) <- end_nodes
colnames(sibling_taxa) <- c("sibling_nodes", "node_state")
sibling_taxa <- data.frame(sibling_taxa, stringsAsFactors = FALSE)
#separate into two dataframes based on internal vs. tip, to be put into
  #two separate but processes. perhaps this should be worked on in two
  #separate other scripts?
internal_nodes <- sibling_taxa %>% rownames_to_column("endemic_taxa")%>%
    filter(node_state == "internal")
internal_nodes$sibling_nodes <- as.integer(internal_nodes$sibling_nodes)
tip_nodes <- sibling_taxa %>% rownames_to_column("endemic_taxa") %>%
    filter(node_state == "tip")
tip_nodes$sibling_nodes <- as.integer(tip_nodes$sibling_nodes)


#for loop creates two lists,
inner <- list()
tips <- list ()
#full_list <- list()
for(i in 1:nrow(internal_nodes)){
  inner[[internal_nodes[i,1]]] <- descendants(g2, internal_nodes[i,2], type = "all") #using all produces more descendants
  tips[[internal_nodes[i,1]]] <- tipdf[names(inner[[i]]),] #internal nodes still go through this and show up as NA
  }
# full_list <- list(inner)
# full_list <- append(full_list, list(tips))

#do things for the internal nodes
inner <- list()
tips <- list ()
for(i in length(1:interal_nodes$sibling_nodes)){
inner[[i]] <- descendants(g2, internal_nodes[i,2])
tips[[i]] <- tipdf[names(inner),]
if(all(!is.na(tips)) && all(tips != 0)){
  clade <- c()
  for(j in 1:length(inner)){
  clade[j] <- stringr::str_c(intenal_nodes[i,1] , names(inner[j]), sep = " , ") ###### this needs to be fixed
  bt <- get_leaf(phy4object = g2, leaf_name = j)                                ###### this needs to be fixed

  df_newrow <- data.frame(Taxa = clade, branching_time = bt)
  endemics_df <- rbind(endemics_df, df_newrow)
  #;
  # double_check <- descendants(g2, j, type = "tips")
  # # should look at what the other options entail, maybe that's a better option
  #   if(is.list(double_check)== TRUE){
  #       triple <- unlist(double_check)
  #      if(all!is.na(tips) && all(tips != 0))
  #       for(k in triple){
  #         claded <- stringr::str_c(i, j, k,  sep = " , ")
  #         btd <- get_leaf(phy4object = g2, leaf_name = k) #should this be a list? will only return one branching time
  #
  #         dfd_newrow <- data.frame(Taxa = claded, branching_time = btd)
  #         endemics_df <- rbind(endemics_df, dfd_newrow)
  #         }
  #       }
      }
    }
  }

