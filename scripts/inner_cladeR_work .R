
#loading in beginning cladeR stuff
View(g2) # the tree with attached data
View(clades_data) #loaded taxa with attached endemicity status
island_max_Age <- 28.000000
View( island_max_Age)

#find taxa that are labelled as endemic in data
end_nodes <-
  unname(g2@label[rownames(g2@data)[which(g2@data == 1)]])

#####-----------create ids to help seperate internal and terminal nodes, with empty vectors--------####
  #for the loop
tip_ids <- nodeId(g2, type = "tip")
tipdf <- tipData(g2)
checking_taxa <- c()
taxa_id <- c()
  #for loop to find sibling taxa for all endemics, and seperate vectors that checks
  #if siblings are a tip or not. Internal nodes == FALSE, terminal nodes == TRUE
for(i in 1:length(end_nodes)){
 checking_taxa[i] <- siblings(g2, end_nodes[i])
  taxa_id[i] <- checking_taxa[i] %in% tip_ids #is the id a tip?
}
############ ------------changing labels of TRUE/FALSE to tip/internal, creating dataframe of vectors,----#####
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
#### dont have a function for mixed nodes i.e.(polytomy?)
#### ---------------------------------- function for internal nodes ------ #########
#for loop creates two lists,
inner_child <- list ()
inner_all <- list()
inner <- list ()
for(i in 1:nrow(internal_nodes)){
    #grab the immediate descendants of internal nodes that are siblings of endemic species
  inner_child[[internal_nodes[i,1]]] <- descendants(g2, internal_nodes[i,2] , type = "children") #using all produces more descendants
    #check and endemics that only have internal node siblings. If this is the case
    # create sublist that runs descendants all to get the next level of descendants
      if(all(is.na(names(inner_child[[i]][])))){
          print(i)
           inner_all[[i]] <- descendants(g2, internal_nodes[i,2], type = "all")
             #if there are values, create an if statement to overwrite above list value
            #do this outside of the for loop?????
          if(!is.null(inner_all[[i]])){
            inner_child[[i]] <- inner_all[[i]]
           }}
  inner[[internal_nodes[i,1]]] <- tipdf[names(inner_child[[i]]),] #internal nodes still go through this and show up as NA
}

#second part, where clade names are put into strings, branching times computed
#and then they are inputed into a dataframe.
for(m in 1:length(inner)){
  internal_endemics_df <- data.frame(matrix(ncol = 2, nrow = 0, dimnames = NULL))
  colnames(internal_endemics_df) <- c("taxa", "brancing_time")
 if(all(!is.na(inner[[m]])) && all(inner[[m]] != 0)){
   tester <- c()
   bt <- c()
     #collapse siblings' children to a single string
     tester <- stringr::str_c(names(inner_child[[m]]), collapse = ", ")
   #add endemic species to create a single clade character string
     clade_str <- stringr::str_c(names(inner_child[m]) ,tester, sep = ", ")
     #create a vector of whole clade, to use to find branching times.
     clade_vec <- unlist(strsplit(clade_str, split = ", "))
       #for length of clade_vec for each interation i, do k to extract branching times
     branchings <- c()
     for(k in 1:length(clade_vec)){
            branchings[k] <- as.character(get_leaf(g2, g2@label[which(g2@label == clade_vec[k])]))
            bt <- stringr::str_c(branchings, collapse = ", ")
          }
    #create a new row to add into dataframe for
    df_newrow <- data.frame(taxa = clade_str ,brancing_times = bt, stringsAsFactors = FALSE)
  internal_endemics_df <- rbind(internal_endemics_df, df_newrow)
   }
if(any(is.na(inner[[m]])) | any(inner[[m]] == 0)){
  sbt <- c()
  inner_single[m] <- names(inner_child[m]) #check????()
  for(v in 1:length(inner_single)){
  sbt[v] <- as.character(get_leaf(g2, g2@label[which(g2@label == inner_single[v])]))
  df_newrows <- data.frame(taxa = inner_single[v], brancing_times = sbt[v], stringsAsFactors = FALSE)
  internal_endemics_df <- rbind(internal_endemics_df, df_newrows)
}
}
  }
#########----------------function for terminal nodes (tips)----##############
tips <- list()
for(p in 1:nrow(tip_nodes)){
  tip_endemics_df <- data.frame(matrix(ncol = 2, nrow = 0, dimnames = NULL))
  colnames(tip_endemics_df) <- c("taxa", "brancing_time")
  tips[[tip_nodes[p,1]]] <- as.character(tipdf[tip_nodes[p,2],]) #internal nodes still go through this and show up as NA
  for(q in 1:length(tips[])){
     #print(tips[[q]])
     tbt <- c()
    tclade <- c()
     tnames <- c()
     tbts <- c()
     ttnames <- c()
     ttbts <- c()
     #onisle <- c()
    if(!is.na(tips[q])){
      #onisle <- tips[[q]]
       if(tips[[q]] == 1){
     tclade[q] <- stringr::str_c(tip_nodes[q,1], unname(g2@label[tip_nodes[q,2]]), sep = ", ")
     tbt[q] <- as.character(get_leaf(g2, tip_nodes[q,1]))
     dft_newrow <- data.frame(taxa = tclade[q], brancing_times = tbt[q], stringsAsFactors = FALSE)
      tip_endemics_df <- rbind(tip_endemics_df, dft_newrow)
       }
      if(tips[[q]] != 1){
         ttnames[q] <- tip_nodes[q,1]
         ttbts[q] <- as.character(get_leaf(g2, tip_nodes[q,1]))
         dftt_newrows <- data.frame(taxa = ttnames[q], brancing_times = ttbts[q], stringsAsFactors = FALSE)
         tip_endemics_df <- rbind(tip_endemics_df, dftt_newrows)
       }
    }
    if(is.na(tips[[q]])){
      tnames[q] <- tip_nodes[q,1]
      tbts[q] <- as.character(get_leaf(g2, tip_nodes[q,1]))
     dft_newrows <- data.frame(taxa = tnames[q], brancing_times = tbts[q], stringsAsFactors = FALSE)
     tip_endemics_df <- rbind(tip_endemics_df, dft_newrows)
      }
   }
  }
########-------combing internal and tip dataframes-----------##############
endemic_df <- rbind(internal_endemics_df, tip_endemics_df)
######-------adding island Age and final column lables---------##############
#endemic_df$brancing_times[endemic_df$brancing_times
                            # >= island_max_Age] <- island_max_Age

#add new status column based on branching times
endemic_dfs <- endemic_df %>% mutate(status = case_when(
  brancing_times == 28 ~ "Endemic_MaxAge",
  TRUE ~ "Endemic"))

# #change dataframe to factors to match galapagos datatable from DAISIE
 endemic_dfs$status <- as.factor(endemic_dfs$status)

