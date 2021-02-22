
#loading in beginning cladeR stuff
# View(phy4object) # the tree with attached data
# View(clades_data) #loaded Taxa with attached endemicity status
# island_max_Age <- 28.000000
# View( island_max_Age)

get_endemics <- function(phy4object, island_max_Age){
  #find Taxa that are labelled as endemic in data
  end_nodes <-
    unname(phy4object@label[rownames(phy4object@data)[which(phy4object@data == 1)]])

  #####-----------create ids to help seperate internal and terminal nodes, with empty vectors--------####
  #for the loop
  tip_ids <- nodeId(phy4object, type = "tip")
  tipdf <- tipData(phy4object)
  checking_Taxa <- c()
  Taxa_id <- c()
  #for loop to find sibling Taxa for all endemics, and seperate vectors that checks
  #if siblings are a tip or not. Internal nodes == FALSE, terminal nodes == TRUE
  for(i in 1:length(end_nodes)){
    checking_Taxa[i] <- siblings(phy4object, end_nodes[i])
    Taxa_id[i] <- checking_Taxa[i] %in% tip_ids #is the id a tip?
  }
  ############ ------------changing labels of TRUE/FALSE to tip/internal, creating dataframe of vectors,----#####
  #with orginal endemics as rownames
  Taxa_id <- gsub(TRUE, "tip", Taxa_id)
  Taxa_id <- gsub(FALSE, "internal", Taxa_id)
  sibling_Taxa <- cbind(checking_Taxa, Taxa_id)
  rownames(sibling_Taxa) <- end_nodes
  colnames(sibling_Taxa) <- c("sibling_nodes", "node_state")
  sibling_Taxa <- data.frame(sibling_Taxa, stringsAsFactors = FALSE)
  #separate into two dataframes based on internal vs. tip, to be put into
  #two separate but processes. perhaps this should be worked on in two
  #separate other scripts?
  internal_nodes <- sibling_Taxa %>% rownames_to_column("endemic_Taxa")%>%
    filter(node_state == "internal")
  internal_nodes$sibling_nodes <- as.integer(internal_nodes$sibling_nodes)
  tip_nodes <- sibling_Taxa %>% rownames_to_column("endemic_Taxa") %>%
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
    inner_child[[internal_nodes[i,1]]] <- descendants(phy4object, internal_nodes[i,2] , type = "children") #using all produces more descendants
    #check and endemics that only have internal node siblings. If this is the case
    # create sublist that runs descendants all to get the next level of descendants
    if(all(is.na(names(inner_child[[i]][])))){
      #print(i)
      inner_all[[i]] <- descendants(phy4object, internal_nodes[i,2], type = "all")
      #if there are values, create an if statement to overwrite above list value
      #do this outside of the for loop?????
      if(!is.null(inner_all[[i]])){
        inner_child[[i]] <- inner_all[[i]]
      }}
    inner[[internal_nodes[i,1]]] <- tipdf[names(inner_child[[i]]),] #internal nodes still go through this and show up as NA
  }

  ###--------second part, where clade names are put into strings, branching times computed --------########
  #and then they are inputed into a dataframe.
  internal_endemics_df <- data.frame(matrix(ncol = 2, nrow = 0, dimnames = NULL))
  colnames(internal_endemics_df) <- c("Taxa", "branching_time")
  inner_single <- c()
  sbt <- c()
  tester <- c()
  bt <- c()

  for(m in 1:length(inner)){
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
        branchings[k] <- as.character(get_leaf(phy4object, phy4object@label[which(phy4object@label == clade_vec[k])]))
        bt <- stringr::str_c(branchings, collapse = ", ")
      }
      #create a new row to add into dataframe for
      df_newrow <- data.frame(Taxa = clade_str , branching_times = bt, stringsAsFactors = FALSE)
      internal_endemics_df <- rbind(internal_endemics_df, df_newrow)
    }
    if(any(is.na(inner[[m]]))){
      inner_single[m] <- names(inner[m])
      for(v in 1:length(inner_single)){
        sbt[v] <- as.character(get_leaf(phy4object, phy4object@label[which(phy4object@label == inner_single[v])]))
        # df_newrows <- data.frame(Taxa = inner_single[v], branching_times = sbt[v], stringsAsFactors = FALSE)
        # internal_endemics_df <- rbind(internal_endemics_df, df_newrows)
      }
      df_newrows <- data.frame(Taxa = inner_single[m], branching_times = sbt[m], stringsAsFactors = FALSE)
      internal_endemics_df <- rbind(internal_endemics_df, df_newrows)
    }
  }
  #########----------------function for terminal nodes (tips)----##############
  tips <- list()
  tip_endemics_df <- data.frame(matrix(ncol = 2, nrow = 0, dimnames = NULL))
  colnames(tip_endemics_df) <- c("Taxa", "branching_times")
  tbt <- c()
  tclade <- c()
  tnames <- c()
  tbts <- c()
  ttnames <- c()
  ttbts <- c()
  for(p in 1:nrow(tip_nodes)){
    tips[[tip_nodes[p,1]]] <- as.character(tipdf[tip_nodes[p,2],]) #internal nodes still go through this and show up as NA
    for(q in 1:length(tips[])){
      if(!is.na(tips[q])){
        if(tips[[q]] == 1){
          tclade[q] <- stringr::str_c(tip_nodes[q,1], unname(phy4object@label[tip_nodes[q,2]]), sep = ", ")
          tclade <- as.character(na.omit(tclade))
          tbt[q] <- get_leaf(phy4object, tip_nodes[q,1])
          tbt <- as.character(na.omit(tbt))
          #  for(x in 1:length(tbt)){
          # dft_newrow <- data.frame(Taxa = tclade[x], branching_times = tbt[x], stringsAsFactors = FALSE)
          #  tip_endemics_df <- rbind(tip_endemics_df, dft_newrow)}
        }
        if(tips[[q]] != 1){
          ttnames[q] <- tip_nodes[q,1]
          ttnames <- unique(as.character(na.omit(ttnames)))
          ttbts[q] <- get_leaf(phy4object, tip_nodes[q,1])
          ttbts <- unique(as.character(na.omit(ttbts)))
          # for(y in 1:length(ttbts)){
          # dftt_newrows <- data.frame(Taxa = ttnames[y], branching_times = ttbts[y], stringsAsFactors = FALSE)
          # tip_endemics_df <- rbind(tip_endemics_df, dftt_newrows)}
        }
      }
      if(is.na(tips[[q]])){
        tnames[q] <- tip_nodes[q,1]
        tnames <- unique(as.character(na.omit(tnames)))
        tbts[q] <- get_leaf(phy4object, tip_nodes[q,1])
        tbts <- unique(as.character(na.omit(tbts)))
        #  for(z in 1:length(tbts)){
        # dft_newrows <- data.frame(Taxa = tnames[z], branching_times = tbts[z], stringsAsFactors = FALSE)
        # tip_endemics_df <- rbind(tip_endemics_df, dft_newrows)}
      }
    }}
  for(x in 1:length(tbt)){
    dft_newrow <- data.frame(Taxa = tclade[x], branching_times = tbt[x], stringsAsFactors = FALSE)
    tip_endemics_df <- rbind(tip_endemics_df, dft_newrow)}
  for(y in 1:length(ttbts)){
    dftt_newrows <- data.frame(Taxa = ttnames[y], branching_times = ttbts[y], stringsAsFactors = FALSE)
    tip_endemics_df <- rbind(tip_endemics_df, dftt_newrows)}
  for(z in 1:length(tbts)){
    dft_newrows <- data.frame(Taxa = tnames[z], branching_times = tbts[z], stringsAsFactors = FALSE)
    tip_endemics_df <- rbind(tip_endemics_df, dft_newrows)}

  ########-------combing internal and tip dataframes-----------##############
  df_endemics <- full_join(internal_endemics_df, tip_endemics_df, by = c("Taxa", "branching_times"))
  ######-------adding island Age and final column lables---------##############
  #df_endemics$branching_times[df_endemics$branching_times
  # >= island_max_Age] <- island_max_Age

  #add new status column based on branching times
  df_endemics <- df_endemics %>% mutate(status = case_when(
    branching_times == 28 ~ "Endemic_MaxAge",
    TRUE ~ "Endemic"))

  # #change dataframe to factors to match galapagos datatable from DAISIE
  df_endemics$status <- as.factor(df_endemics$status)

  #end of times
  print("dataframe is called df_endemics")
  return(df_endemics)
}
