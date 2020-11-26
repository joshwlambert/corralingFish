

library(fishtree)
library(ape)
library(phytools)
library(dplyr)
library(stringr) ## don't really know what this does yet, but was in Luis's code


####Desired output is input needed for DAISIE dataprep, as seen in
####DAISIE::data(Galapagos_datatable)
####output = df(Clade_name, Status, MissingSpecies = False, Branching Times)

#####input needed????
##### 1. complete tree (phylo object)
##### 2. df with two columns, taxa and endemicity status
##### 3. Island Max Age
##### 4. missing species = FALSE (figure out how to do this later)

#################################
######prep for me

#create full fishtree!
actino_tree <- fishtree::fishtree_phylogeny()

  #create dataframe and have data ready for missing species eventually
fish_data <- read.csv("~/Desktop/S3/corralingfish/trimmed_NOAA_taxa.csv")
taxon_list <- fish_data$Taxa

  #create DNA and full trees and respecitive species lists from that
hawaii_tree <- fishtree::fishtree_phylogeny(species = taxon_list)
trimmed_taxa_haw <- hawaii_tree[["tip.label"]]
full_hawaii_tree <- fishtree::fishtree_complete_phylogeny(species = taxon_list)
full_trimmed_taxa_haw <- unlist(full_hawaii_tree[[66]]["tip.label"])
  #separate species dropped between the two trees
lost_species <- setdiff(full_trimmed_taxa_haw, trimmed_taxa_haw)

  #create dataframe needed for the new function. Don't need to trim it, but I
  #wanted to try this to get it to work.
molecular_data <-
  fish_data[!fish_data$Taxa %in% lost_species,]
droplevels(molecular_data)
all_trimmed_genus_list <- molecular_data[-c(6:10)]

  #set Hawaii islands MaxAge
island_MaxAge <- 28.0
#million years ago (Grigg 1997; Rooney et al., 2008; Friedlander et al., 2020)

##################################

#would it be smarter to split up the dataframe into to different things,
#such as species and endemicity vectors?
######start the function
get_islandClade_ages <- function(tree, df_species_endemicty, island_MaxAge){

  #create empty dataframe for output?
  island_datatable <- data.frame(clade_name = character(0),
                                 status = character(0),
                                 brancing_times = numeric(0),
                                 stringsAsFactors = FALSE)
  #create empty lists?
df_species_status %>% group_by(endemicity)
######go through the nonendemic species first
  if(df_species_status$endemicity == 0){
    #match spcies from df_species_status to tree, to have as clade name
    #for assingment later.
    CT <- data.frame(taxa = character(0),
                     bt = numeric,
                     stringsAsFactors = FALSE)
    cbind(df_species_status$Taxa)

    #pull closest node (see luis's code) and label as CT = colonization time
    CT[,2] <-get_leaf_age(tree, CT[,1]) #this is the single species
      #code from Luis, could input this int
    #compare CT of taxa to island_MaxAge
      if(CT <= island_MaxAge){
        #append (rbind) new row with ("clade_name", nonendemic, CT)
      }
      else(CT >= island_MaxAge){
        #append (rbind) new row with ("clade_name", Non_endemic_MaxAge, island_MaxAge)
      }
  }
########start to go through the endemic species that are left!
  if else(df_species_status$Endemicity == 1){
    #for all species == 1, match species names to phylo
    #pull most recent node +/or edges from matrix from the phylo
    #create a "middle man" vector to this in
    #middleman <- vector() #in order of node age and species

#this will be starting point for endemic decision making, based on a vector
#with species_names and closest branching time for all of them, descending order
      for(i in middleman){
        #starting with youngest bt? ask if there are any other branching times
        #that match it. If yes, go through more steps to find a clade, if no,
        #output an endemic singleton.
          if(species_branchingtimes == secondary_species){
            #append two species names and times into list as a "clade"
            #remove from middle man vector
            #ask what is most recent common ancestor of clade
              #get this branching time, see if there any species with this bt
                  if(clade_bt == more_species){
                    #append species to clade
                    #remove species name from middleman
                    #re-run mrca to get new mrca and bt time.

                        then #go back another node to these species
                #ask again if there are any endemic = 1 with same branching times as this
                    if (clade_brancing == further_species)
                  #append clade to include this species
                  #find all node.leaves of this branching point
                  }
                  else (node_leaves_endemic != 1 break*) {
                      #append (rbind) new row
                      #with ("Genus**_clade", endemic, branching_string)
                  else( add_other_node_leaves == 1) #how to change it if one node leave is endemic?
                      #as a string together
                      #append (rbind) new row
                      #with ("Genus**_clade", endemic, branching_string)
                  }
              }
                      #

          if(species_brancingtimes != secondary_species){
            #append (rbind) new row with
              #("species_name", endemic, branching time)
          }


      }
  }
  else(df_species_status$missing == 1){
  }

}
}













