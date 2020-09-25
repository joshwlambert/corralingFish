#plot a tree with a binary trait at the tips to represent reef associate

################3load libraries
library(fishtree)
library(rfishbase)
library(dplyr)
library(ape)
library(maps)
library(phytools)

##################load data
reef_fish_data <- read.csv("../GalapagosFishes_AA (2).csv")
species_count <- read.csv("../counts_of_species_in_families.csv")

##################filter species count data for families that have more >= 5 representatives (disregard the grand total)
rep_species_count <- species_count %>%
  filter(Count.of.Family >= 10) %>%
  arrange(desc(Count.of.Family))

#################extract all species from fishbase, picked families of species over ten

family_list <- c("Serranidae", "Carangidae", "Muraenidae", "Labridae", "Exocoetidae", "Scorpaenidae", "Haemulidae", "Pomacentridae",
                  "Tetraodontidae", "Ophichthidae", "Scombridae")

all_fam <- list()
for(i in family_list) {
all_fam[[i]] <- rfishbase::species_list(Family = i)
}

#########extract species from our reef data
##I couldn't figure out how to do this so I gave up, hopefully I can try again later to figure it out?
# gala_reef_data <- ls()
# gala_reef <- ls()
#
# for(i in family_list){
#   gala_reef_data[[i]] <- reef_fish_data[which(reef_fish_data$Family == i,]
#   gala_reef[[i]] <- gala_reef_data[[i]]$species_SFTEP
# }
#
# gala_reef[[i]] <- gala_reef_data[[i]]$species_SFTEP


#serranidae
serran_data <- reef_fish_data[which(reef_fish_data$Family == "Serranidae"), ]
reef_serran_data <- serran_data[which(serran_data$Reef_associated == 1 ),]
reef_serran <- reef_serran_data$species_SFTEP

#carangidae
caran_data <- reef_fish_data[which(reef_fish_data$Family == "Carangidae"), ]
reef_caran_data <- caran_data[which(caran_data$Reef_associated == 1 ),]
reef_caran <- reef_caran_data$species_SFTEP

#muraenidae
mura_data <- reef_fish_data[which(reef_fish_data$Family == "Muraenidae"), ]
reef_mura_data <- mura_data[which(mura_data$Reef_associated == 1 ),]
reef_mura <- reef_mura_data$species_SFTEP

#labridae
labri_data <- reef_fish_data[which(reef_fish_data$Family == "Labridae"), ]
reef_labri_data <- labri_data[which(labri_data$Reef_associated == 1 ),]
reef_labri <- reef_labri_data$species_SFTEP

#exocoetidae
exo_data <- reef_fish_data[which(reef_fish_data$Family == "Exocoetidae"), ]
reef_exo_data <- exo_data[which(exo_data$Reef_associated == 1 ),]
reef_exo <- reef_exo_data$species_SFTEP

#scorpaenidae
scorp_data <- reef_fish_data[which(reef_fish_data$Family == "Scorpaenidae"), ]
reef_scorp_data <- scorp_data[which(scorp_data$Reef_associated == 1 ),]
reef_scorp <- reef_scorp_data$species_SFTEP

#Haemulidae
haem_data <- reef_fish_data[which(reef_fish_data$Family == "Haemulidae"), ]
reef_haem_data <- haem_data[which(haem_data$Reef_associated == 1 ),]
reef_haem <- reef_haem_data$species_SFTEP

#Pomacentridae
poma_data <- reef_fish_data[which(reef_fish_data$Family == "Pomacentridae"), ]
reef_poma_data <- poma_data[which(poma_data$Reef_associated == 1 ),]
reef_poma <- reef_poma_data$species_SFTEP

#Tetraodontidae
tetra_data <- reef_fish_data[which(reef_fish_data$Family == "Tetraodontidae"), ]
reef_tetra_data <- tetra_data[which(tetra_data$Reef_associated == 1 ),]
reef_tetra <- reef_tetra_data$species_SFTEP

#ophichthidae
ophich_data <- reef_fish_data[which(reef_fish_data$Family == "Ophichthidae"), ]
reef_ophich_data <- ophich_data[which(ophich_data$Reef_associated == 1 ),]
reef_ophich <- reef_ophich_data$species_SFTEP

#scombridae
scom_data <- reef_fish_data[which(reef_fish_data$Family == "Scombridae"), ]
reef_scom_data <- scom_data[which(scom_data$Reef_associated == 1 ),]
reef_scom <- reef_scom_data$species_SFTEP


###############extract molecular phylogeny of species
#don't mind the warning messages
#that is gonna happen with how the two phylogenies were constructed and names extracted
#however, created trimmed vectors with these values, in case they will be useful later

all_serran <- unlist(all_fam[["Serranidae"]])
mol_phy_all_serran <- fishtree_phylogeny(species = all_serran)
#only pulls out 218 species of 549
trimmed_serran <- mol_phy_all_serran[["tip.label"]]

all_carran <- unlist(all_fam[["Carangidae"]])
mol_phy_all_caran <- fishtree_phylogeny(species = all_carran)
#only pulls out 118 species of 146
trimmed_caran <- mol_phy_all_caran[["tip.label"]]

all_mura <- unlist(all_fam[["Muraenidae"]])
mol_phy_all_mura <- fishtree_phylogeny(species = all_mura)
#only pulls out 64 of 202 species
trimmed_mura <- mol_phy_all_mura[["tip.label"]]

all_labri <- unlist(all_fam[["Labridae"]])
mol_phy_all_labri <- fishtree_phylogeny(species = all_labri)
#only pulls 251 of 548 species
trimmed_labri <- mol_phy_all_labri[["tip.label"]]

all_exo <- unlist(all_fam[["Exocoetidae"]])
mol_phy_all_exo <- fishtree_phylogeny(species = all_exo)
#only pulls 41 of 71 species
trimmed_exo <- mol_phy_all_exo[["tip.label"]]

all_scorp <- unlist(all_fam[["Scorpaenidae"]])
mol_phy_all_scorp <- fishtree_phylogeny(species = all_scorp)
#only pulls 55 species of 223
trimmed_scorp <- mol_phy_all_scorp[["tip.label"]]

all_haem <- unlist(all_fam[["Haemulidae"]])
mol_phy_all_haem <- fishtree_phylogeny(species = all_haem)
#only pulls 87 species of 134
trimmed_haem <- mol_phy_all_haem[["tip.label"]]

all_poma <- unlist(all_fam[["Pomacentridae"]])
mol_phy_all_poma <- fishtree_phylogeny(species = all_poma)
#only pulls 222 of 400 species
trimmed_poma <- mol_phy_all_poma[["tip.label"]]

all_tetra <- unlist(all_fam[["Tetraodontidae"]])
mol_phy_all_tetra <- fishtree_phylogeny(species = all_tetra)
#only pulls 101 of 200 species
trimmed_tetra <- mol_phy_all_tetra[["tip.label"]]

all_ophich <- unlist(all_fam[["Ophichthidae"]])
mol_phy_all_ophich <- fishtree_phylogeny(species = all_ophich)
#only pulls 38 species of 323
trimmed_ophich <- mol_phy_all_ophich[["tip.label"]]

all_scom <- unlist(all_fam[["Scombridae"]])
mol_phy_all_scom <-fishtree_phylogeny(species = all_scom)
#only pulls 44 of 54 species
trimmed_scom <- mol_phy_all_scom[["tip.label"]]

################Serranidae
#plot phylogeny
plot(mol_phy_all_serran, show.tip.label = FALSE, no.margin = TRUE)

#categorise species as reef associated
reef_serran_trait_data <- data.frame(trimmed_serran, row.names = trimmed_serran)
reef_serran_trait_data$trait <- rep("non_reef", length(reef_serran_trait_data$trimmed_serran))
reef_serran <- gsub(x = reef_serran, pattern = " ", replacement = "_")
reef_serran_trait_data[which(reef_serran_trait_data$trimmed_serran %in% reef_serran), 2] <- "reef"

#plot traits on phylogeny

fmode<-as.factor(setNames(reef_serran_trait_data$trait, reef_serran_trait_data$trimmed_serran))

#png("Serranidae_all_trait_tree.png", units = "cm", width = 258 , height = 390, res = 500 )
dotTree(mol_phy_all_serran, fmode, colors=setNames(c("blue","red"),
                                   c("non_reef","reef")),ftype="i",fsize=0.1, no.margin = TRUE)
#dev.off()


#####################Carangidae
#plot phylogeny
plot(mol_phy_all_caran, show.tip.label = FALSE)

#categorise species as reef associated
reef_caran_trait_data <- data.frame(trimmed_caran, row.names = trimmed_caran)
reef_caran_trait_data$trait <- rep("non_reef", length(reef_caran_trait_data$trimmed_caran))
reef_caran <- gsub(x = reef_caran, pattern = " ", replacement = "_")
reef_caran_trait_data[which(reef_caran_trait_data$trimmed_caran %in% reef_caran), 2] <- "reef"

#plot traits on phylogeny

fmode_caran <-as.factor(setNames(reef_caran_trait_data$trait, reef_caran_trait_data$trimmed_caran))
dotTree(mol_phy_all_caran, fmode_caran, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.3)

#####################Muraenidae
#plot phylogeny
plot(mol_phy_all_mura, show.tip.label = FALSE)

#categorise species as reef associated
reef_mura_trait_data <- data.frame(trimmed_mura, row.names = trimmed_mura)
reef_mura_trait_data$trait <- rep("non_reef", length(reef_mura_trait_data$trimmed_mura))
reef_mura <- gsub(x = reef_mura, pattern = " ", replacement = "_")
reef_mura_trait_data[which(reef_mura_trait_data$retrimmed_mura %in% reef_mura), 2] <- "reef"

#plot traits on phylogeny

fmode_mura <-as.factor(setNames(reef_mura_trait_data$trait, reef_mura_trait_data$trimmed_mura))
dotTree(mol_phy_all_mura, fmode_mura, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.1)


#####################Labridae
#plot phylogeny
plot(mol_phy_all_labri, show.tip.label = FALSE)

#categorise species as reef associated
reef_labri_trait_data <- data.frame(trimmed_labri, row.names = trimmed_labri)
reef_labri_trait_data$trait <- rep("non_reef", length(reef_labri_trait_data$trimmed_labri))
reef_labri <- gsub(x = reef_labri, pattern = " ", replacement = "_")
reef_labri_trait_data[which(reef_labri_trait_data$trimmed_labri %in% reef_labri), 2] <- "reef"

#plot traits on phylogeny

fmode_labri <-as.factor(setNames(reef_labri_trait_data$trait, reef_labri_trait_data$trimmed_labri))
dotTree(mol_phy_all_labri, fmode_labri, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.1)

#########################Exocoetidae
#plot phylogeny
plot(mol_phy_all_exo, show.tip.label = FALSE)

#categorise species as reef associated
reef_exo_trait_data <- data.frame(trimmed_exo, row.names = trimmed_exo)
reef_exo_trait_data$trait <- rep("non_reef", length(reef_exo_trait_data$trimmed_exo))
reef_exo <- gsub(x = reef_exo, pattern = " ", replacement = "_")
reef_exo_trait_data[which(reef_exo_trait_data$trimmed_exo %in% reef_exo), 2] <- "reef"

#plot traits on phylogeny

fmode_exo <-as.factor(setNames(reef_exo_trait_data$trait, reef_exo_trait_data$trimmed_exo))
dotTree(mol_phy_all_exo, fmode_exo, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.1)

#########################Scorpaenidae
#plot phylogeny
plot(mol_phy_all_scorp, show.tip.label = FALSE)

#categorise species as reef associated
reef_scorp_trait_data <- data.frame(trimmed_scorp, row.names = trimmed_scorp)
reef_scorp_trait_data$trait <- rep("non_reef", length(reef_scorp_trait_data$trimmed_scorp))
reef_scorp <- gsub(x = reef_scorp, pattern = " ", replacement = "_")
reef_scorp_trait_data[which(reef_scorp_trait_data$trimmed_scorp %in% reef_scorp), 2] <- "reef"

#plot traits on phylogeny

fmode_scorp <-as.factor(setNames(reef_scorp_trait_data$trait, reef_scorp_trait_data$trimmed_scorp))
dotTree(mol_phy_all_scorp, fmode_scorp, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.1)

#######################Haemulidae
#plot phylogeny
plot(mol_phy_all_haem, show.tip.label = FALSE)

#categorise species as reef associated
reef_haem_trait_data <- data.frame(trimmed_haem, row.names = trimmed_haem)
reef_haem_trait_data$trait <- rep("non_reef", length(reef_haem_trait_data$trimmed_haem))
reef_haem <- gsub(x = reef_haem, pattern = " ", replacement = "_")
reef_haem_trait_data[which(reef_haem_trait_data$trimmed_haem %in% reef_haem), 2] <- "reef"

#plot traits on phylogeny

fmode_haem <-as.factor(setNames(reef_haem_trait_data$trait, reef_haem_trait_data$trimmed_haem))
dotTree(mol_phy_all_haem, fmode_haem, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.1)

######################Pomacentridae
#plot phylogeny
plot(mol_phy_all_poma, show.tip.label = FALSE)

#categorise species as reef associated
reef_poma_trait_data <- data.frame(trimmed_poma, row.names = trimmed_poma)
reef_poma_trait_data$trait <- rep("non_reef", length(reef_poma_trait_data$trimmed_poma))
reef_poma <- gsub(x = reef_poma, pattern = " ", replacement = "_")
reef_poma_trait_data[which(reef_poma_trait_data$trimmed_poma %in% reef_poma), 2] <- "reef"

#plot traits on phylogeny

fmode_poma <-as.factor(setNames(reef_poma_trait_data$trait, reef_poma_trait_data$trimmed_poma))
dotTree(mol_phy_all_poma, fmode_poma, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.1)

#####################Tetraodontidae
#plot phylogeny
plot(mol_phy_all_tetra, show.tip.label = FALSE)

#categorise species as reef associated
reef_tetra_trait_data <- data.frame(trimmed_tetra, row.names = trimmed_tetra)
reef_tetra_trait_data$trait <- rep("non_reef", length(reef_tetra_trait_data$trimmed_tetra))
reef_tetra <- gsub(x = reef_tetra, pattern = " ", replacement = "_")
reef_tetra_trait_data[which(reef_tetra_trait_data$trimmed_tetra %in% reef_tetra), 2] <- "reef"

#plot traits on phylogeny

fmode_tetra <-as.factor(setNames(reef_tetra_trait_data$trait, reef_tetra_trait_data$trimmed_tetra))
dotTree(mol_phy_all_tetra, fmode_tetra, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.1)

####################Ophichthidae
#plot phylogeny
plot(mol_phy_all_ophich, show.tip.label = FALSE)

#categorise species as reef associated
reef_ophich_trait_data <- data.frame(trimmed_ophich)
reef_ophich_trait_data$trait <- rep("non_reef", length(reef_ophich_trait_data$trimmed_ophich))
reef_ophich <- gsub(x = reef_ophich, pattern = " ", replacement = "_")
reef_ophich_trait_data[which(reef_ophich_trait_data$trimmed_ophich %in% reef_ophich), 2] <- "reef"

#plot traits on phylogeny

fmode_ophich <-as.factor(setNames(reef_ophich_trait_data$trait, reef_ophich_trait_data$trimmed_ophich))
dotTree(mol_phy_all_ophich, fmode_ophich, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.1)

#####################Scombridae
#plot phylogeny
plot(mol_phy_all_scom, show.tip.label = FALSE)

#categorise species as reef associated
reef_scom_trait_data <- data.frame(trimmed_scom, row.names = trimmed_scom)
reef_scom_trait_data$trait <- rep("non_reef", length(reef_scom_trait_data$trimmed_scom))
reef_scom <- gsub(x = reef_scom, pattern = " ", replacement = "_")
reef_scom_trait_data[which(reef_scom_trait_data$trimmed_scom %in% reef_scom), 2] <- "reef"

#plot traits on phylogeny

fmode_scom<-as.factor(setNames(reef_scom_trait_data$trait, reef_scom_trait_data$trimmed_scom))
dotTree(mol_phy_all_scom, fmode_scom, colors=setNames(c("blue","red"),
                                                   c("non_reef","reef")),ftype="i",fsize=0.1)








