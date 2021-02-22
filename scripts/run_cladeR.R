### Run cladeR script

fishframe <- read.csv("../datasets/full_dataset_wmissingsp_moleculartwoo.csv")
molecular_fishframe <- fishframe %>% filter(molecular_data == 1)
molecular_fishframe <- fishframe %>% filter(molecular_data == 1, depth_lower <= 150)
clade_data <- molecular_fishframe[,c(5,13)]
clades_data <- clade_data %>% remove_rownames() %>%
  column_to_rownames(var = "Taxa")
output <- cladeR(clades_data, "molecular", 28)
