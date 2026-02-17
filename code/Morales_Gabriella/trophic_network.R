#this script is for visualizing potential trophic links between aquatic invertebrates and birds at NCOS
# trophic links were compiled by Gabriella primarily using Birds of the World

#load packages
library(tidyverse)
library(readxl)

#packages we could use for visualizing networks:
library(bipartite)
library(igraph)


links_path <- "data/aquatic_diets/NCOS_aquatic_trophic_links_2026-02-10.xlsx"

#in network terminology this is an "edge list"
trophic_links <- read_xlsx(path = links_path, sheet = "trophic links") %>% 
  select(c(bird_species, invert_taxon)) %>% 
  #relocate(invert_taxon, .before = bird_species) %>% 
  filter(bird_species != "Whimbrel") %>% 
  filter(bird_species != "Hooded Merganser")

#create vector of bird names
birds <- unique(trophic_links$bird_species)
inverts <- unique(trophic_links$invert_taxon)

tl_matrix <- trophic_links %>% 
  as.matrix()

trophic_network <- graph_from_edgelist(tl_matrix, directed = FALSE)

adj_matrix <- as_adjacency_matrix(trophic_network, sparse=FALSE)

matrix_subset <- adj_matrix[birds, inverts]

pdf(file = "figures/invert_network.pdf", width = 6, height = 9)

#continue filling out trophic links data
#create draft network viz based on existing data
plotweb(web = matrix_subset, text_size =1.1, horizontal = TRUE)

dev.off()


# piscivorous network ----

# FIXME- Gabriella to update, using new version of bipartite plotweb() AND saving directly to pdf

fish_links_path <- "data/aquatic_diets/NCOS_piscivorous_trophic_links_2026-02-17.xlsx"

#in network terminology this is an "edge list"
fish_trophic_links <- read_xlsx(path = links_path, sheet = "trophic links") %>% 
  select(c(bird_species, prey_taxon)) %>% 
  relocate(prey_taxon, .before = bird_species) 

#create vector of bird names
birds_fish_eating <- unique(fish_trophic_links$bird_species)
prey <- unique(fish_trophic_links$prey_taxon)

tl_matrix_fish <- fish_trophic_links %>% 
  as.matrix()

#create netowrk "graph"
trophic_network_fish <- graph_from_edgelist(tl_matrix_fish, directed = FALSE)

#extract adjacency matrix
adj_matrix_fish <- as_adjacency_matrix(trophic_network_fish, sparse=FALSE)

#subset matrix to avoid duplication
matrix_subset_fish <- adj_matrix_fish[prey, birds_fish_eating]

#continue filling out trophic links data
#create draft network viz based on existing data
plotweb(web = matrix_subset_fish, text.rot = 90)


