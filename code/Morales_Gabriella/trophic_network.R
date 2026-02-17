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
