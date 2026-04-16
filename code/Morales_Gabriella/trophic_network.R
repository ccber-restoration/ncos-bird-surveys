#this script is for visualizing potential trophic links between aquatic invertebrates and birds at NCOS
# trophic links were compiled by Gabriella primarily using Birds of the World

# 1. load packages ----
library(tidyverse)
library(readxl)

#packages we could use for visualizing networks:
library(bipartite)
library(igraph)

#package for adding icons to networks
library(rphylopic)

# 2. create full list of focal species, with abundances

 # read in abundances of focal species (invert-eaters) ----
  
# abundances (total count) for "waterfowl & friends" & shorebird species
# note that we dropped some of these because they mostly eat plants...
duck_shorebird_abundance <- read_csv(file = "data/aquatic_diets/aquatic_focal_species.csv")

# abundance for aquatic bird groups that eat fish (gulls & herons)
#also includes cormorant & kingfisher
gull_waders_abundance <-  read_csv(file = "data/aquatic_diets/piscivorous_focal_species.csv")

# combine dataframes into single list of aquatic birds that eat inverts and/or fish/aquatic vertebrates
bird_abundance <- bind_rows(duck_shorebird_abundance, gull_waders_abundance)

rm(duck_shorebird_abundance, gull_waders_abundance)

# 2. read in invert trophic link data ----
links_path <- "data/aquatic_diets/NCOS_aquatic_trophic_links_2026-02-10.xlsx"

#in network terminology this is an "edge list"
trophic_links <- read_xlsx(path = links_path, sheet = "trophic links") %>% 
  select(c(bird_species, invert_taxon)) %>% 
  #relocate(invert_taxon, .before = bird_species) %>% 
  filter(bird_species != "Whimbrel") %>% 
  filter(bird_species != "Hooded Merganser")

# 3. read in invert abundance data ----
invert_abundances <- read_csv(file= "data/aquatic_diets/invert_abundances.csv")

#keep only log column and invert taxon names
invert_abundances_log <- invert_abundances[, c("invert_taxon", "ln_count")]

#Chironomidae spelled wrong
invert_abundances_log$invert_taxon[
  invert_abundances_log$invert_taxon == "Chironimidae"
] <- "Chironomidae"

#make vector of ln_count data
invert_abundances_vector <- invert_abundances_log$ln_count
names(invert_abundances_vector) <- invert_abundances_log$invert_taxon

# create vector of bird names in invert network
birds <- unique(trophic_links$bird_species)
inverts <- unique(trophic_links$invert_taxon)

# filter bird abundance data to just species in invert network ---

birds_invert_abundance <- bird_abundance %>% 
  filter(species %in% birds)

#make vector of bird counts
bird_invert_abundances_vector <- birds_invert_abundance$total_count
names(bird_invert_abundances_vector) <- birds_invert_abundance$species

## create matrix for invert network ----
tl_matrix <- trophic_links %>% 
  as.matrix()

trophic_network <- graph_from_edgelist(tl_matrix, directed = FALSE)

adj_matrix <- as_adjacency_matrix(trophic_network, sparse=FALSE)

matrix_subset <- adj_matrix[birds, inverts]

link_colors1 <- matrix("gray80",
                       nrow = nrow(matrix_subset),
                       ncol = ncol(matrix_subset))

#reorder abundances to match matrix
invert_abundances_vector <- invert_abundances_vector[colnames(matrix_subset)]
bird_invert_abundances_vector <- bird_invert_abundances_vector[rownames(matrix_subset)]

# Assign colors by prey type

# Ostracoda
link_colors1[, colnames(matrix_subset) == "Ostracoda"][
  matrix_subset[, colnames(matrix_subset) == "Ostracoda"] > 0] <- "#264653"

# Corixidae
link_colors1[, colnames(matrix_subset) == "Corixidae"][
  matrix_subset[, colnames(matrix_subset) == "Corixidae"] > 0] <- "#287271"

# Chironomidae
link_colors1[, colnames(matrix_subset) == "Chironomidae"][
  matrix_subset[, colnames(matrix_subset) == "Chironomidae"] > 0] <- "#2a9d8f"

# Oligochaeta
link_colors1[, colnames(matrix_subset) == "Oligochaeta"][
  matrix_subset[, colnames(matrix_subset) == "Oligochaeta"] > 0] <- "#8ab17d"

# Copepoda
link_colors1[, colnames(matrix_subset) == "Copepoda"][
  matrix_subset[, colnames(matrix_subset) == "Copepoda"] > 0] <- "#babb74"

# Ephydridae
link_colors1[, colnames(matrix_subset) == "Ephydridae"][
  matrix_subset[, colnames(matrix_subset) == "Ephydridae"] > 0] <- "#e9c46a"

# Cladocera
link_colors1[, colnames(matrix_subset) == "Cladocera"][
  matrix_subset[, colnames(matrix_subset) == "Cladocera"] > 0] <- "#f4a261"

# Nematoda
link_colors1[, colnames(matrix_subset) == "Nematoda"][
  matrix_subset[, colnames(matrix_subset) == "Nematoda"] > 0] <- "#ee8959"

# Ceratopogonidae
link_colors1[, colnames(matrix_subset) == "Ceratopogonidae"][
  matrix_subset[, colnames(matrix_subset) == "Ceratopogonidae"] > 0] <- "#e76f51"


png(file = "figures/invert_network.png", width = 600, height = 900, units = "px", res = 100)

#continue filling out trophic links data
#create draft network viz based on existing data
plotweb(web = matrix_subset, text_size =0.8, horizontal = TRUE, link_color = link_colors1, 
        higher_abundances = invert_abundances_vector, lower_abundances = bird_invert_abundances_vector)

dev.off()


# ~~~~~~~~~~~~~~~~~ ----

# piscivorous network ----

## load trophic links data ----
fish_links_path <- "data/aquatic_diets/NCOS_piscivorous_trophic_links_2026-02-17.xlsx"

# in network terminology this is an "edge list"
fish_trophic_links <- read_xlsx(path = fish_links_path, sheet = "trophic links") %>% 
  select(c(bird_species, prey_taxon))
  #relocate(prey_taxon, .before = bird_species) 

# create vector of bird names
birds_fish_eating <- unique(fish_trophic_links$bird_species)

#create vector of (fish/frog/crayfish) prey names
prey <- unique(fish_trophic_links$prey_taxon)

#filter bird abundance dataframe to only include species within the vert network 
birds_vert_abundance <- bird_abundance %>% 
  filter(species %in% birds_fish_eating)
#only has 19, expecting 20...


#check which are missing...
setdiff(birds_fish_eating, birds_vert_abundance$species)
# TODO- for Francis- sort out RBME

tl_matrix_fish <- fish_trophic_links %>% 
  as.matrix()

#create netowrk "graph"
trophic_network_fish <- graph_from_edgelist(tl_matrix_fish, directed = FALSE)

#extract adjacency matrix
adj_matrix_fish <- as_adjacency_matrix(trophic_network_fish, sparse=FALSE)

#subset matrix to avoid duplication
matrix_subset_fish <- adj_matrix_fish[birds_fish_eating, prey]

link_colors <- matrix("gray80",
                      nrow = nrow(matrix_subset_fish),
                      ncol = ncol(matrix_subset_fish))

# Assign colors by prey type

# Small fish
link_colors[, colnames(matrix_subset_fish) == "small fish"][
  matrix_subset_fish[, colnames(matrix_subset_fish) == "small fish"] > 0] <- "#287271"

# Large fish
link_colors[, colnames(matrix_subset_fish) == "large fish"][
  matrix_subset_fish[, colnames(matrix_subset_fish) == "large fish"] > 0] <- "#8ab17d"

# Amphibia
link_colors[, colnames(matrix_subset_fish) == "Amphibia"][
  matrix_subset_fish[, colnames(matrix_subset_fish) == "Amphibia"] > 0] <- "#e9c46a"

# Crayfish
link_colors[, colnames(matrix_subset_fish) == "Procambarus clarkii"][
  matrix_subset_fish[, colnames(matrix_subset_fish) == "Procambarus clarkii"] > 0] <- "#e76f51"


## save to file (png) ----
png(file = "figures/trophic_network_fish_eating.png", width = 600, height = 800, units = "px", res = 100)

plotweb(web = matrix_subset_fish, text_size =1.1, horizontal = TRUE, link_color = link_colors)


dev.off()


# add images to network attempt


#FHJ notes: also saved as pdf with same width and height (no units needed bc inches is default), no res argument

png(file = "figures/Food_Webs/trophic_network_fish_eating_phylopic.png", width = 7, height = 7.5, units = "in", res = 200)

plotweb(web = matrix_subset_fish, text_size =1.1, horizontal = TRUE, link_color = link_colors)
add_phylopic_base(name = "Mugil cephalus",
                  x = -0.18, y = 0.53,
                  width = 0.2,
                  color = "black",
                  verbose = TRUE)
add_phylopic_base(name = "Procambarus clarkii",
                  x = -0.18, y = 0.035,
                  width = 0.18,
                  color = "black",
                  verbose = TRUE,
                  angle = 90)
add_phylopic_base(name = "Gnatholepis cauerensis",
                  x = -0.18, y = 0.82,
                  width = 0.2,
                  color = "black",
                  verbose = TRUE)
add_phylopic_base(name = "Pseudacris maculata",
                  x = -0.18, y = 0.3,
                  width = 0.13,
                  color = "black",
                  verbose = TRUE)
dev.off()

#make phylopic network for invert trophic network

png(file = "figures/Food_Webs/invert_network_phylopic.png", width = 6, height = 8, units = "in", res = 200)

plotweb(web = matrix_subset, text_size =1, horizontal = TRUE, mar = c(2, 0.82, 0.82, 0.42), link_color = link_colors1)
add_phylopic_base(name = "Cypris", #Ostracoda image 6
                  x = -0.15, y = 0.93,
                  width = 0.11,
                  color = "black",
                  verbose = TRUE)
add_phylopic_base(name = "Corixidae", 
                  x = -0.15, y = 0.785,
                  width = 0.13,
                  color = "black",
                  verbose = TRUE)
add_phylopic_base(name = "Clunio marinus", #Chironomidae image 5
                  x = -0.15, y = 0.595,
                  width = 0.13,
                  color = "black",
                  verbose = TRUE)
add_phylopic_base(name = "Lumbricina", #Oligochaeta image 3
                  x = -0.16, y = 0.43,
                  width = 0.25,
                  color = "black",
                  verbose = TRUE,
                  angle = 90)
add_phylopic_base(name = "Copepoda", 
                  x = -0.15, y = 0.3,
                  width = 0.07,
                  color = "black",
                  verbose = TRUE)
add_phylopic_base(name = "Ephydroidea", #Ephydridae broader family
                  x = -0.15, y = 0.205,
                  width = 0.12,
                  color = "black",
                  verbose = TRUE)
add_phylopic_base(name = "Daphnia", #Cladocera image 1
                  x = -0.15, y = 0.1,
                  width = 0.07,
                  color = "black",
                  verbose = TRUE)
add_phylopic_base(name = "Caenorhabditis elegans", #Nematode image 4Ceratopogonidae
                  x = -0.15, y = 0.03,
                  width = 0.14,
                  color = "black",
                  verbose = TRUE,
                  angle = 90)
add_phylopic_base(name = "Ceratopogonidae",
                  x = -0.15, y = -0.03,
                  width = 0.095,
                  color = "black",
                  verbose = TRUE)


dev.off()


