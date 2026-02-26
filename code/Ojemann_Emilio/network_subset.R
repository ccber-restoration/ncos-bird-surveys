# subset global network to just the birds observed in the vernal pool zone in "wet" months

#note that this relies on objects from trophic_network.R

library(readxl)

vp_wet_birds <- read_xls(path = "data/aquatic_diets/BirdsWetVernalExport.xls")

vp_wet_birds_list <- unique(x = vp_wet_birds$species)

vp_wet_links <- trophic_links  %>% 
  filter(bird_species %in% vp_wet_birds_list)


#create vector of bird names
birds <- unique(vp_wet_links$bird_species)
inverts <- unique(vp_wet_links$invert_taxon)

tl_matrix_vp <- vp_wet_links %>% 
  as.matrix()

trophic_network_vp <- graph_from_edgelist(tl_matrix_vp, directed = FALSE)

adj_matrix_vp <- as_adjacency_matrix(trophic_network_vp, sparse=FALSE)

matrix_subset <- adj_matrix_vp[birds, inverts]

pdf(file = "figures/invert_network_vp_wet.pdf", width = 6, height = 9)

#continue filling out trophic links data
#create draft network viz based on existing data
plotweb(web = matrix_subset, text_size =1.1, horizontal = TRUE)

dev.off()
