#this script is for filtering bird surveys to observations relevant to Emilio's research project

#on birds potentially eating aquatic inverts

library(tidyverse)
library(sf)
library(mapview)


surveys_2023_2025 <- read_rds(file = "code/Ojemann_Emilio/bird_surveys_2023_2025.rds")

aquatic_focal_taxa <- read_csv(file = "data/aquatic_diets/aquatic_focal_species.csv")

surveys_filtered_for_EO <- surveys_2023_2025 %>% 
  #filter to only aquatic focal species
  filter(species %in% aquatic_focal_taxa$species) %>% 
  #get rid of unnecessary columns
  select(species:observation_notes, x:season) %>% 
  filter(substrate != "Flyover")

#write_csv(surveys_filtered_for_EO, "data/aquatic_diets/aquatic_bird_observations_2023_2025.csv")


#summarize sightings

species_summary <- surveys_filtered_for_EO %>% 
  group_by(species) %>% 
  summarize(total = sum(count)) %>% 
  arrange(-total)

sightings_summary <- surveys_filtered_for_EO %>% 
  group_by(species) %>% 
  summarize(n_observations = n()) %>% 
  arrange(-n_observations)

#demo map

aquatic_birds_sf <-  st_as_sf(surveys_filtered_for_EO, coords = c("x", "y"), crs = 4326)

#display interactive map
mapview(aquatic_birds_sf)
