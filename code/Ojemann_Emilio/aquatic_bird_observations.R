#this script is for filtering bird surveys to observations relevant to Emilio's research project

#on birds potentially eating aquatic inverts

library(tidyverse)

surveys_2023_2025 <- read_rds(file = "code/Ojemann_Emilio/bird_surveys_2023_2025.rds")

aquatic_focal_taxa <- read_csv(file = "data/aquatic_diets/aquatic_focal_species.csv")

surveys_filtered_for_EO <- surveys_2023_2025 %>% 
  #filter to only aquatic focal species
  filter(species %in% aquatic_focal_taxa$species) %>% 
  #get rid of unnecessary columns
  select(species:observation_notes, x:season)

write_csv(surveys_filtered_for_EO, "data/aquatic_diets/aquatic_bird_observations_2023_2025.csv")
