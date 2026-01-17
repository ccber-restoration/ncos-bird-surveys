# the purpose of this script is to extract the full list of bird species from the survey data
#and to select species most relevant to the aquatic food web at NCOS

library(tidyverse)

focal_aquatic_groups <- c("Waterfowl & Friends", "Shorebirds")

#calculate the total non-repeat number of bird detections by species, then filter to the two focal aquatic groups
focal_aquatic_species <- read_rds("data/aggregated/MV_updated_aggregated.rds") %>% 
  #filter out repeat observations
  filter(repeat_observation != "Yes") %>% 
  #get total num,ber of observations
  group_by(species, general_type) %>% 
  summarize(total_count = sum(count)) %>% 
  #filter to two guilds of interest
  filter(general_type %in% focal_aquatic_groups) %>% 
  #sort by total_count
  arrange(-total_count)

#FIXME- should probably also filter out non-species categories

#write to csv

write_csv(focal_aquatic_species, "data/aquatic_diets/aquatic_focal_species.csv")

# summarize fish-eating bird species

#note that there are piscivorous species in the waterfowl & friends group (e.g. ducks & grebes)

focal_piscivorous_groups <- c("Herons, Egrets, Ibis", "Cormorants", "Kingfishers", "Gulls & Terns") 

focal_piscivorous_species <- read_rds("data/aggregated/MV_updated_aggregated.rds") %>% 
  #filter out repeat observations
  filter(repeat_observation != "Yes") %>% 
  #get total num,ber of observations
  group_by(species, general_type) %>% 
  summarize(total_count = sum(count)) %>% 
  #filter to two guilds of interest
  filter(general_type %in% focal_piscivorous_groups) %>% 
  #sort by total_count
  arrange(-total_count)


write_csv(focal_piscivorous_species, "data/aquatic_diets/piscivorous_focal_species.csv")
