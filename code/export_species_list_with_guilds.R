# THe purpose of this script is to obtain the list of species with the guilds assigned by the Cheadle Center
# Because I cannot find the most recent guild list as a standalone file, I am extracting it from the data set itself (i.e. the Dryad version)

library(tidyverse)

library(auk)

ebird_taxonomy <- ebird_taxonomy


guild_species_list <- read_rds("data/Matt_Vinh/compiled_and_cleaned_2026-02-18.rds") %>% 
  select(c(species, e_bird_group, general_type, category)) %>% 
  distinct() %>% 
  left_join(ebird_taxonomy, by = join_by(category, species == common_name)) %>% 
  arrange(taxonomic_order) 

guild_species_list %>% 
  select(species, e_bird_group, general_type) %>% 
  write_csv("data/guild_assignments_extracted_from_2026-02-18_NCOS_data.csv")
