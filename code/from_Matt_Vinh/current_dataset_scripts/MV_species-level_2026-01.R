library(here)
library(tidyverse)
library(janitor)


##### data

datapath <- here("data","aggregated","from_Matt_Vinh","MV_cleaned_2026-01-24.rds")
data <- readRDS(datapath)

taxonomy_path <- here("data","ebird_clements_checklist","Clements_v2025-October-2025.csv")
taxonomy_data <- read_csv(taxonomy_path)


##### filtering for all unique NCOS species recorded

intermediate <- data %>% filter(category %in% c("species","domestic")) %>% 
  select(c("species","category","e_bird_group")) %>% distinct()


##### adding NCOS surveys specific variables

intermediate$total_count <- NA * nrow(intermediate)
intermediate$number_years <- NA * nrow(intermediate)

no_repeats <- data %>% filter(repeat_observation == "No")

species_list <- unique(intermediate$species)

for (i in 1:nrow(intermediate)){
  intermediate$total_count[i] = sum(no_repeats$count[no_repeats$species == species_list[i]])
  intermediate$number_years[i] = length(unique(data$survey_year[data$species == species_list[i]]))
}


##### joining

species_level <- left_join(intermediate,taxonomy_data,by = c("species" = "English name"))
# two rows for osprey?
species_level <- clean_names(species_level)


##### saving 

csv_path <- here("data","aggregated","from_Matt_Vinh","MV_species-level_2026-01-24.csv")
write_csv(species_level,csv_path)

rds_path <- here("data","aggregated","from_Matt_Vinh","MV_species-level_2026-01-24.rds")
saveRDS(species_level,rds_path)
