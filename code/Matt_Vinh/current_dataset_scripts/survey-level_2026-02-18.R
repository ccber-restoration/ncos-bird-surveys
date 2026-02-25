##### libraries ----

library(here)
library(tidyverse)
library(janitor)
library(visdat)
library(naniar)


##### data ----

datapath <- here("data","aggregated","Matt_Vinh","compiled_and_cleaned_2026-02-18.rds")
data <- read_rds(datapath)



##### data formatting ----

# TODO- consider using relocate() upstream to reorder columns
# TODO- FHJ to follow up on water level issue

survey_info <- data %>% 
  
  ### removing observation-specific variables
  select(-c(objectid:observation_notes,
            observers,
            global_id:y,
            direction_of_travel_a:standard_deviation_m,
            e_bird_group:general_type,
            category)) %>% 
  
  ### keeping only one row per survey
  distinct(observation_date,
           .keep_all = T) 

vis_dat(survey_info)

survey_missing_summary <- miss_var_summary(survey_info)


#TODO- add day of year column using yday() function from lubridate

species_wide <- data %>% 
  
  ### only including "species" category observations
  filter(category == "species") %>% 
  
  ### giving each species a column filled with abundance per survey
  pivot_wider(
    names_from = species,
    values_from = count,
    values_fn = sum,
    values_fill = 0,
    id_cols = observation_date
  )

survey_level_data <- full_join(survey_info,
                               species_wide,
                               by = "observation_date") %>% 
  arrange(-desc(observation_date)) %>% 
  mutate(
    
    ### assigning survey id from 1 to 96
    survey_id = 1:96,
    ### calculating the day of the survey year
    temp_start = make_date(year(observation_date) - (month(observation_date) < 9),9,1),
    day_of_survey_year = as.integer(observation_date - temp_start) + 1
  ) %>% 
  
  ### reorder variables
  select(c(survey_id,
           day_of_survey_year,
           observation_date,
           water_level:slough_water_elevation_ft,
           `Snowy Egret`:`Fox Sparrow`,
           -temp_start))


### visualize data types and missingness

vis_dat(survey_level_data)

missing_data_summary <- miss_var_summary(survey_level_data)


### declutter

rm(data,survey_info,species_wide)

#TODO- remove columns with 100% missing values


##### saving ----

csv_path <- here("data","aggregated","Matt_Vinh","survey-level_2026-02-18.csv")
write_csv(survey_level_data,csv_path)

rds_path <- here("data","aggregated","Matt_Vinh","survey-level_2026-02-18.rds")
saveRDS(survey_level_data,rds_path)
