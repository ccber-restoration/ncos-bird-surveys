library(here)
library(tidyverse)
library(janitor)


##### data

datapath <- here("data","aggregated","from_Matt_Vinh","MV_cleaned_2026-01-24.rds")
data <- readRDS(datapath)


##### removing bird observation variables and removing all duplicate observation dates

survey_level <- data %>% select(-c("species":"breeding_activity",
                                   "e_bird_group":"general_type")) %>% 
  distinct(observation_date,.keep_all = T)
# 95 rows, matches number of unique survey dates in original data


##### saving 

csv_path <- here("data","aggregated","from_Matt_Vinh","MV_survey-level_2026-01-25.csv")
write_csv(survey_level,csv_path)

rds_path <- here("data","aggregated","from_Matt_Vinh","MV_survey-level_2026-01-25.rds")
saveRDS(survey_level,rds_path)
