
##### libraries ----

library(here)
library(tidyverse)



##### data ----

path <- here("data","SBA_hourly_precipitation_mm.csv")
data <- read_csv(path)



##### modification

precip <- data %>% 
  mutate(
    
    ### creating date type date variable excluding recorded time of day
    date = as.Date(valid,
                   format = "%m/%d/%Y %H:%M"),
    
    ### assigning survey id by consecutive month
    # matches survey id of survey-level data
    # requires second mutate() call
    survey_id = year(date) * 12 + month(date)
    
  ) %>% 
  
  ### finishing survey id variable creation
  mutate(survey_id = as.integer(survey_id - min(survey_id) + 1)) %>% 
  
  ### removing station variable
  # the station is the same for all data (SBA)
  select(survey_id,
         date,
         p01m,
         valid,
         -station)



##### saving ----

csvpath <- here("data","Matt_Vinh","SBA_hourly_precipitation_2026-03-01.csv")
write_csv(precip,csvpath)

rdspath <- here("data","Matt_Vinh","SBA_hourly_precipitation_2026-03-01.rds")
write_rds(precip,rdspath)
