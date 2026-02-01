
##### libraries ----

library(tidyverse)
library(here)
library(janitor)



##### existing compiled data cleaning ----


birdsurveyfilepath <- here("data","aggregated","dryad_2017-2023",
                           "doi_10_5061_dryad_bvq83bkhz__v20240617",
                           "NCOS_Monthly_Bird_Surveys_2017-2023.csv")
birdsurveys <- read_csv(birdsurveyfilepath) # 11 variables


birdsurveys_filtered <- birdsurveys %>% 
  mutate(
    
    ### data type
    Count = as.integer(Count),
    
    ### replacing missing observation counts with 1
    Count = replace_na(Count,1),
    
    ### cleaning substrate names
    # replacing <Null> and NA substrates with new Unknown level
    Substrate = replace(Substrate,
                        Substrate == "rocks",
                        "Rock"),
    Substrate = replace(Substrate,
                        Substrate == "Other (Enter in Observation Notes",
                        "Other"),
    Substrate = replace(Substrate,
                        Substrate == "<Null>",
                        "Unknown"),
    Substrate = replace_na(Substrate,"Unknown"),
    
    ### cleaning observation date
    # removing time of observation
    # - only some observations have time included with observation date
    # to determine observation date of two rows missing observation date:
    # - filter birdsurveys by year == 2022 because year of rows missing dates are 2022
    # - then rows should be entered in chronological order for 2022?
    # - NA observation dates lie between dates 2022-04-20 and 2022-05-16
    # - assigning 2022-04-20 to one row and 2022-05-16 to the other
    obs_date2 = substr(birdsurveys$Observation.Date,1,10),
    observation_date = parse_date_time(obs_date2,
                                       orders = c("mdy","ymd")),
    observation_date = as.Date(observation_date),
    observation_date = replace(observation_date,
                               which(is.na(observation_date))[1],
                               as.Date("2022-04-20")),
    observation_date = replace(observation_date,
                               which(is.na(observation_date)),
                               as.Date("2022-05-16")),
    
    ### adding survey year variable
    survey_year = case_when(
      observation_date >= as.Date("2017-09-01") & observation_date <= as.Date("2018-08-31") ~ 1,
      observation_date >= as.Date("2018-09-01") & observation_date <= as.Date("2019-08-31") ~ 2,
      observation_date >= as.Date("2019-09-01") & observation_date <= as.Date("2020-08-31") ~ 3,
      observation_date >= as.Date("2020-09-01") & observation_date <= as.Date("2021-08-31") ~ 4,
      observation_date >= as.Date("2021-09-01") & observation_date <= as.Date("2022-08-31") ~ 5,
      observation_date >= as.Date("2022-09-01") & observation_date <= as.Date("2023-08-31") ~ 6),
    
    ### adding season variable
    season = case_when(
      month(observation_date) %in% c(1,2,12) ~ "Winter",
      month(observation_date) %in% c(3:5) ~ "Spring",
      month(observation_date) %in% c(6:8) ~ "Summer",
      month(observation_date) %in% c(9:11) ~ "Fall")) %>% 
  
  ### removing intermediate, unnecessary observation date variables
  select(-c("Observation.Date","obs_date2")) %>% 
  
  ### janitor package name cleaning
  clean_names()


### declutter
rm(birdsurveys)



##### wrangling recent data (2023-2025) ----


csvpath23 <- here("data","2023")
csvpath24 <- here("data","2024")
csvpath25 <- here("data","2025")


### file extraction with list.files()
csvfiles23 <- list.files(path = csvpath23,pattern = "2023_(09|10|11|12)_.*\\.csv$",
                         full.names = T)
csvfiles24 <- list.files(path = csvpath24,pattern = "\\.csv$",full.names = T)
csvfiles25 <- list.files(path = csvpath25,pattern = "\\.csv$",full.names = T)


### reading files with map_dfr() and cleaning
# for all survey data objects, observation date variable is added,
# observation date intermediate variable "parsed" is removed,
# and NA substrates become new Unknown level

df23 <- map_dfr(csvfiles23,~ read_csv(.x,col_types = cols(.default = "c"),
                                      locale = locale(encoding = "Latin1")) %>% 
                  mutate(
                    
                    # intermediate and new variable
                    parsed = mdy_hms(CreationDate),
                    observation_date = as.Date(parsed),
                    
                    # new substrate level
                    Substrate = replace_na(Substrate,"Unknown"))) %>% 
  # remove intermediate
  select(-"parsed")

df24 <- map_dfr(csvfiles24,~ read_csv(.x,col_types = cols(.default = "c"),
                                      locale = locale(encoding = "Latin1")) %>% 
                  mutate(
                    
                    # intermediate na dnew variable
                    parsed = mdy_hms(CreationDate),
                    observation_date = as.Date(parsed),
                    
                    # new substrate level
                    Substrate = replace_na(Substrate,"Unknown"),
                    
                    # replacing erroneous date
                    # 3 observations with "2020-10-24", many more with "2024-10-23"
                    observation_date = replace(observation_date,
                                               observation_date == "2020-10-24",
                                               "2024-10-23"))) %>% 
  
  # removing two extra variables with no data, along with intermediate
  select(-c("parsed",`Direction of travel (°)`,`Compass reading (°)`))

df25 <- map_dfr(csvfiles25,~ read_csv(.x,col_types = cols(.default = "c"),
                                      locale = locale(encoding = "Latin1")) %>% 
                  mutate(
                    
                    # intermediate and new variable
                    parsed = mdy_hms(CreationDate),
                    observation_date = as.Date(parsed),
                    
                    # new substrate level
                    Substrate = replace_na(Substrate,"Unknown"))) %>% 
  
  # remove intermediate
  select(-"parsed")


### rbind-ing bird survey data of each year, new variables and cleaning
compiled <- rbind(df23,df24,df25) %>% 
  mutate(
    
    ### new variable
    year = year(observation_date),
    
    ### correct data type
    observation_date = as.Date(observation_date),
    
    ### adding survey year variable
    survey_year = case_when(
      observation_date >= as.Date("2023-09-01") & observation_date <= as.Date("2024-08-31") ~ 7,
      observation_date >= as.Date("2024-09-01") & observation_date <= as.Date("2025-08-31") ~ 8),
    
    ### adding season variable
    season = case_when(
      month(observation_date) %in% c(1,2,12) ~ "Winter",
      month(observation_date) %in% c(3:5) ~ "Spring",
      month(observation_date) %in% c(6:8) ~ "Summer",
      month(observation_date) %in% c(9:11) ~ "Fall"),
    
    ### fixing observations with species in notes variable
    Species = replace(Species,
                      `Observation Notes` == "American bittern",
                      "American Bittern"),
    Species = replace(Species,
                      `Observation Notes` == "Bittern",
                      "American Bittern"),
    Species = replace(Species,
                      `Observation Notes` == "sharp skinned hawk",
                      "Sharp-shinned Hawk"),
    Species = replace(Species,
                      `Observation Notes` == "hummingbird sp",
                      "Hummingbird sp."),
    Species = replace(Species,
                      `Observation Notes` == "Sharp Shinned hawk",
                      "Sharp-shinned Hawk"),
    Species = replace(Species,
                      `Observation Notes` == "swinhoes white eye",
                      "Swinhoe's White-eye"),
    Species = replace(Species,
                      `Observation Notes` == "Wilson's Phalarope",
                      "Wilson's Phalarope"),
    Species = replace(Species,
                      `Observation Notes` == "Wilson's phalarope",
                      "Wilson's Phalarope"),
    Species = replace(Species,
                      `Observation Notes` == "wilsons phaloropes 4 repeat",
                      "Wilson's Phalarope"),
    Species = replace(Species,
                      `Observation Notes` == "barn owl",
                      "American Barn Owl"),
    Species = replace(Species,
                      `Observation Notes` == "horned lark",
                      "Horned Lark"),
    Species = replace(Species,
                      `Observation Notes` == "mountain bluebird",
                      "Mountain Bluebird"),
    Species = replace(Species,
                      `Observation Notes` == "chestnut backed chickadee",
                      "Chestnut-backed Chickadee"),
    Species = replace(Species,
                      `Observation Notes` == "lazuli bunting",
                      "Lazuli Bunting"),
    Species = replace(Species,
                      `Observation Notes` == "Swinhoes White Eye",
                      "Swinhoe's White-eye"),
    Species = replace(Species,
                      `Observation Notes` == "cost as hummingbird",
                      "Costa's Hummingbird"),
    # guessing that this is a peregrine falcon because no other bird species with
    # "falcon" in the name have been recorded
    Species = replace(Species,
                      `Observation Notes` == "falcon ppp flew over",
                      "Peregrine Falcon"),
    Species = replace(Species,
                      `Observation Notes` == "swallow spp",
                      "Swallow sp."),
    # one species remains na with the note:
    # - "orphaned ducklings", row 1449
    
    ### filling remaining NA species with new level Unknown
    Species = replace_na(Species,"Unknown"),
    
    ### data type
    Count = as.integer(Count),
    
    ### replacing NA count with 1
    Count = replace_na(Count,1))


### declutter
rm(df23,df24,df25)



##### compiling aggregates ----
#TODO declutter objects after not needed

# reading eBird.csv, joining with new aggregated dataframe
ebirdpath <- here("data","ebird_clements_checklist",
                  "ebird-Clements-v2018-integrated-checklist-August-2018.csv")
ebird <- read_csv(ebirdpath)
ebird <- ebird %>% distinct(`English name`,`eBird species group`,category)
# now includes category variable for distinction of taxa not at species level
newAndEbird <- left_join(newaggregate,ebird,by = c("Species" = "English name"))
# + 1 variable, total 60

# using existing general type designations to apply to new aggregated dataframe
species <- birdsurveys_filtered %>% select(c("Species","General.Type"))
species_unique <- species %>% distinct() # reduces data size from 10634 to 169

#use leftjoin here
newjoined <- left_join(newAndEbird,species_unique,by = "Species")


# + 1 row, total 61


# variable renaming
newjoined <- newjoined %>% 
  rename(General_Type = General.Type,
         eBird_Group = "eBird species group",
         Repeat_Observation = `Repeat Observation`,
         Breeding_Activity = `Breeding Activity`)
birdsurveys_filtered <- birdsurveys_filtered %>% 
  rename(General_Type = General.Type,
         eBird_Group = eBird.Group,
         Repeat_Observation = Repeat.Observation,
         Breeding_Activity = Breeding.Activity)

# combining new aggregated with old aggregated
updated_aggregated_survey_data <- bind_rows(newjoined,birdsurveys_filtered)
# + 3 variables, total 64
# 4034 obs + 11343 obs = 15377 obs
# writes NAs for variables not common to both dataframes



# manipulating data types
updated_aggregated_survey_data$Substrate <- 
  as.factor(updated_aggregated_survey_data$Substrate)
updated_aggregated_survey_data$`Water Level`<- 
  as.numeric(updated_aggregated_survey_data$`Water Level`)
updated_aggregated_survey_data$`Starting Temp (F)` <- 
  as.numeric(updated_aggregated_survey_data$`Starting Temp (F)`)
updated_aggregated_survey_data$`Starting % Cloud Cover` <- 
  as.numeric(updated_aggregated_survey_data$`Starting % Cloud Cover`)
updated_aggregated_survey_data$`Ending Temp (F)` <- 
  as.numeric(updated_aggregated_survey_data$`Ending Temp (F)`)
updated_aggregated_survey_data$`Ending % Cloud Cover` <- 
  as.numeric(updated_aggregated_survey_data$`Ending % Cloud Cover`)
updated_aggregated_survey_data$`Horizontal Accuracy (m)` <- 
  as.numeric(updated_aggregated_survey_data$`Horizontal Accuracy (m)`)
updated_aggregated_survey_data$`Vertical Accuracy (m)` <- 
  as.numeric(updated_aggregated_survey_data$`Vertical Accuracy (m)`)
updated_aggregated_survey_data$Latitude <- 
  as.numeric(updated_aggregated_survey_data$Latitude)
updated_aggregated_survey_data$Longitude <- 
  as.numeric(updated_aggregated_survey_data$Longitude)
updated_aggregated_survey_data$Altitude <- 
  as.numeric(updated_aggregated_survey_data$Altitude)
updated_aggregated_survey_data$PDOP <- as.numeric(updated_aggregated_survey_data$PDOP)
updated_aggregated_survey_data$HDOP <- as.numeric(updated_aggregated_survey_data$HDOP)
updated_aggregated_survey_data$VDOP <- as.numeric(updated_aggregated_survey_data$VDOP)
updated_aggregated_survey_data$`Number of Satellites` <- 
  as.numeric(updated_aggregated_survey_data$`Number of Satellites`)

# removing redundant observation date variables
updated_aggregated_survey_data <- updated_aggregated_survey_data %>% 
  select(-c(Observation.Date,obs_date2))

updated_aggregated_survey_data <- clean_names(updated_aggregated_survey_data)

# writing .csv and .rds
csvpath <- here("data","aggregated","MV_updated_aggregated.csv")
write_csv(updated_aggregated_survey_data,csvpath)
rdspath <- here("data","aggregated","MV_updated_aggregated.rds")
saveRDS(updated_aggregated_survey_data,rdspath)

