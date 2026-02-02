##### libraries ----

library(tidyverse)
library(here)
library(janitor)



##### existing compiled data cleaning ----


birdsurveyfilepath <- here("data","aggregated","dryad_2017-2023",
                           "doi_10_5061_dryad_bvq83bkhz__v20240617",
                           "NCOS_Monthly_Bird_Surveys_2017-2023.csv")
birdsurveys <- read_csv(birdsurveyfilepath) # 11 variables


### cleaning and adding new variables
# + 2 variables: survye_year, season
# 13 total variables, 11351 observations
birdsurveys_cleaned <- birdsurveys %>% 
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
# all have 55 variables

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
# + 3 variables: year, survey_year, season
# total 58 variables, 4848 observations
compiled1 <- rbind(df23,df24,df25) %>% 
  mutate(
    
    ### new variable
    year = year(observation_date),
    
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
    Count = replace_na(Count,1)) %>% 
  
  clean_names()


### declutter
rm(df23,df24,df25)



##### adding bird grouping variables and joining data ----


ebirdpath <- here("data","ebird_clements_checklist",
                  "ebird-Clements-v2018-integrated-checklist-August-2018.csv")
ebird <- read_csv(ebirdpath)


### ebird_clements ebird variable joining
# + 1 variable
# total 59 variables, 4848 observations
ebird_group <- ebird %>% 
  distinct(`English name`, `eBird species group`)

compiled2 <- left_join(compiled1,
                       ebird_group,
                       by = c("species" = "English name")) %>% 
  clean_names() %>% 
  
  # renaming for better row binding with birdsurveys_cleaned
  rename(e_bird_group = e_bird_species_group)


### existing dataset general type variable joining
# + 1 variable
# total 60 variables, 4848 observations
general_type <- birdsurveys_cleaned %>% 
  distinct(species, general_type)

compiled3 <- left_join(compiled2,
                       general_type,
                       by = "species") %>% 
  clean_names()


### binding compiled3 with birdsurveys_cleaned
# 4848 observations + 11351 observations = 16199 observations
# total 61 variables
# + 1 variable: slough_water_elevation_ft only in birdsurveys_cleaned
compiled_new_and_old <- bind_rows(compiled3, birdsurveys_cleaned)


### ebird_clements category variable joining
# + 1 variable
# total 62 variables, 16199 observations
category <- ebird %>% 
  distinct(`English name`, category)

compiled_final <- left_join(compiled_new_and_old,
                            category,
                            by = c("species" = "English name"))


### declutter
rm(ebird,ebird_group,general_type,category,
   compiled1,compiled2,compiled_new_and_old)



##### further cleaning ----


compiled_final <- compiled_final %>% 
  mutate(
    
    ### data types
    
    # factor
    substrate = as.factor(substrate),
    
    #numeric
    water_level = as.numeric(water_level),
    starting_temp_f = as.numeric(starting_temp_f),
    starting_percent_cloud_cover = as.numeric(starting_percent_cloud_cover),
    ending_temp_f = as.numeric(ending_temp_f),
    ending_percent_cloud_cover = as.numeric(ending_percent_cloud_cover),
    horizontal_accuracy_m = as.numeric(horizontal_accuracy_m),
    vertical_accuracy_m = as.numeric(vertical_accuracy_m),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    altitude = as.numeric(altitude),
    pdop = as.numeric(pdop),
    hdop = as.numeric(hdop),
    vdop = as.numeric(vdop),
    number_of_satellites = as.numeric(number_of_satellites),
    
    ### taxonomy update
    # based on investigation during species-level dataset cleaning,
    # the following species have no taxonomic match with
    # updated clements dataset: "Clements_v2025-October-2025.csv"
    
    # Black-crowned Night-Heron
    # Yellow Warbler
    # Rock Pigeon (Feral Pigeon)
    # House Wren
    # Mew Gull
    # Whimbrel
    # Pacific-slope Flycatcher
    
    ### replacements (based on cornell ebird unless stated otherwise):
    
    # Black-crowned Night Heron (spelling change, removal of second "-")
    # Northern Yellow Warbler
    # Rock Pigeon (removal of second name)
    # Northern House Wren
    # Common Gull (according to https://animaldiversity.org/accounts/Larus_canus/)
    # Hudsonian Whimbrel
    # Western Flycatcher
    
    species = replace(species,
                      species == "Black-crowned Night-Heron",
                      "Black-crowned Night Heron"),
    species = replace(species,
                      species == "Yellow Warbler",
                      "Northern Yellow Warbler"),
    species = replace(species,
                      species == "Rock Pigeon (Feral Pigeon)",
                      "Rock Pigeon"),
    species = replace(species,
                      species == "House Wren",
                      "Northern House Wren"),
    species = replace(species,
                      species == "Mew Gull",
                      "Common Gull"),
    species = replace(species,
                      species == "Whimbrel",
                      "Hudsonian Whimbrel"),
    species = replace(species,
                      species == "Pacific-slope Flycatcher",
                      "Western Flycatcher")) %>% 
  
  ### filling data where applicable
  # TODO manual verification of filled data accuracy
  arrange(desc(observation_date)) %>%
  
  fill(c(water_level,
         
         observers,
         
         starting_time,
         ending_time,
         
         starting_temp_f,
         ending_temp_f,
         
         starting_percent_cloud_cover,
         ending_percent_cloud_cover,
         
         starting_cloud_height,
         ending_cloud_height,
         
         starting_wind_speed,
         ending_wind_speed,
         
         starting_wind_direction,
         ending_wind_direction,
         
         starting_rain,
         ending_rain),
       
       .by = observation_date,
       .direction = "down")


##### writing .csv and .rds ----


csvpath <- here("data","aggregated","Matt_Vinh","compiled_and_cleaned_2026-02-02.csv")
write_csv(compiled_final,csvpath)

rdspath <- here("data","aggregated","Matt_Vinh","compiled_and_cleaned_2026-02-02.rds")
saveRDS(compiled_final,rdspath)

