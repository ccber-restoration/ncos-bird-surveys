library(tidyverse)
library(here)
library(janitor)

##### existing aggregated dataset wrangling

birdsurveyfilepath <- here("data","aggregated","dryad_2017-2023",
                           "doi_10_5061_dryad_bvq83bkhz__v20240617",
                           "NCOS_Monthly_Bird_Surveys_2017-2023.csv")
birdsurveys <- read_csv(birdsurveyfilepath) # 11 variables

# cleaning substrates
# NA and <Null> substrates become Unknown
birdsurveys_filtered <- birdsurveys %>%
  filter(!is.na(Count)) # removes 6 rows with no recorded species count
birdsurveys_filtered$Substrate[birdsurveys_filtered$Substrate == "rocks"] <- "Rock"
birdsurveys_filtered$Substrate[birdsurveys_filtered$Substrate == 
                                 "Other (Enter in Observation Notes)"] <- "Other"
birdsurveys_filtered$Substrate[birdsurveys_filtered$Substrate == "<Null>"] <- "Unknown"
birdsurveys_filtered$Substrate[is.na(birdsurveys_filtered$Substrate)] <- "Unknown"

# cleaning observation date
# adding survey year and season variables
birdsurveys_filtered$obs_date2 <- substr(birdsurveys_filtered$Observation.Date,1,10) # + 1 variable
birdsurveys_filtered <- birdsurveys_filtered %>% # + 3 variables
  mutate(Observation_Date = parse_date_time(obs_date2,orders = c("mdy","ymd")),
         Observation_Date = as.Date(Observation_Date),
         Survey_Year = case_when(
           Observation_Date >= as.Date("2017-09-01") & Observation_Date <= as.Date("2018-08-31") ~ 1,
           Observation_Date >= as.Date("2018-09-01") & Observation_Date <= as.Date("2019-08-31") ~ 2,
           Observation_Date >= as.Date("2019-09-01") & Observation_Date <= as.Date("2020-08-31") ~ 3,
           Observation_Date >= as.Date("2020-09-01") & Observation_Date <= as.Date("2021-08-31") ~ 4,
           Observation_Date >= as.Date("2021-09-01") & Observation_Date <= as.Date("2022-08-31") ~ 5,
           Observation_Date >= as.Date("2022-09-01") & Observation_Date <= as.Date("2023-08-31") ~ 6),
         Season = case_when(
           month(Observation_Date) %in% c(1,2,12) ~ "Winter",
           month(Observation_Date) %in% c(3:5) ~ "Spring",
           month(Observation_Date) %in% c(6:8) ~ "Summer",
           month(Observation_Date) %in% c(9:11) ~ "Fall")) %>% 
  filter(!is.na(Observation_Date))
# total 15 variables



##### wrangling recent data

# file paths to folders organized by year containing bird survey data
csvpath23 <- here("data","2023")
csvpath24 <- here("data","2024")
csvpath25 <- here("data","2025")

# extracting files from file path locations with list.files()
csvfiles23 <- list.files(path = csvpath23,pattern = "2023_(09|10|11|12)_.*\\.csv$",
                         full.names = T)
csvfiles24 <- list.files(path = csvpath24,pattern = "\\.csv$",full.names = T)
csvfiles25 <- list.files(path = csvpath25,pattern = "\\.csv$",full.names = T)

# reading files with map_dfr(), creating column for date of bird survey and 
# editing resulting dates after manual verification of accuracy
df23 <- map_dfr(csvfiles23,~ read_csv(.x,col_types = cols(.default = "c"),
                                      locale = locale(encoding = "Latin1")) %>% 
                  mutate(
                    parsed = mdy_hms(CreationDate),
                    Observation_Date = as.Date(parsed)))
df23$Observation_Date[df23$Observation_Date == "2023-12-06"] <- "2023-12-04"

df24 <- map_dfr(csvfiles24,~ read_csv(.x,col_types = cols(.default = "c"),
                                      locale = locale(encoding = "Latin1")) %>% 
                  mutate(
                    parsed = mdy_hms(CreationDate),
                    Observation_Date = as.Date(parsed)))
df24$Observation_Date[df24$Observation_Date == "2024-08-22"] <- "2024-08-23"
df24 <- df24 %>% filter(Observation_Date != "2020-10-24")

df25 <- map_dfr(csvfiles25,~ read_csv(.x,col_types = cols(.default = "c"),
                                      locale = locale(encoding = "Latin1")) %>% 
                  mutate(
                    parsed = mdy_hms(CreationDate),
                    Observation_Date = as.Date(parsed)))
df25$Observation_Date[df25$Observation_Date == "2025-04-29"] <- "2025-04-25"

df24 <- df24 %>% select(-c(`Direction of travel (°)`,`Compass reading (°)`))

# converting NA substrate to Unknown
df23$Substrate[is.na(df23$Substrate)] <- "Unknown"
df24$Substrate[is.na(df24$Substrate)] <- "Unknown"
df25$Substrate[is.na(df25$Substrate)] <- "Unknown"

# rbind-ing bird survey data of each year and creating year, survey year and
# season variables using case_when()
intermediate <- rbind(df23,df24) # 56 variables
newaggregate <- rbind(intermediate,df25) %>% mutate( # + 3 variables
  Year = year(Observation_Date),
  Observation_Date = as.Date(Observation_Date),
  Survey_Year = case_when(
    Observation_Date >= as.Date("2023-09-01") & Observation_Date <= as.Date("2024-08-31") ~ 7,
    Observation_Date >= as.Date("2024-09-01") & Observation_Date <= as.Date("2025-08-31") ~ 8),
  Season = case_when(
    month(Observation_Date) %in% c(1,2,12) ~ "Winter",
    month(Observation_Date) %in% c(3:5) ~ "Spring",
    month(Observation_Date) %in% c(6:8) ~ "Summer",
    month(Observation_Date) %in% c(9:11) ~ "Fall"))

# cleaning resulting aggregated data frame
newaggregate$Count <- as.integer(newaggregate$Count)
newaggregate <- newaggregate %>% filter(!is.na(newaggregate$Count)) # -12 rows
newaggregate <- newaggregate[!is.na(newaggregate$Species),] # -29 rows



##### compiling aggregates

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
species_unique <- species %>% distinct() # reduces data size from 10634 to 164
newjoined <- inner_join(newAndEbird,species_unique,by = "Species")
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

