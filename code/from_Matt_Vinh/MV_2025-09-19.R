library(tidyverse)
library(here)

# existing aggregated dataset wrangling
birdsurveyfilepath <- here("data","aggregated","dryad_2017-2023","doi_10_5061_dryad_bvq83bkhz__v20240617",
                           "NCOS_Monthly_Bird_Surveys_2017-2023.csv")
birdsurveys <- read_csv(birdsurveyfilepath)

birdsurveys_filtered <- birdsurveys %>%
  select(-c(Repeat.Observation:Slough.Water.Elevation..ft.)) %>% 
  filter(Substrate != "<Null>" & !is.na(Count))
birdsurveys_filtered$Substrate[birdsurveys_filtered$Substrate == "rocks"] <- "Rock"
birdsurveys_filtered$Substrate[birdsurveys_filtered$Substrate == 
                                 "Other (Enter in Observation Notes)"] <- "Other"

birdsurveys_filtered$obs_date2 <- substr(birdsurveys_filtered$Observation.Date,1,10)
birdsurveys_filtered <- birdsurveys_filtered %>% 
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

# selecting only relevant variables to ensure equal number of variables for rbind
df23 <- df23 %>% select(c(Species,Count,Substrate,Observation_Date))
df24 <- df24 %>% select(c(Species,Count,Substrate,Observation_Date))
df25 <- df25 %>% select(c(Species,Count,Substrate,Observation_Date))

# rbind-ing bird survey data of each year and creating year, survey year and season variables using
# case_when()
intermediate <- rbind(df23,df24)
newaggregate <- rbind(intermediate,df25) %>% mutate(
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
newaggregate <- newaggregate %>% filter(!is.na(newaggregate$Count))
newaggregate <- newaggregate[!is.na(newaggregate$Species),]

ebirdpath <- here("data","ebird_clements_checklist",
                  "ebird-Clements-v2018-integrated-checklist-August-2018.csv")
ebird <- read_csv(ebirdpath)
newAndEbird <- left_join(newaggregate,ebird,by = c("Species" = "English name")) # left join doesnt seem to work as the documentation suggests? keeping all varibles from both dataframes

species <- birdsurveys_filtered %>% select(c("Species","General.Type"))
species_unique <- species %>% distinct()
newjoined <- inner_join(newAndEbird,species_unique,by = "Species")

newjoined_select <- newjoined %>% select(c("Species","General.Type","eBird species group",
                                           "Count","Substrate","Observation_Date","Year","Survey_Year",
                                           "Season")) %>% 
  rename(General_Type = General.Type,eBird_Group = "eBird species group")
birdsurveys_select <- birdsurveys_filtered %>% select(c("Species","General.Type","eBird.Group",
                                                        "Count","Substrate","Observation_Date","Year",
                                                        "Survey_Year","Season")) %>% 
  rename(General_Type = General.Type,eBird_Group = eBird.Group)
