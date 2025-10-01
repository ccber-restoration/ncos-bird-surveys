library(tidyverse)
library(here)

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
                    Observation.Date = as.Date(parsed)))
df23$Observation.Date[df23$Observation.Date == "2023-12-06"] <- "2023-12-04"

df24 <- map_dfr(csvfiles24,~ read_csv(.x,col_types = cols(.default = "c"),
                                      locale = locale(encoding = "Latin1")) %>% 
                  mutate(
                    parsed = mdy_hms(CreationDate),
                    Observation.Date = as.Date(parsed)))
df24$Observation.Date[df24$Observation.Date == "2024-08-22"] <- "2024-08-23"
df24 <- df24 %>% filter(Observation.Date != "2020-10-24")

df25 <- map_dfr(csvfiles25,~ read_csv(.x,col_types = cols(.default = "c"),
                                      locale = locale(encoding = "Latin1")) %>% 
                  mutate(
                    parsed = mdy_hms(CreationDate),
                    Observation.Date = as.Date(parsed)))
df25$Observation.Date[df25$Observation.Date == "2025-04-29"] <- "2025-04-25"

# selecting only relevant variables to ensure equal number of variables for rbind
df23 <- df23 %>% select(c(Species,Count,Substrate,Observation.Date))
df24 <- df24 %>% select(c(Species,Count,Substrate,Observation.Date))
df25 <- df25 %>% select(c(Species,Count,Substrate,Observation.Date))

# rbind-ing bird survey data of each year and creating year, survey year and season variables using
# case_when()
intermediate <- rbind(df23,df24)
newaggregate <- rbind(intermediate,df25) %>% mutate(
  Year = year(Observation.Date),
  Observation.Date = as.Date(Observation.Date),
  survey_year = case_when(
    Observation.Date >= as.Date("2023-09-01") & Observation.Date <= as.Date("2024-08-31") ~ 7,
    Observation.Date >= as.Date("2024-09-01") & Observation.Date <= as.Date("2025-08-31") ~ 8),
  season = case_when(
    month(Observation.Date) %in% c(1,2,12) ~ "Winter",
    month(Observation.Date) %in% c(3:5) ~ "Spring",
    month(Observation.Date) %in% c(6:8) ~ "Summer",
    month(Observation.Date) %in% c(9:11) ~ "Fall"))

# cleaning resulting aggregated data frame
newaggregate$Count <- as.integer(newaggregate$Count)
newaggregate <- newaggregate %>% filter(!is.na(newaggregate$Count))
newaggregate <- newaggregate[!is.na(newaggregate$Species),]
