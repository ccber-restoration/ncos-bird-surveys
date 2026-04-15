library(tidyverse)
library(readxl)


# 2026-01-22 ----

ncos_ebird_2026_01_22 <- read_xlsx("data/excel_summaries/2026/NCOS_Bird_Survey_Data_2026_01_22.xlsx")[-1, ]


# 1. Create a data frame matching eBird Record Format (ERF)
# Columns: Common Name, Genus, Species, Number, Species Comments, 
# Date, Start Time, State/Province, County, Location, 
# Distance, Area, Duration, Observation Type, Checklist Comments
ebird_data <- data.frame(
  Common_Name = c("Northern Cardinal", "Blue Jay"),
  Genus = "", 
  Species = "",
  Number = c("2", "1"),
  Comments = c("Male and female", ""),
  Date = c("04/01/2026", "04/01/2026"),
  Start_Time = c("08:00 AM", "08:15 AM"),
  State = "US-CA",
  County = "Santa Barbara",
  Location = "My Backyard",
  Distance = "", 
  Area = "",
  Duration = "15",
  Type = "S", # S for Stationary, P for Protocol/Traveling
  Checklist_Comments = "Clear day"
)

# 2. Export as CSV without headers
write.table(ebird_data, "ebird_upload.csv", 
            sep = ",", 
            row.names = FALSE, 
            col.names = FALSE, # MANDATORY: Remove headers for eBird
            na = "", 
            quote = TRUE)