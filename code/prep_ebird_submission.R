library(tidyverse)
library(readxl)


# 2026-01-22 ----

ncos_ebird_2026_01_22 <- read_xlsx("data/excel_summaries/2026/NCOS_Bird_Survey_Data_2026_01_22.xlsx")[-1, ]

# the key columns are the first one (bird species) and the last one (grand total)
# those will become "Common Name" and "Number" in the reformatted data.


# 1. Create a data frame matching eBird Record Format (ERF)
# Columns: Common Name, Genus, Species, Number, Species Comments, 
# Date, Start Time, State/Province, County, Location, 
# Distance, Area, Duration, Observation Type, Checklist Comments


# 2. Export as CSV without headers
