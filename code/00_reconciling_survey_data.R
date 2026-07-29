# setup ----
library(tidyverse)
library(readxl)


## Notes on general approach:

# The protocol for summarizing and reviewing bird survey data has been to create a pivot table by observer (survey team), species, and repeat (yes), in order to obtain the "true" coutn totals

# THe problem is that these are not reflected in the raw data (which include locations)

#Goal: summarize discrepancies (in species identities and/or counts) between the summarized data and the the raw data, to avoid misuse of the raw data and to ensure that the summarized data include all of the relevant species.

# 2017 ----




# 2018 ----

# 2019 ----

# 2020 ----

# 2021 ----

# 2022 ----

# 2023 ----

pt_2023_01_24 <- read_xlsx("data/excel_summaries/2023/NCOS_Bird_Survey_Data_2023_01_24.xlsx")

raw_2023_01_24 <- read_xlsx("data/excel_summaries/2023/NCOS_Bird_Survey_Data_2023_01_24.xlsx", sheet = "NCOS_Bird_Survey_Data_2023_01_2")

# no NA values for species,
# no observation notes

pt_2023_02 <- read_xlsx("data/excel_summaries/2023/NCOS_Bird_Survey_Data_2023_02_28.xlsx")

raw_2023_02 <- read_xlsx("data/excel_summaries/2023/NCOS_Bird_Survey_Data_2023_02_28.xlsx", sheet = "NCOS_Bird_Survey_Data_2023_02_2")

#has one bittern observataion, 

# 2024 ----

# 2025 ----

# 2026 ----