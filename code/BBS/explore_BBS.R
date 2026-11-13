library(tidyverse)
library(auk)
library(gt)

# load ebird taxonomy
data("ebird_taxonomy")

year_final <- 2025

SB_BBS <- read_csv("data/SB_BBS/SB_BBS_data_2026-08-05.csv") |> 
  mutate(date = mdy_hms(Observation_Date)) |> 
  mutate(year = year(date),
         month = month(date)) |> 
  # assign NCOS survey year
  mutate(survey_year = case_when(
    month > 8 ~ year -1,
    .default = year
  )) |> 
  mutate(survey_year_numeric = survey_year - 2017) |> 
  mutate(Common_Name = case_when(
    Common_Name == "Belding's Savannah Sparrow" ~ "Savannah Sparrow (Belding's)",
    .default = Common_Name
  ))

# TODO- check taxonomy

ncos_bbs <- SB_BBS |> 
  filter((str_detect(Locality, "NCOS")))

ncos_bbs_2025 <- ncos_bbs |> 
  filter(survey_year == 2025)

ncos_bbs_through_current_year <- ncos_bbs |> 
  filter(survey_year > 2017) |> 
  filter(survey_year <= year_final)

n_distinct(ncos_bbs_2025$Scientific_Name)
# 23 species


# create table through 2025 survey year
table_2025 <- ncos_bbs_through_current_year |> 
  group_by(year, Common_Name) |> 
  summarize(n_observations = n()) |> 
  pivot_wider(id_cols = Common_Name, names_from = year, values_from = n_observations) |> 
  left_join(ebird_taxonomy, by = join_by(Common_Name == common_name)) |> 
  # sort taxonomically
  arrange(taxonomic_order) |> 
  relocate(family_common) |> 
  # trim down columns
  select(family_common:`2025`)

table_bbs_2025 <- gt(table_2025, groupname_col = "family_common") |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |> 
  sub_missing(missing_text = "") |> 
  cols_label(
    Common_Name = "Species common name"
  )

table_bbs_2025

# save to Word doc
gtsave(table_bbs_2025, "outputs_for_annual_monitoring_reports/year_8/Table_A3.2_BBS_only.docx")

