library(tidyverse)
library(gt)
library(auk)

# run BBS script ----
source("code/BBS/explore_BBS.R")

data("ebird_taxonomy")

compiled_2025 <- read_rds("data/Matt_Vinh/compiled_and_cleaned_2026-02-18.rds")

# filter to breeding records
compiled_2025_breeding <- compiled_2025 |> 
  filter_out(is.na(breeding_activity)) |> 
  # change Selasphorus sp. to a taxon matching ebird taxonomy
  mutate(species = case_when(
    species == "Selasphorus sp." ~ "Rufous/Allen's Hummingbird",
    species == "Western Snowy Plover" ~ "Snowy Plover",
    # change orphaned ducklings of unknown species to Gadwall 
    # (as recorded by Conor and someone else the day before: https://ebird.org/checklist/S176277265)
    species == "Unknown" & year == 2024 ~ "Gadwall",
    .default = species
  )) |> 
  mutate(survey_year = survey_year + 2017) |> 
  group_by(species, year, survey_year) |> 
  summarize(n_records = n()) |> 
  ungroup() |> 
  arrange(year)


# create table through 2025 survey year
table_2025_monthly_breeding <- compiled_2025_breeding |> 
  pivot_wider(id_cols = species, names_from = year, values_from = n_records) |> 
  left_join(ebird_taxonomy, by = join_by(species == common_name)) |> 
  # sort taxonomically
  arrange(taxonomic_order) |> 
  relocate(family_common) |> 
  # trim down columns
  select(family_common:`2025`)

# format as gt table
table_monthly_breeding_2025 <- gt(table_2025_monthly_breeding, groupname_col = "family_common") |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |> 
  sub_missing(missing_text = "") |> 
  cols_label(
    species = "Species common name"
  )

table_monthly_breeding_2025

# save to Word doc
gtsave(table_monthly_breeding_2025, "outputs_for_annual_monitoring_reports/year_8/Table_A3.2_monthly_only.docx")

# alphabetical version
table_2025_monthly_alphabetical <- compiled_2025_breeding |> 
  pivot_wider(id_cols = species, names_from = year, values_from = n_records) |> 
  arrange(species)


# format as gt table
table_monthly_breeding_2025_alphabetical <- gt(table_2025_monthly_alphabetical) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
    ) |> 
  sub_missing(missing_text = "") |> 
  cols_label(
    species = "Species common name"
  )

table_monthly_breeding_2025_alphabetical

# save to Word doc
gtsave(table_monthly_breeding_2025_alphabetical, "outputs_for_annual_monitoring_reports/year_8/Table_A3.2_monthly_alphabetical.docx")


# combine data from both sources ----
compiled_bbs <- ncos_bbs_through_current_year |> 
  group_by(Common_Name, survey_year) |> 
  summarize(n_records = n()) |> 
  ungroup() |> 
  rename(species = Common_Name)


breeding_records_combined <- bind_rows(compiled_2025_breeding, compiled_bbs)

# resummarize
breeding_records_summarized <- breeding_records_combined |> 
  group_by(species, survey_year) |> 
  summarize(n_observations = sum(n_records, na.rm = TRUE)) |> 
  arrange(survey_year, species ) |> 
  mutate(observed = case_when(
    n_observations >=1 ~ "B",
    .default = NA
  ))

# create table through 2025 survey year
table_2025_breeding <- breeding_records_summarized |> 
  pivot_wider(id_cols = species, names_from = survey_year, values_from = observed) |> 
  left_join(ebird_taxonomy, by = join_by(species == common_name)) |> 
  # sort taxonomically
  arrange(taxonomic_order) |> 
  relocate(family_common) |> 
  # trim down columns
  select(family_common:`2025`)

# format as gt table
table_breeding_combined <- gt(table_2025_breeding, groupname_col = "family_common") |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |> 
  sub_missing(missing_text = "") |> 
  cols_label(
    species = "Species common name"
  ) |> 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = `2018`,
      rows = `2018` == "B"
    )
  ) |> 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = `2019`,
      rows = `2019` == "B"
    )) |> 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = `2020`,
      rows = `2020` == "B"
    )) |> 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = `2021`,
      rows = `2021` == "B"
    )) |> 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = `2022`,
      rows = `2022` == "B"
    )) |> 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = `2023`,
      rows = `2023` == "B"
    )) |> 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = `2024`,
      rows = `2024` == "B"
    )) |> 
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = `2025`,
      rows = `2025` == "B"
    ))

table_breeding_combined

# save to Word doc
gtsave(table_breeding_combined, "outputs_for_annual_monitoring_reports/year_8/Table_A3.3_breeding_evidence_combined.docx")
