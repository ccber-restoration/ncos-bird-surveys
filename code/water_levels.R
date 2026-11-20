# the purpose of this script is to plot the water level of each survey,
# with a line for each survey year

library(tidyverse)
library(janitor)
library(readxl)
library(scales)
library(cowplot)

current_year <- 8

# read in manually-compiled checklist
excel_checklist <- read_xlsx("data/ncos_survey_list.xlsx") |> 
  select(-Date) |> 
  mutate(date = make_date(year = Year, month = Month, day = Day)) |> 
  relocate(date, .after = survey_id) |> 
  # create month abbreviation column and reorder
  mutate(month_abbr = month.abb[Month]) |> 
  mutate(month_abbr = fct_relevel(month_abbr, 
                                  "Sep", 
                                  "Oct", 
                                  "Nov", 
                                  "Dec", 
                                  "Jan", 
                                  "Feb", 
                                  "Mar", 
                                  "Apr", 
                                  "May", 
                                  "Jun", 
                                  "Jul", 
                                  "Aug")) |> 
  mutate(survey_year_label = as.character(survey_year)) |> 
  filter(survey_year <= current_year) |> 
  # revise survey year labels
  mutate(survey_year_label = case_when(
    survey_year_label == "1" ~ "1 (2017-18)",
    survey_year_label == "2" ~ "2 (2018-19)",
    survey_year_label == "3" ~ "3 (2019-20)",
    survey_year_label == "4" ~ "4 (2020-21)",
    survey_year_label == "5" ~ "5 (2021-22)",
    survey_year_label == "6" ~ "6 (2022-23)",
    survey_year_label == "7" ~ "7 (2023-24)",
    survey_year_label == "8" ~ "8 (2024-25)",
  ))


# plot figure
fig_wse <- ggplot(data = excel_checklist, aes(x = month_abbr, y = water_level_ft, color = survey_year_label, group = survey_year_label)) +
  # add reference line for bottom
  geom_hline(yintercept = 2.96, linetype = "dashed") +
  geom_point() +
  geom_line() + 
  xlab("Month") +
  ylab("Water surface elevation (ft NAVD88)") +
  scale_y_continuous(breaks = breaks_width(1),
                     limits = c(2.5, NA)) +
  labs(color = "Survey year",
       title = "Water levels during bird surveys") +
  theme_cowplot() +
  # put legend within plot area
  theme(legend.position = c(0.75, 0.85))


fig_wse

# save to file ----
ggsave(filename = "figures/monitoring_reports/year_8/water_levels_2025.png",
       width = 7,
       height = 6,
       units = "in",
       bg = "white")



# read in compilation by Matt ----
mv_survey_level <- read_rds("data/Matt_Vinh/survey-level_2026-03-01.rds") |> 
  mutate(month = month(observation_date))

# plot figure
fig_mv_wse <- ggplot(data = mv_survey_level, aes(x = month, y = slough_water_level, color = survey_year, group = survey_year)) +
  geom_point() +
  geom_line() + 
  xlab("Month") +
  ylab("Water surface elevation (ft NAVD88)") +
  labs(color = "Survey year")
      
  

fig_mv_wse




# read in Dryad data ----
dryad <- read_csv("data/aggregated/dryad_2017-2023/doi_10_5061_dryad_bvq83bkhz__v20240617/NCOS_Monthly_Bird_Surveys_2017-2023.csv") |> 
  clean_names() |> 
  # assume two values are typos with misplaced decimal points
  mutate(slough_water_elevation_ft = case_when(
    slough_water_elevation_ft == 67.0 ~ 6.7,
    slough_water_elevation_ft == 55.0 ~ 5.5,
    .default = slough_water_elevation_ft
  ))


format_check <- data.frame(
  ymd = !is.na(ymd(dryad$observation_date, quiet = TRUE)),
  mdy = !is.na(mdy(dryad$observation_date, quiet = TRUE)),
  dmy = !is.na(dmy(dryad$observation_date, quiet = TRUE)),
  ymd_hms = !is.na(ymd_hms(dryad$observation_date, quiet = TRUE)),
  ymd_hm = !is.na(mdy_hm(dryad$observation_date, quiet = TRUE))
)

# Count how many rows match each format rule
colSums(format_check)

water_levels_dryad <- dryad |> 
  mutate(parsed_datetime = parse_date_time(
    observation_date, 
    orders = c("ymd", "mdy HMS", "mdy", "dmy", "ymd HM" )
  )) |> 
  mutate(date = date(parsed_datetime)) |> 
  # add year and month columns
  mutate(year = year(date),
         month = month(date)) |> 
  # assign survey year
  mutate(survey_year = case_when(
    month > 8 ~ year + 1,
    .default = year
  )) |> 
  # filter out blank values
  filter_out(is.na(slough_water_elevation_ft)) |> 
  group_by(date, year, month, survey_year) |> 
  summarize(level_max = max(slough_water_elevation_ft),
            level_min = min(slough_water_elevation_ft)) |> 
  ungroup() |> 
  #check if min is same as max
  mutate(check = level_max == level_min) |> 
  mutate(survey_year_ch = as.character(survey_year)) |> 
  mutate(survey_year_label = case_when(
    survey_year_ch == "2018" ~ "1 (2017-18)",
    survey_year_ch == "2019" ~ "2 (2018-19)",
    survey_year_ch == "2020" ~ "3 (2019-20)",
    survey_year_ch == "2021" ~ "4 (2020-21)",
    survey_year_ch == "2022" ~ "5 (2021-22)",
    survey_year_ch == "2023" ~ "6 (2022-23)",
    survey_year_ch == "2024" ~ "7 (2023-24)",
    survey_year_ch == "2025" ~ "8 (2024-25)",
  )) |> 
  mutate(month_abbr = month.abb[month]) |> 
  mutate(month_abbr = fct_relevel(month_abbr, 
                                  "Sep", 
                                  "Oct", 
                                  "Nov", 
                                  "Dec", 
                                  "Jan", 
                                  "Feb", 
                                  "Mar", 
                                  "Apr", 
                                  "May", 
                                  "Jun", 
                                  "Jul", 
                                  "Aug")) |> 
  filter_out(is.na(month_abbr))




water_levels <- read_rds("data/Matt_Vinh/compiled_and_cleaned_2026-02-18.rds") |> 
  # filter to non-NA values for water level
  filter_out(is.na(water_level)) |> 
  group_by(observation_date) |> 
  summarize(level_max = max(water_level),
            level_min = min(water_level)) 

# plot figure
fig_venoco_wse <- ggplot(data = water_levels_dryad, aes(x = month_abbr, y = level_max, color = survey_year_label, group = survey_year_label)) +
  geom_point() +
  geom_line() + 
  xlab("Month") +
  ylab("Water surface elevation (ft NAVD88)") +
  labs(color = "Survey year")


fig_venoco_wse

# check number of data points per year*month combination
chk_dups <- water_levels_dryad |> 
  group_by(survey_year_label, month_abbr) |> 
  summarize(n_dates = n()) |> 
  filter(n_dates > 1)

