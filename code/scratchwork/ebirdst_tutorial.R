# FHJ, 2026-08-07, following ebirdst documentation website


# Setup ---

# load other packages
library(dplyr)
library(fields)
library(rnaturalearth)
# library(rnaturalearthhires) # to store high resolution data from the rnaturalearth package (not on CRAN, only frim GitHub)
library(sf)
library(terra)

# load eBird Status and Trends R package
library(ebirdst)

# API setup

# See https://ebird.github.io/ebirdst/index.html#data-access

# note that you need to run the following code to save your API key as an environment variable
# set_ebirdst_access_key("XXXXX")


# view all available data sets
# data()

# load ebird_st_runs data frame, which lists species coverage
data("ebirdst_runs", package = "ebirdst")

# download all data products for the example species
# this was needed in previous versions of ebirdst, but not anymore
# ebirdst_download_status(species = "yebsap-example", download_all = TRUE)

# check default download location
ebirdst_data_dir()

# on FHJ's machine:
# "/Users/ccber_work/Library/Application Support/org.R-project.R/R/ebirdst"


# Trends ----

# See this article: https://ebird.github.io/ebirdst/articles/trends.html

trends_runs <- ebirdst_runs |>
  filter(has_trends) |>
  select(
    # note that trends_runs does not have a scientific name variable
    # so species will need to be selected based on common name or code
    species_code, common_name, 
    trends_season, trends_region,
    trends_start_year, trends_end_year,
    trends_start_date, trends_end_date,
    rsquared, beta0, trends_version_year
  )

glimpse(trends_runs)

# example with mourning dove

trends_moudov <- load_trends("Mourning Dove")

trends_sagthr <- load_trends("Sage Thrasher")

# getting this error: 

# Error in .rs.downloadFile(url = files$src_path[i], destfile = files$dest_path[i],  : 
# cannot open URL 'https://st-download.ebird.org/v1/fetch?objKey=2022/sagthr/trends/sagthr_breeding_ebird-trends_2022.parquet&key=fm1gtsrbhf07'

# Also getting this warning:
# Warning message:
#   In .rs.downloadFile(url = files$src_path[i], destfile = files$dest_path[i],  :
#                         cannot open URL 'https://st-download.ebird.org/v1/fetch?objKey=2022/sagthr/trends/sagthr_breeding_ebird-trends_2022.parquet&key=fm1gtsrbhf07': HTTP status was '500 Internal Server Error'

# Possibly caused by UCSB firewall?

# TODO- try from home network


