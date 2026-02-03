#this script is for visualizing potential trophic links between aquatic invertebrates and birds at NCOS
# trophic links were compiled by Gabriella primarily using Birds of the World

#load packages
library(tidyverse)
library(readxl)

#packages we could use for visualizing networks:

library(foodwebr)
library(bipartite)


links_path <- "data/aquatic_diets/NCOS_aquatic_trophic_links_2026-02-03.xlsx"

trophic_links <- read_xlsx(path = links_path, sheet = "trophic links")


#next steps:

#continue filling out trophic links data
#create draft network viz based on existing data