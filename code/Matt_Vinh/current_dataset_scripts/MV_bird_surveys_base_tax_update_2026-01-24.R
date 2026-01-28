library(here)
library(tidyverse)


##### data

datapath <- here("data","aggregated","from_Matt_Vinh","MV_updated_cleaned.rds")
data <- readRDS(datapath)


##### updating taxonomy information
# based on investigation during species-level dataset cleaning,
# the following species have no taxonomic match with
# updated clements dataset: "Clements_v2025-October-2025.csv"

# Black-crowned Night-Heron
# Yellow Warbler
# Rock Pigeon (Feral Pigeon)
# House Wren
# Mew Gull
# Whimbrel
# Pacific-slope Flycatcher

### replacements (based on cornell ebird unless stated otherwise):

# Black-crowned Night Heron (spelling change, removal of second "-")
# Northern Yellow Warbler
# Rock Pigeon (removal of second name)
# Northern House Wren
# Common Gull (according to https://animaldiversity.org/accounts/Larus_canus/)
# Hudsonian Whimbrel
# Western Flycatcher

### modification

data$species[data$species == "Black-crowned Night-Heron"] <- "Black-crowned Night Heron"
data$species[data$species == "Yellow Warbler"] <- "Northern Yellow Warbler"
data$species[data$species == "Rock Pigeon (Feral Pigeon)"] <- "Rock Pigeon"
data$species[data$species == "House Wren"] <- "Northern House Wren"
data$species[data$species == "Mew Gull"] <- "Common Gull"
data$species[data$species == "Whimbrel"] <- "Hudsonian Whimbrel"
data$species[data$species == "Pacific-slope Flycatcher"] <- "Western Flycatcher"

### writing .csv and .rds

csvpath <- here("data","aggregated","from_Matt_Vinh","MV_cleaned_2026-01-24.csv")
write_csv(data,csvpath)
rdspath <- here("data","aggregated","from_MAtt_Vinh","MV_cleaned_2026-01-24.rds")
saveRDS(data,rdspath)
