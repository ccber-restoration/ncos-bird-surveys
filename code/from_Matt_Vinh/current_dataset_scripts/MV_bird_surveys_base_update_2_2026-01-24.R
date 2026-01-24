library(here)
library(tidyverse)


##### data

datapath <- here("data","aggregated","from_Matt_Vinh","MV_cleaned_2026-01-24.rds")
data <- readRDS(datapath)



##### filling in data where applicable
# water level, observers, starting_time, ending_time, starting_temp_f, 
# starting_percent_cloud_cover, starting_cloud_height, starting_wind_speed, 
# starting_wind_direction, starting_rain, ending_temp_f, ending_percent_cloud_cover, 
# ending_cloud_height, ending_wind_speed, ending_wind_direction, ending_rain, 

data <- data %>% arrange(desc(observation_date)) %>% 
  fill(c(water_level,observers,starting_time,ending_time,starting_temp_f,
         ending_temp_f,starting_percent_cloud_cover,ending_percent_cloud_cover,
         starting_cloud_height,ending_cloud_height,starting_wind_speed,
         ending_wind_speed,starting_wind_direction,ending_wind_direction,
         starting_rain,ending_rain),
       .by = observation_date,
       .direction = "up")


##### writing .csv and .rds

csvpath <- here("data","aggregated","from_Matt_Vinh","MV_cleaned_2026-01-24.csv")
write_csv(data,csvpath)
rdspath <- here("data","aggregated","from_MAtt_Vinh","MV_cleaned_2026-01-24.rds")
saveRDS(data,rdspath)
