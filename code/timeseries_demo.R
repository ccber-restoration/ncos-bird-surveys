
#using base R

#create time series object that matches the structure of our bird surveys (complete 8 years)
ts(1:96, frequency = 12, start = c(2017, 9))



#tidyverts family of packages: https://tidyverts.org/:


# tsibble https://tsibble.tidyverts.org/


#example:
library(dplyr)
library(tsibble)
library(nycflights13)

weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)
weather_tsbl <- as_tsibble(weather, key = origin, index = time_hour)
weather_tsbl
