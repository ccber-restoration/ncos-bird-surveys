library(tidyverse)
library(here)



birdsurveyfilepath <- here("data","aggregated","dryad_2017-2023","doi_10_5061_dryad_bvq83bkhz__v20240617","NCOS_Monthly_Bird_Surveys_2017-2023.csv")

birdsurveys <- read_csv(birdsurveyfilepath)

#check what species are in the omnivore category
omnivores <- birdsurveys %>% 
  filter(General.Type == "Omnivores")

unique(omnivores$Species)  


n_distinct(birdsurveys$Substrate)

unique(birdsurveys$Substrate)

#birdsurveys <- readSubstratebirdsurveys <- read_csv(birdsurveyfilepath) %>% 
  filter(Substrate == "Habitat Feature - Hibernacula"  )
 
bird_surveys_wide <- birdsurveys %>% 
  group_by(Observation.Date, Species) %>% 
  summarize(totalcount = sum(Count)) %>% 
  pivot_wider(names_from = Species, values_from = totalcount, values_fill = 0) %>% 
  #need to make observation date into rownames
  #column_to_rownames()
  as.matrix()

library(vegan) 

#example NMDS call
#distance = "bray" is default
bird_nmds_test <- metaMDS(bird_surveys_wide)  
  


