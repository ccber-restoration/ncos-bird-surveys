library(here)
library(tidyverse)
library(patchwork)
library(grid)

datapath <- here("data","aggregated","MV_updated_aggregated.rds")
data <- readRDS(datapath)



### S x T matrix construction

year_abundance <- data %>% group_by(survey_year,species) %>% 
  summarise(abundance = sum(count))

#this works, but could also use pivot_wider()

community <- xtabs(abundance ~ survey_year + species,data = year_abundance)

#convert to proper matrix
community <- as.matrix(unclass(community))

#transpose
community <- t(community)



### calculating bootstrapping variables

# m_i 
#"The total number of individuals of species i in all sampling periods"

# m_i
null <- data.frame(m_i = 1:169)
for (i in 1:169){
  x = 0
  for (j in 1:8){
    x = x + community[i,j]
  }
  null$m_i[i] = x
}

# N= "total number of individuals summed across all species and samples"

N <- 0
for (i in 1:169){
  N = N + null$m_i[i]
}
N <- ceiling(N)
# 2696 individuals

# s_i
# We define the relative abundance of species i in the source pool of N individuals as

for (i in 1:169){
  null$s_i[i] = null$m_i[i] / N
}

null2 <- data.frame(n_j = 1:8,q_j = 1:8)
for (i in 1:8){
  null2$n_j[i] = sum(community[,i])
  null2$q_j[i] = null2$n_j[i] / N
}



### estimating undetected species

# Chao diversity indices (1 & 2) can also be directly calculated using functions in packages
#vegan and iNEXT

number_occurences <- data.frame(species_number_surveys_detected = 1:169)
for (i in 1:169){
  x = 0
  for (j in 1:8){
    if (community[i,j] > 0){
      x = x + 1
    }
  }
  number_occurences$species_number_surveys_detected[i] = x
}
q1_numbers <- which(number_occurences$species_number_surveys_detected == 1)
q1 <- length(q1_numbers)
q2_numbers <- which(number_occurences$species_number_surveys_detected == 2)
q2 <- length(q2_numbers)



#estimated undetected species...

S_bar <- (7 / 8) * ((q1 * (q1 - 1)) / (2 * (q2 + 1))) # 8.5, rounds to 9

# accounting for undetected species

undetected <- data.frame(m_i = ceiling(S_bar) * NA,s_i = rep(0.5 * min(null$s_i),ceiling(S_bar)))

null <- rbind(null,undetected) # adding 9 repetitions of 0.5 * minimum observed s_i

names <- append(rownames(community),paste0("undetected",1:ceiling(S_bar)))



### simulation

set.seed(234)
nsim <- 1000
TC_list <- 1:nsim

for (i in 1:nsim){
  
  # generating null model
  null <- data.frame(sample_group = sample(1:8,N,prob = null2$q_j,replace = T),
                          sample_species = sample(names,N,prob = null$s_i,replace = T))
  
  # calculating coefficients
  simulated <- null %>% group_by(sample_group,sample_species) %>% 
    tally %>% distinct(sample_group,sample_species,n)
  
  species_simulated <- unique(simulated$sample_species)
  beta1_simulated <- 1:length(species_simulated)
  for (j in 1:length(species_simulated)){
    x <- lm(n ~ sample_group,
            data = simulated[simulated$sample_species == species_simulated[j],])
    beta1_simulated[j] <- as.numeric(x$coefficients[2])
  }
  beta1_simulated <- beta1_simulated[!is.na(beta1_simulated)]
  
  # calculating TC
  intermediate <- 0
  beta1bar_simulated <- sum(beta1_simulated)
  for (j in 1:length(beta1_simulated)){
    x <- (beta1_simulated[j] - beta1bar_simulated)^2
    intermediate = intermediate + x
  }
  TC_simulated <- (1 / (length(species_simulated) - 1)) * intermediate
  
  # recording TC
  TC_list[i] <- TC_simulated
  
}

TC_data <- data.frame(TC = TC_list)

TC_observed <- 29821

TC_data_desc <- TC_data %>% arrange(desc(TC))
percentile_95 <- TC_data_desc$TC[51]

p_value <- mean(TC_list >= TC_observed)

text1 <- "Observed TC"
text2 <- "95th percentile"
text3 <- "p-value: 0.057"

hist <- ggplot(TC_data,aes(x = TC)) + geom_histogram(alpha = 0.9) + 
  geom_vline(xintercept = TC_observed,color = "red") + 
  geom_text(aes(label = text1),
            x = 34000,y = 75,
            color = "red",
            size = 5) + 
  geom_vline(xintercept = percentile_95,color = "blue") + 
  geom_text(aes(label = text2),
            x = 34000,y = 69,
            color = "blue",
            size = 5) + 
  geom_text(aes(label = text3),
            x = 17500,y = 75,
            size = 5) + 
  theme_light() + 
  labs(title = "Distribution of Simulated TC Values",
       x = "Temporal Change (TC)",
       y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5))

hist

ggsave(filename = paste0("Simulated_TC_Histogram",format(Sys.Date(),"%y-%m-%d"),
                         ".pdf"),hist,width = 200,height = 200,units = "mm")
