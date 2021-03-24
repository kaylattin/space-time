library(tidyverse)
library(vegan)
library(openxlsx)
library(readxl)
setwd("~/space-time/data prep")
spatial <- read.csv("spatial_dataset_mar2021_200km.csv")
temporal <- read.csv("temporal_dataset_mar2021_200km.csv")
forestcodes <- read.csv("forestcodes.csv", header = T)
forestcodes <- forestcodes %>% dplyr::select(English_Common_Name, status_forest) %>% distinct(English_Common_Name, status_forest)
bbl <- read.csv("bbl_codes.csv") # had to make manual changes to YRWA, DEJU in Excel as well as add Sooty Grouse, Ruffed Grouse, Northern Bobwhite 
# since BBL doesn't provide codes for gallinaceous birds




# some prep - removing spatial & temporal sites that have <10 spatial matches or bbs years
# leaves me with 40 regions!
remove <- as.vector(unlist(read.delim("remove_list.txt", header = F)))
spatial <- spatial %>% filter(!ref %in% remove)

temporal <- temporal %>% filter(!ref %in% remove)

nreg <- n_distinct(temporal$ref)

d <- rbind(spatial, temporal)
d <- merge(d, bbl, by = "English_Common_Name")
d <- merge(d, forestcodes, by = "English_Common_Name")

d <- d %>% filter(status_forest == "F")

d <- d %>% filter(!Count == 0)

# remove the temporal sites that need to be excluded in order to match the ND dataset = 32 regions
# d <- d %>% filter(!ref == 14410)
# d <- d %>% filter(!ref == 53063)
# d <- d %>% filter(!ref == 69057)

n_distinct(d$BBL)
n_distinct(d$ref)

# dataset for total abundance at a transect
total <- d %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total$ObsN)

# dataset for species richness at a transect
richness <- d %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness$ObsN)

diversity <- d %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Hprime = diversity(Count, index = "shannon"))
n_distinct(diversity$ObsN)

write.csv(total, "whole_dataset_ta_mar2021_200km.csv")
write.csv(richness, "whole_dataset_richness_mar2021_200km.csv")
write.csv(diversity, "whole_dataset_diversity_mar2021_200km.csv")


trial <- d %>% 




### OBSERVER DATASET
obs <- read.csv("observer_dataset_over40_D.csv")
n_distinct(obs$ObsN)

total_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total_obs$ObsN)

# dataset for species richness at a transect
richness_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness_obs$ObsN)

diversity_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID) %>%
  summarise(Hprime = diversity(Count, index = "shannon"))
n_distinct(diversity_obs$ObsN)

write.csv(total_obs, "observer_dataset_ta.csv")
write.csv(richness_obs, "observer_dataset_richness.csv")
write.csv(diversity_obs, "observer_dataset_diversity.csv")



## no duplicates
setwd("~/space-time/final datasets")

#### MAIN DATASET
dat <- read.csv("whole_dataset_over40_ND.csv")
n_distinct(dat$ObsN)

# dataset for total abundance at a transect
total <- dat %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total$ObsN)

# dataset for species richness at a transect
richness <- dat %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness$ObsN)

diversity <- dat %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Hprime = diversity(Count, index = "shannon"))
n_distinct(diversity$ObsN)

write.csv(total, "whole_dataset_ta_ND.csv")
write.csv(richness, "whole_dataset_richness_ND.csv")
write.csv(diversity, "whole_dataset_diversity_ND.csv")



### OBSERVER DATASET
obs <- read.csv("observer_dataset_over40_ND.csv")
n_distinct(obs$ObsN)

total_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total_obs$ObsN)

# dataset for species richness at a transect
richness_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness_obs$ObsN)

diversity_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID) %>%
  summarise(Hprime = diversity(Count, index = "shannon"))
n_distinct(diversity$ObsN)

write.csv(total_obs, "observer_dataset_ta_ND.csv")
write.csv(richness_obs, "observer_dataset_richness_ND.csv")
write.csv(diversity_obs, "observer_dataset_diversity_ND.csv")


