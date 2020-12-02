setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)

dat <- read.csv("wholedataset_D_NOV12.csv")

canada_d <- read.csv("complete_canada_dataset.csv")

obs <- dat %>% distinct(ObsN)
obs.list <- obs$ObsN

dat_obs <- canada_d[canada_d$ObsN %in% obs.list ,]
observers <- dat_obs %>% distinct(ObsN)

dat_obs$Obs_ID <- as.integer(as.factor(dat_obs$ObsN))
dat_obs$Route_ID <- as.integer(as.factor(dat_obs$Route))
dat_obs$Eco_ID <- as.integer(as.factor(dat_obs$ECOZONE))

write.csv(observers, "observerdataset_NOV12.csv")
