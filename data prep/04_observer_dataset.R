library(tidyverse)

setwd("~/space-time/data prep")

dat <- read.csv("whole_dataset_over40_5p - FEB 23.csv")
d <- read.csv("clean_bbs_dataset.csv")
bbl <- read.csv("bbl_codes.csv")

d <- merge(d, bbl, by = "English_Common_Name", all.x = TRUE)

obs <- distinct(dat, ObsN)
obs.list <- obs$ObsN

sp <- distinct(dat, BBL)
sp.list <- sp$BBL

dat_obs <- d %>% filter(ObsN %in% obs.list) %>% filter(BBL %in% sp.list)

dat_obs$Obs_ID <- as.integer(as.factor(dat_obs$ObsN))
dat_obs$Route_ID <- as.integer(as.factor(dat_obs$RouteNumber))
dat_obs$Eco_ID <- as.integer(as.factor(dat_obs$Ecoregion_L1Code))


d_obs <- d_obs[!is.na(d_obs$Eco_ID),]


write.csv(dat_obs, "observer_dataset_over40 - FEB 23.csv")
