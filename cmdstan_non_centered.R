## load up stuff
setwd("/Users/kayla/Documents/space-time/data prep")
library(tidyverse)
library(cmdstanr)
library(bayesplot)
rm(list = ls())
gc()


d <- read.csv("whole_dataset_over40_5p - FEB 23.csv")
d_obs <- read.csv("observer_dataset_over40 - FEB 23.csv")


### cut down dataset to test (first 4 comparison regions)
d <- d %>% filter(Region %in% c(1,2,3,4))
d_obs <- d_obs %>% filter(ObsN %in% d$ObsN)



## integer for species and regions
d$species <- as.integer(as.factor(d$BBL))
d$reg_id <- as.integer(as.factor(d$ref))



## counts of species per region
summ <- d %>% group_by(BBL) %>% summarize(nsp = n_distinct(reg_id))



### create an indicator ragged array that determines which species are present at which regions
# it is nreg x nspecies wide
sp_ind <- d %>% group_by(reg_id, BBL) %>% select(reg_id, BBL)
sp_ind <- sp_ind[!duplicated(sp_ind),]
sp_ind$id <- 1

sp_ind_wide <- spread(sp_ind, key = "reg_id", value = "id", fill = 0)
sp_ind_wide <- data.frame(sp_ind_wide) %>% select(-BBL)


## set up data -------------------------------
d_slim <- list(
  ncounts = nrow(d),
  nspecies = length(unique(d$BBL)),
  nreg = length(unique(d$ref)),
  nst = 2,
  nobs = length(unique(d$ObsN)),
  nreg_s = summ$nsp,
  sp_reg_mat = sp_ind_wide,
  
  count = d$Count,
  space = d$space,
  time = d$time,
  spacetime = d$space.time,
  species = d$species, 
  reg = d$reg_id,
  pforest = as.vector(scale(d$Forest.cover)), # standardized
  obs = as.integer(as.factor(d$ObsN)),
  
  
  count_obs = d_obs$Count,
  species_obs = as.integer(as.factor(d_obs$BBL)),
  route_obs = as.integer(as.factor(d_obs$RouteNumber)),
  ecoreg_obs = as.integer(as.factor(d_obs$Ecoregion_L1Code)),
  obs_obs = as.integer(as.factor(d_obs$ObsN)),
  
  ncounts_obs = nrow(d_obs),
  nspecies_obs = length(unique(d_obs$BBL)),
  nroutes_obs = length(unique(d_obs$Route_ID)),
  necoreg_obs = length(unique(d_obs$Eco_ID)), 
  nobs_obs = length(unique(d_obs$ObsN))
  
  
)


# compile the model in cmdstan -------------------

file <- file.path("~/space-time/thesis_model.stan")
mod <- cmdstan_model(file, pedantic = TRUE)

# run the model --------------
fit <- mod$sample(
  data = d_slim,
  chains = 2,
  iter_warmup = 500,
  iter_sampling = 1000,
  parallel_chains = 2,
  max_treedepth = 12,
  adapt_delta = 0.9,
  iter = 0.1,
  show_messages = TRUE,
  output_dir = "~\space-time\cmdstan output files"
)

fit$save_object(file = "march2.RDS")
fit$cmdstan_diagnose()

# create a stan.fit object 
stanfit <- rstan::read_stan_csv(fit$output_files())

## 2nd stage - a priori calculation of space vs. time slopes --------------


# dumping code from Adam
# select for indexes that i want for each biome -> new matrices
# for each species-reg index, calculate:
bsl = function(y,x){
  n = length(x)
  sy = sum(y)
  sx = sum(x)
  ssx = sum(x^2)
  sxy = sum(y*x)
  b = (n*sxy - sx*sy)/(n*ssx - sx^2)
  return(b)
}

xx = b_space
yy = b_time
bsl(y = yy,x = xx)

## it gives the same estimate as lm
mlm = lm(yy~xx)
mlm$coefficients[[2]]


