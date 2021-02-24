## load up stuff
setwd("/Users/kayla/Documents/space-time/data prep")
library(tidyverse)
library(rstan)
library(bayesplot)
rm(list = ls())
gc()


d <- read.csv("whole_dataset_over40_5p.csv")
d_obs <- read.csv("observer_dataset_over40.csv")


### cut down dataset to test (first 4 comparison regions)
d <- d %>% filter(Region == c(1,2,3,4))
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
  st = d$space.time,
  species = d$species, 
  reg = d$reg_id,
  pforest = (d$Forest.cover - mean(d$Forest.cover)), # centered
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

# compile the model --------------------------
model <- stan_model(file = "thesis_model.stan")
gc()

# fit the model ------------------------------
stan.fit <- sampling(object = model,
                     data = d_slim,
                     iter = 2000,
                     pars = c("noise", "noise_obs"),
                     include = FALSE,
                     chains = 3,
                     cores = 3,
                     init = 'random',
                     show_messages = TRUE,
                     control = list(max_treedepth = 15,
                                    adapt_delta = 0.99))

save(stan.fit, file = ".RData")


# prior predictive check ------------------------------------
y_rep <- as.matrix(stan.fit, pars = "y_rep")
ppc_dens_overlay(y = d$Count, yrep = y_rep)


## summary statistics ---------------------------------------
summary <- print(summary(stan.fit))



## 2nd stage - a priori calculation of space vs. time slopes --------------


# dumping code from Adam
bsl = function(y,x){
  n = length(x)
  sy = sum(y)
  sx = sum(x)
  ssx = sum(x^2)
  sxy = sum(y*x)
  b = (n*sxy - sx*sy)/(n*ssx - sx^2)
  return(b)
}

xx = 1:21
yy = (xx-mean(xx))*0.2 + rnorm(length(xx),0,1)
bsl(y = yy,x = xx)

## it gives the same estimate as lm
mlm = lm(yy~xx)
mlm$coefficients[[2]]



