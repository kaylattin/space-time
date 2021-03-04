## load up stuff
setwd("/Users/kayla/Documents/space-time/final datasets")
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)

rm(list = ls())
gc()


d <- read.csv("whole_dataset_over40_ND.csv")
d_obs <- read.csv("observer_dataset_over40_ND.csv")
n_distinct(d$SpeciesRegion)


## counts of species per region
summ <- d %>% group_by(BBL) %>% summarize(nsp = n_distinct(Region))


## create space and time indicators
d$space <- d$space.time
d$space[which(d$space == 1)] <- 0
d$space[which(d$space == 2)] <- 1

d$time <- d$space.time
d$time[which(d$time == 2)] <- 0
d$time[which(d$time == 1)] <- 1


### create an indicator ragged array that determines which species are present at which regions
# it is nreg x nspecies wide
sp_reg_mat <- as.matrix(read.csv("pseudo_ragged_array_nd.csv", header = F))


## set up data -------------------------------
d_slim <- list(
  ncounts = nrow(d),
  nspecies = length(unique(d$BBL)),
  nreg = length(unique(d$ref)),
  nst = 2,
  nobs = length(unique(d$ObsN)),
  nreg_s = summ$nsp,
  sp_reg_mat =  sp_reg_mat,
  
  count = d$Count,
  space = d$space,
  time = d$time,
  species = as.integer(as.factor(d$BBL)),
  reg = d$Region,
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
  chains = 3,
  iter_warmup = 1000,
  iter_sampling = 2000,
  parallel_chains = 2,
  max_treedepth = 12,
  adapt_delta = 0.99,
  show_messages = TRUE,
  init = 0.1,
  output_dir = "~/space-time/cmdstan_output_files/"
)

fit$save_object(file = "march3.RDS")
fit$cmdstan_diagnose()


# quick summary
summary <- fit$cmdstan_summary()

# create a stanfit S4 object 
stanfit <- rstan::read_stan_csv(fit$output_files())
save(stanfit, file =  "march2_stanfit.RDS")

# load up in shinystan for convergence diagnostics
shinyfit <- as.shinystan(stanfit)
launch_shinystan(shinyfit)


# posterior predictive check
y_rep <- as.matrix(stanfit, pars = "y_rep")
ppc_dens_overlay(y = d$Count, yrep = y_rep)



## 2nd stage - a priori calculation of space vs. time slopes --------------
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time"))


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


space_vs_time <- vector("list")
slope_list <- vector("list")

species = 23
nreg = 4
niter = 1000

xx <- array(numeric(), c(niter, nspecies, nreg))
yy <- array(numeric(), c(niter, nspecies, nreg))

mlm <- rep(list(list()), 23)


# create a quick index legend for referencing the slope_list
species <- rep(1:23, 4)
reg <- c(rep(1, 23), rep(2, 23), rep(3, 23), rep(4, 23))
index_legend <- data.frame(species, reg)


# extract for each species-reg combo
 for(s in 1:nspecies){
    
    for(g in 1:nreg){   
      
      xx[, s, g] = draws$b_space[, s, g]
      yy[, s, g] = draws$b_time[, s, g]
      
      
      mlm[[s]][[g]] = lm(yy[, s, g] ~ xx[, s, g])

    }
    
  }


# mlm is an nspecies list of nreg lists, so each species-reg combo has an mlm output
# calculated from the niterations of space and time
# so each mlm[[s]][[g]]$coefficients[[2]] is a slope estimate 
# distribution of slope estimates makes it possible to get the mean slope between time ~ space across all species-reg combos

