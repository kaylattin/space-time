## load up stuff
setwd("/Users/kayla/Documents/space-time/final datasets")
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)

rm(list = ls())
gc()


d <- read.csv("whole_dataset_over40_D.csv")
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

# alternatively, create a 0-1 indicator array for species in certain regions
d$Species <- as.integer(as.factor(d$BBL))
sp_ind <- d %>% group_by(Region, Species) %>% select(Region, Species)
sp_ind <- sp_ind[!duplicated(sp_ind),]
sp_ind$id <- 1

sp_ind_wide <- spread(sp_ind, key = "Region", value = "id", fill = 0)
sp_ind_wide <- data.frame(sp_ind_wide) %>% select(-Species)


## set up data -------------------------------
d_slim <- list(
  ncounts = nrow(d),
  nspecies = length(unique(d$BBL)),
  nreg = length(unique(d$ref)),
  nst = 2,
  nobs = length(unique(d$ObsN)),
  nreg_s = summ$nsp,
  sp_reg_mat =  sp_ind_wide,
  
  count = d$Count,
  space = d$space,
  time = d$time,
  species = d$Species,
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

fit$save_object(file = "march5_species_abundance_nd.RDS")
fit$cmdstan_diagnose()


# quick summary
summary <- fit$cmdstan_summary(pars = c())

# create a stanfit S4 object 
stanfit <- rstan::read_stan_csv(march4_test_0$output_files())
save(stanfit, file =  "march5_species_abundance_nd.RDS")

# load up in shinystan for convergence diagnostics & posterior predictive / assumptions
shinyfit <- as.shinystan(stanfit)
launch_shinystan(shinyfit)


# posterior predictive check
y_rep <- as.matrix(stanfit, pars = "y_rep")
ppc_dens_overlay(y = d$Count, yrep = y_rep)
y <- d$Count



## 2nd stage - a priori calculation of space vs. time slopes --------------
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "B_TIME", "B_SPACE"))
b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")

x <- print(b_space$summary[1:85])
y <- print(b_time$summary[1:85])

plot(x, y)


  
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


nspecies = 17
nregions = 5
niterations = 2000

# estimate of the slope across all species in a given region
for(r in 1:nregions){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, reg_sp_mat[r,1:nspecies], r] ~ b_space[i, reg_sp_mat[r, 1:nspecies], r])
    
    intercept_by_r[i,r] = mlm$coefficients[[1]]
    slope_by_r[i,r] = mlm$coefficients[[2]]

   
  }
  
  # summarize across all iterations
  mean_intercept_by_r[r] = mean(intercept_by_r[,r])
  
  mean_slope_by_r[r] = mean(slope_by_r[,r])
  

}



# estimate of the slope across regions for a given species
for(s in 1:nspecies){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, s, sp_reg_mat[s, 1:nregions]] ~ b_space[i, s, sp_reg_mat[s, 1:nregions]])
    
    intercept_by_s[i, s] = mlm$coefficients[[1]]
    slope_by_s[i, s] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_s[s] = mean(intercept_by_s[,s])
  
  mean_slope_by_s[s] = mean(slope_by_s[,s])
  
  
}



## summarize across the 2 biomes

biome <- read.csv("forest_biome_input.csv")

# how many species and regions in eastern forests?
east_forest <- biome %>% filter(biome == 1)

d_east <- d %>% filter(Region %in% east_forest$region)

reg_east <- d_east %>% distinct(Region)
nregions_east <- reg_east %>% n_distinct(Region)

# estimate of the slope across all species in a given region
for(r in reg_east){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, reg_sp_mat[r,1:nspecies], r] ~ b_space[i, reg_sp_mat[r, 1:nspecies], r])
    
    intercept_by_r_east[i,r] = mlm$coefficients[[1]]
    slope_by_r_east[i,r] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_r_east[r] = mean(intercept_by_r_east[,r])
  
  mean_slope_by_r_east[r] = mean(slope_by_r_east[,r])
  
  
}


species_east <- d_east %>% distinct(Species)

nspecies_east <- species_east %>% n_distinct(Species)

# estimate of the slope across regions for a given species
for(s in species_east){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, s, sp_reg_mat_east[s, 1:nregions]] ~ b_space[i, s, sp_reg_mat_east[s, 1:nregions]])
    
    intercept_by_s_east[i, s] = mlm$coefficients[[1]]
    slope_by_s_east[i, s] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_s[s] = mean(intercept_by_s_east[,s])
  
  mean_slope_by_s[s] = mean(slope_by_s_east[,s])
  
  
}



# how many species and regions in northwestern mountains?
west_forest <- biome %>% filter(biome == 1)

d_west <- d %>% filter(Region %in% west_forest$region)

reg_west <- d_west %>% distinct(Region)
nregions_west <- reg_west %>% n_distinct(Region)

# estimate of the slope across all species in a given region
for(r in reg_west){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, reg_sp_mat[r,1:nspecies], r] ~ b_space[i, reg_sp_mat[r, 1:nspecies], r])
    
    intercept_by_r_west[i,r] = mlm$coefficients[[1]]
    slope_by_r_west[i,r] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_r_west[r] = mean(intercept_by_r_west[,r])
  
  mean_slope_by_r_west[r] = mean(slope_by_r_west[,r])
  
  
}


species_west <- d_west %>% distinct(Species)

nspecies_west <- species_west %>% n_distinct(Species)

# estimate of the slope across regions for a given species
for(s in species_east){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, s, sp_reg_mat_west[s, 1:nregions]] ~ b_space[i, s, sp_reg_mat_west[s, 1:nregions]])
    
    intercept_by_r_west[i, s] = mlm$coefficients[[1]]
    slope_by_r_west[i, s] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_s_west[s] = mean(intercept_by_s_west[,s])
  
  mean_slope_by_s_west[s] = mean(slope_by_s_west[,s])
  
  
}


