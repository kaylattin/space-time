## load up stuff
setwd("/Users/kayla/Documents/space-time/final datasets")
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)

rm(list = ls())
gc()


d <- read.csv("whole_dataset_ND_version4.csv")
d_obs <- read.csv("observer_dataset_ND_version4.csv")
n_distinct(d$BBL)
d$Species <- as.integer(as.factor(d$BBL))

## counts of species per region
summ_r <- d %>% group_by(BBL) %>% summarize(nreg = n_distinct(Region))


## counts of region for each species
summ_s <- d %>% group_by(Region) %>% summarize(nsp = n_distinct(BBL))

## create space and time indicators
d$space <- d$space.time
d$space[which(d$space == 1)] <- 0
d$space[which(d$space == 2)] <- 1

d$time <- d$space.time
d$time[which(d$time == 2)] <- 0
d$time[which(d$time == 1)] <- 1


### create an indicator ragged array that determines which species are present at which regions
# and which regions a species is found in, in both east and west (for a priori calculations)
# it is nreg x nspecies wide
sp_reg_mat <- as.matrix(read.csv("species_reg_matrix.csv", header = F))
#sp_reg_mat_east <- as.matrix(read.csv("sp_reg_mat_east.csv", header = F))
#sp_reg_mat_west <- as.matrix(read.csv("sp_reg_mat_west.csv", header = F))

reg_sp_mat <- as.matrix(read.csv("region_sp_matrix.csv", header = F))


# create a 0-1 indicator array for species in certain regions
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
  nobs = length(unique(d$ObsN)),
  nst = 2,
  
  nreg_s = summ_r$nreg,
  nsp_r = summ_s$nsp,
  sp_reg_inclusion = sp_ind_wide,
  sp_reg_mat =  sp_reg_mat,
  reg_sp_mat = reg_sp_mat,
  
  count = d$Count,
  space = d$space,
  time = d$time,
  spacetime = d$space.time,
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
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000,
  parallel_chains = 2,
  max_treedepth = 18,
  adapt_delta = 0.999,
  show_messages = TRUE,
  step_size = 0.01,
  init = 0.1,
  output_dir = "~/space-time/cmdstan_output_files/"
)


fit$save_object(file = "main_model.RDS")
fit$cmdstan_diagnose()


files <- c("~/space-time/cmdstan_output_files/thesis_model-202103111258-1-80b20a.csv",
           "~/space-time/cmdstan_output_files/thesis_model-202103111258-2-80b20a.csv",
           "~/space-time/cmdstan_output_files/thesis_model-202103111258-3-80b20a.csv")

# quick summary
summary <- fit$cmdstan_summary(pars = c())

# create a stanfit S4 object 
stanfit <- rstan::read_stan_csv(fit$output_files())
save(stanfit, file =  "main_model_mar31.RData")

y <- d$Count

# load up in shinystan for convergence diagnostics & posterior predictive / assumptions
shinyfit <- as.shinystan(stanfit)
launch_shinystan(shinyfit)


# posterior predictive check
y_rep <- as.matrix(stanfit, pars = "y_rep")
ppc_dens_overlay(y = d$Count, yrep = y_rep)
y <- d$Count

b_space_mean_rg <- summary(stanfit, pars = "b_space_mean_rg")
b_time_mean_rg <-summary(stanfit, pars = "b_time_mean_rg")

a_space

## 2nd stage - a priori calculation of space vs. time slopes --------------
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "B_TIME", "B_SPACE"))
b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")

x <- print(b_space$summary[1:1344])
y <- print(b_time$summary[1:1344])

plot(x, y)


# correlation coefficient
function (y,x){
  
  n = length(x)
  sy = sum(y)
  sx = sum(x)
  ssx = sum(x^2)
  sxy = sum(y*x)
  
  xsq = x^2
  sxsq = sx^2
  
  ysq = y^2
  sysq = sy^2
  
  r = (n*sxy - (sx * sy)) / sqrt((n*sum(xsq) - sxsq) * (n*sum(ysq) - sysq))
  
}
  
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


nspecies = 42
nregions = 32
niterations = 2000
N = 300

# estimate of the slope across all species in a given region
for(r in 1:nregions){
  pred_data = data.frame(b_space = seq(min(b_space[, 1:summ_s[r], r]), max(b_space[, 1:summ_s[r], r]), length = N))
  
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, reg_sp_mat[r, 1:summ_s[r]], r] ~ b_space[i, reg_sp_mat[r, 1:summ_s[r]], r])
    
    intercept_by_r[i,r] = mlm$coefficients[[1]]
    slope_by_r[i,r] = mlm$coefficients[[2]]
    
    pred_lines[i, r, 1:N] <- predict(mlm, newdata = pred_data)
   
  }
  
  # summarize across all iterations
  mean_intercept_by_r[r] = mean(intercept_by_r[,r])
  
  mean_slope_by_r[r] = mean(slope_by_r[,r])
  

}



# estimate of the slope across regions for a given species
for(s in 1:nspecies){
  pred_data = data.frame(b_space = seq(min(b_space[, 1:summ_r[s], s]), max(b_space[, 1:summ_r[s], s]), length = N))
  
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, s, sp_reg_mat[s, 1:summ_r[s]]] ~ b_space[i, s, sp_reg_mat[s, 1:summ_r[s]]])
    
    intercept_by_s[i, s] = mlm$coefficients[[1]]
    slope_by_s[i, s] = mlm$coefficients[[2]]
    
    pred_lines[i, s, 1:N] <- predict(mlm, newdata = pred_data)
    
    
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
    
    mlm = lm(b_time[i, reg_sp_mat[r,1:summ_s[r]], r] ~ b_space[i, reg_sp_mat[r, 1:summ_s[r]], r])
    
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
    
    mlm = lm(b_time[i, s, sp_reg_mat_east[s, 1:summ_r[s]]] ~ b_space[i, s, sp_reg_mat_east[s, 1:summ_r[s]]])
    
    intercept_by_s_east[i, s] = mlm$coefficients[[1]]
    slope_by_s_east[i, s] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_s[s] = mean(intercept_by_s_east[,s])
  
  mean_slope_by_s[s] = mean(slope_by_s_east[,s])
  
  
}



# how many species and regions in northwestern mountains?
west_forest <- biome %>% filter(biome == 2)

d_west <- d %>% filter(Region %in% west_forest$region)

reg_west <- d_west %>% distinct(Region)
nregions_west <- reg_west %>% n_distinct(Region)

# estimate of the slope across all species in a given region
for(r in reg_west){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, reg_sp_mat[r,1:summ_s[r]], r] ~ b_space[i, reg_sp_mat[r, 1:summs[r]], r])
    
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
    
    mlm = lm(b_time[i, s, sp_reg_mat_west[s, 1:summ_r[s]]] ~ b_space[i, s, sp_reg_mat_west[s, 1:summ_r[s]]])
    
    intercept_by_r_west[i, s] = mlm$coefficients[[1]]
    slope_by_r_west[i, s] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_s_west[s] = mean(intercept_by_s_west[,s])
  
  mean_slope_by_s_west[s] = mean(slope_by_s_west[,s])
  
  
}


