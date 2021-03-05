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

### cut down dataset to test (first 4 comparison regions)
d <- d %>% filter(Region %in% c(1,2,3,4,5))
d_obs <- d_obs %>% filter(ObsN %in% d$ObsN)



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
  chains = 2,
  iter_warmup = 500,
  iter_sampling = 1000,
  parallel_chains = 2,
  max_treedepth = 12,
  adapt_delta = 0.99,
  show_messages = TRUE,
  init = 0.1,
  output_dir = "~/space-time/cmdstan_output_files/"
)

fit$save_object(file = "march4_test_0.RDS")
fit$cmdstan_diagnose()


# quick summary
summary <- fit$cmdstan_summary(pars = c())

# create a stanfit S4 object 
stanfit <- rstan::read_stan_csv(fit$output_files())
save(stanfit, file =  "march4_test0_stanfit.RDS")

# load up in shinystan for convergence diagnostics & posterior predictive / assumptions
shinyfit <- as.shinystan(stanfit)
launch_shinystan(shinyfit)


# posterior predictive check
y_rep <- as.matrix(stanfit, pars = "y_rep")
ppc_dens_overlay(y = d$Count, yrep = y_rep)



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


biome <- read.csv("forest_biome_input.csv")

space_vs_time <- vector("list")
slope_list <- vector("list")

species = 23
nreg = 4
niter = 1000


# how many species and regions in eastern forests?
east_forest <- biome %>% filter(biome == 1)

d_1 <- d %>% filter(Region %in% east_forest$region)

nspecies_1 <- d_1 %>% distinct(Species)
nreg_1 <- d_1 %>% distinct(Region)

# how many species and regions in northwestern mountains?
west_mount <- biome %>% filter(biome == 2)

d_2 <- d %>% filter(Region %in% west_mount$region)

nspecies_2 <- d_2 %>% distinct(Species)
nreg_1 <- d_2 %>% distinct(Region)


xx <- array(numeric(), c(niter, nspecies, nreg))
yy <- array(numeric(), c(niter, nspecies, nreg))


xx_1 <- array(numeric(), c(niter, nspecies_1, nreg_1))
yy_1 <- array(numeric(), c(niter, nspecies_1, nreg_1))



xx_2 <- array(numeric(), c(niter, nspecies_2, nreg_2))
yy_2 <- array(numeric(), c(niter, nspecies_2, nreg_2))


mlm <- rep(list(list()), 23)
mlm_1 <- rep(list(list()), 23)
mlm_2 <- rep(list(list()), 23)



# extract for each species-reg combo
 for(s in 1:nspecies){
    
    for(g in 1:nreg){   
      
      
      if(sp_reg_mat[s, g] == 1){
        
        # slopes for all biomes
        xx[, s, g] = draws$b_space[, s, g]
        yy[, s, g] = draws$b_time[, s, g]
        
        
        mlm[[s]][[g]] = lm(yy[, s, g] ~ xx[, s, g])
        
        
        # if region falls in EPA ecoregion 1 == eastern temperate forests
        if(biome$biome[g] == 1){
          
          xx_1[, s, g] = draws$b_space[, s, g]
          yy_1[, s, g] = draws$b_time[, s, g]
          
          
          mlm_1[[s]][[g]] = lm(yy_1[, s, g] ~ xx_1[, s, g])
          
          # if region falls into ecoregion 2 == northwestern forested mountains
        }else if(biome$biome[g] == 2){
          
          xx_2[, s, g] = draws$b_space[, s, g]
          yy_2[, s, g] = draws$b_time[, s, g]
          
          
          mlm_2[[s]][[g]] = lm(yy_1[, s, g] ~ xx_1[, s, g])
          
          
        }
        
      }


    }
    
  }


# mlm is an nspecies list of nreg lists, so each species-reg combo has an mlm output
# calculated from the niterations of space and time
# so each mlm[[s]][[g]]$coefficients[[2]] is a slope estimate 
# distribution of slope estimates makes it possible to get the mean slope between time ~ space across all species-reg combos

# filter for species-reg combos 