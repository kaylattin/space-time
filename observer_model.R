library(jagsUI)
library(tidyverse)
library(ggmcmc)
library(rjags)


#########################
#     OBSERVER MODEL    #
#########################
## set up fake data structure ----------------------
dat_start <- expand.grid(species_obs = c("Red-eyed Vireo","American Robin","Ovenbird","Wood Thrush","Hermit Thrush","Black-capped Chickadee"),
                         route = rep(1:10))

# set dataframe i can merge
route <- as.integer(c(1,2,3,4,5,6,7,8,9,10)) # common column
observer <- c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10")
ecozone <-as.integer(c(14,6,7,13,14,14,9,6,7,13)) # random ecozones for each route

dat_merge <- data.frame(route, observer, ecozone)

dat_obs <- merge(dat_start, dat_merge, by="route")

# convert all to integer
for(i in c("species_obs","observer")){
  dat_obs[,paste0(i,"_f")] = as.integer(dat_obs[,i])
}

nobs <- length(unique(dat_obs$observer_f)) # number of observers
nroutes <- length(unique(dat_obs$route)) # number of routes
nspecies_obs <- length(unique(dat_obs$species_obs)) # number of species in observer dataset
necozones <- length(dat_obs$ecozone) # number of ecozones

a.sp_eff = runif(nspecies_obs,0.5,2) #coarse mean counts overall
a.route_eff = rnorm(nroutes,0,0.1) #random variation
a.ecozone_eff = rnorm(necozones,0,0.1)
a.obs_off <- runif(nobs,-1,1) # 10 observers

species_effect <- a.sp_eff[dat_obs[i,"species_obs_f"]]
ecozone_effect <- a.ecozone_eff[dat_obs[i,"ecozone"]]
route_effect <- a.route_eff[dat_obs[i,"route"]]
obs_offset <- a.obs_off[dat_obs[i,"observer_f"]]

for(i in 1:nrow(dat_obs)) {
  int <- obs_offset + route_effect + species_effect + ecozone_effect
  lmbd_obs <- exp(int+rnorm(1,0,noise))
  dat_obs[i,"count_obs"] <- rpois(1, lambda = lmbd_obs)
}

ncounts_obs <- nrow(dat_obs) # number of observer counts

###############################
#    MODEL CODE!!!            #
###############################
modl <- "
model {

######### priors & constraints ###########
sd_route ~ dt(0, 1, 4) T(0,)
tau_route <- pow(sd_route, -2)
  
sd_obs ~ dt(0, 1, 4) T(0,)
tau_obs <- pow(sd_obs, -2)
      
sd_noise_obs ~ dt(0, 1, 4) T(0,)
tau_noise_obs <- pow(sd_noise_obs, -2)
    
  #### observer model
for(k in 1:ncounts_obs) {
  log(lambda_obs[k]) <- alpha_obs[k] + species_effect[species_obs[k]] + obs_effect[obs[k]] + route_effect[route[k]] + ecozone_effect[ecozone[k]] + noise_obs[k]
  
  alpha_obs[k] ~ dnorm(0,0.01) # intercept
  noise_obs[k] ~ dnorm(0, tau_noise_obs)
  
  count_obs[k] ~ dpois(lambda[k])
  }
  
  for(o in 1:nobs) {
    obs_effect[o] ~ dnorm(0,tau_obs)
  }
  
  for(r in 1:nroutes_obs) {
    route_effect[r] ~ dnorm(0, tau_route)
  }
  
  for(s in 1:nspecies_obs) {
    species_effect[s] ~ dnorm(0, 0.01)
  }
  
  for(e in 1:necozones_obs) {
    ecozone_effect[e] ~ dnorm(0, 0.01)
  }

}
"
cat(modl,file = "observer_model.r")



jags_dat <- list('count_obs' = dat_obs$count_obs,
                 'obs' = dat_obs$obs,
                 'species_obs' = dat_obs$species_obs,
                 'route' = dat_obs$route,
                 'ecozone' = dat_obs$ecozone,
                 'nobs' = nobs,
                 'ncounts_obs' = ncounts_obs,
                 'nspecies_obs' = nspecies_obs,
                 'nroutes' = nroutes,
                 'necozones' = necozones)



parms <- c( "sd_noise_obs",
            "sd_route",
            "sd_obs",
            "obs_offset",
            "route_effect",
            "species_effect",
            "ecozone_effect")

burnInSteps = 5000            # Number of steps to "burn-in" the samplers. 
nChains = 4                  # Number of chains to run.
numSavedSteps=4000         # Total number of steps in each chain to save. 
thinSteps=10                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.

# get posterior samples in mcmc list format ----------------------------
out = jagsUI(data = jags_dat,
             parameters.to.save = parms,
             n.chains = 3,
             n.burnin = burnInSteps,
             n.thin = thinSteps,
             n.iter = nIter,
             parallel = T,
             modules = NULL,
             model.file = "thesis_mod_withoutobserver.r")


summary(out)
print(out)
out$mean$beta_time_space #posterior means of the slope parameters
