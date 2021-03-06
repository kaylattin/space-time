library(jagsUI)
library(tidyverse)
library(ggmcmc)

setwd("/Users/kayla/Documents/space-time")
#dat <- read.csv("wholedataset_speciesover40_DEC4.csv")
dat <- read.csv("wholedataset_speciesover40_DEC6_NODUPLICATES.csv")
#obsdat <- read.csv("observerdataset_NOV12.csv")

#dat_species <- dat %>% distinct(SpeciesCode)
#specieslist <- dat_species$SpeciesCode
#dat_obs <- obsdat[obsdat$SpeciesCode %in% specieslist ,]

#write.csv(dat_obs, "observerdataset_NOV23.csv")
#dat_obs <- read.csv("observerdataset_DEC4.csv")
dat_obs <- read.csv("observerdataset_DEC6_NODUPLICATES.csv")

obsID <- select(dat_obs, c(ObsN, Obs_ID))
obsID <- obsID %>% distinct(ObsN, Obs_ID)
dat <- merge(dat, obsID, by = "ObsN", all.x = TRUE)

### set up main analysis data
space.time <- dat$space.time # categorical
forest <- dat$Forest.cover # continuous 
count <- dat$Count # count
observer <- dat$Obs_ID # categorical
sp.region_f <- dat$SpeciesRegion # categorical

### set up observer model data
route <- dat_obs$Route_ID # categorical - index variable
species_obs_f <- dat_obs$SpeciesCode # categorical - factor
obs <- dat_obs$Obs_ID # categorical - index variable
count_obs <- dat_obs$Count 
ecozone <- dat_obs$Eco_ID # categorical - index variable


### convert to percentage
p_forest <- 0.01*forest

# convert species factor to integer
sp.region <- as.integer(as.factor(sp.region_f))
species_obs <- as.integer(as.factor(species_obs_f))

### set up n's
ncounts <- nrow(dat)
nsp.regions <- length(unique(sp.region)) # number of species
nobservers <- length(unique(observer)) # number of observers
ncounts_obs <- nrow(dat_obs) # number of observer counts
nobs <- length(unique(obs)) # number of observers
nroutes_obs <- length(unique(route)) # number of routes
nspecies_obs <- length(unique(species_obs)) # number of species in observer dataset
necozones_obs <- length(unique(ecozone)) # number of ecozones


# use function so that inits apply to both chains (or all chains)
inits <- function() {
  list(sd_noiset = 1,
       sd_beta_modt = 1)
}


###############################
#    MODEL CODE!!!            #
###############################
modl <- "
model {

######### priors & constraints ###########
sd_noiset ~ dt(0, 1, 20) T(0,) # absolute value (truncated >0 ) of student's-t centred on 0 / half-t prior on standard deviation
sd_noise <- 0.1*sd_noiset # puts 95% of sdnoise below ~0.5 on the log scale
taunoise <- pow(sd_noise, -2) # converts back to precision (inverse of variance)

sd_route ~ dt(0, 1, 4) T(0,) # 95% below ~0.1 on the log scale
tau_route <- pow(sd_route, -2)
  
sd_obs ~ dt(0, 1, 4) T(0,)
tau_obs <- pow(sd_obs, -2)
      
sd_noise_obs ~ dt(0, 1, 4) T(0,)
tau_noise_obs <- pow(sd_noise_obs, -2)
    
sd_beta_modt ~ dt(0, 1, 4) T(0,)
sd_beta_mod <- 0.5 * sd_beta_modt
tau_beta_mod <- pow(sd_beta_mod, -2)

## priors on alpha vars
#sd_species ~ dt(0, 1, 20) T(0,) 
#tau_species <- pow(sd_species, -2) # prior on precision

#sd_betat ~ dt(0, 1, 20) T(0,)
#sd_beta <- 0.5 * sd_betat
#tau_beta <- pow(sd_beta, -2)

  
######### observer model ###########
for(i in 1:ncounts_obs) {
  log(lambda_obs[i]) <- species_effect[species_obs[i]] + obs_offset[obs[i]] + route_effect[route[i]] + ecozone_effect[ecozone[i]] + noise_obs[i]
  
  noise_obs[i] ~ dnorm(0, tau_noise_obs)
  
  count_obs[i] ~ dpois(lambda_obs[i])
  }
  
  for(o in 1:nobs) {
    obs_offset[o] ~ dnorm(0,tau_obs)
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
  

######### MAIN model ###########
for(k in 1:ncounts) {
  log(lambda[k]) <- alpha[sp.region[k]] + (beta_space_time[sp.region[k],space.time[k]] * (p_forest[k] - 0.5)) + obs_offset[observer[k]] + noise[k]
  count[k] ~ dpois(lambda[k])
  
  # priors
  noise[k] ~ dnorm(0, taunoise)
  
}

for(s in 1:nsp.regions){

## prior on alpha
alpha[s] ~ dnorm(0, 0.01)

## priors on beta
beta_mod[s] ~ dnorm(0, tau_beta_mod)
beta_space_time[s,1] ~ dnorm(0,0.01) # or beta_space_time[s,1] ~ dnorm(0,tau.beta_space_time) if random effect
beta_space_time[s,2] <- beta_space_time[s,1] + beta_mod[s] # space slope == 2


}
}
"
cat(modl,file = "space_time_DATA.r")



library(rlist)
jags_dat <- list('count' = count,
                 'space.time' = space.time,
                 'p_forest' = p_forest,
                 'sp.region' = sp.region,
                 'observer' = observer,
                 'ncounts' = ncounts,
                 'nsp.regions' = nsp.regions,
                 'nobservers' = nobservers,
                 # observer
                 'count_obs' = count_obs,
                 'obs' = obs,
                 'species_obs' = species_obs,
                 'route' = route,
                 'ecozone' = ecozone,
                 'nobs' = nobs,
                 'ncounts_obs' = ncounts_obs,
                 'nspecies_obs' = nspecies_obs,
                 'necozones_obs' = necozones_obs,
                 'nroutes_obs' = nroutes_obs)


# skipping wind for now b/c it's so big
parms <- c("beta_space_time",
           "sd_noise",
           "alpha",
           "beta_mod",
           "sd_beta_mod",
           "noise",
           "sd_noise_obs",
           "obs_offset")

# re-set R memory limit to be really big -------------

memory.limit(56000)

# get posterior samples ----------------------------

x = jagsUI(data = jags_dat,
           parameters.to.save = parms,
           n.chains = 2,
           n.adapt = 2000,
           n.burnin = 20000, # discard half the iterations re: gelman 
           n.thin = 50, # keep more
           inits = inits,
           n.iter = 40000,
           parallel = T,
           modules = NULL,
           model.file = "space_time_DATA.r")

list.save(x,"fixedeffects_ND_DEC6.RData")