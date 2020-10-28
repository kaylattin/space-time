library(jagsUI)
library(tidyverse)
library(ggmcmc)

##########################
#      MAIN MODEL        #
##########################

# setting up fake data structure------------------------------------------

dat <- expand.grid(species = c("Red-eyed Vireo","American Robin","Ovenbird","Wood Thrush","Hermit Thrush","Black-capped Chickadee"),
                   sites = rep(1:8), # 8 year sites and 8 space sites for 21 regions
                   regions = rep(1:21),
                   space_time = rep(c("time","space")),
                   p_forest = seq(0.1,0.8,length.out = 13))

dat$wind <- floor(runif(8,min=1,max=5))
observer_ID <- rep(1:7)
dat$observer <- sample(observer_ID,replace=TRUE)
nobservers <- length(unique(dat$observer))

#changing the factor-level information in dat to unique integers
for(i in c("species","space_time")){
  dat[,paste0(i,"_f")] = as.integer(dat[,i])
}

nspecies = length(unique(dat$species)) #number of species
nregions = length(unique(dat$regions))


## True trends for time 
B = c(1,0,2,-2,-1.5,1,1,0,2,-2,-1.5,1,1,0,2,-2,-1.5,1,2,0,-1) #true region slopes for p_forest (change in log-scale counts for a unit change in p_forest) note: a unit change in proportional variable = change from no forest to complete forest, so these values are relatively large
B.wind = c(-1) # true wind slope

## True proportional change in slope for space i.e., here space-slopes are 90% of the value for time - on average across all species
True.mean.modifier.space <- 0.9
True.mod.B <- rnorm(nregions,True.mean.modifier.space,0.05) #random variation among regions in the modifier

#table of the True slopes for each region in time (column-1) and space (column-2)
B.space.time = matrix(c(B,True.mod.B*B),nrow = nregions,ncol = 2,byrow = FALSE) #random 1%/year variance around the region-leve mean response based on time or space
B.space.time[2,2] <- -0.8

## species abundances = True intercepts
a.sp = runif(nspecies,0.5,2) #coarse mean counts overall

a.reg = rnorm(nregions,0,0.1)#random variation in abundance among regions

#relatively small amount of overdispersion
noise <- 0.1 #introduced overdispersion


for(i in 1:nrow(dat)){
  forst.eff <- (B.space.time[dat[i,"regions"],dat[i,"space_time_f"]]*(dat[i,"p_forest"]-0.5)) #this -0.5 just centers the values of percent forest without changing the scale of the variable, it also means the intercepts represent the mean values of abundance and not some hypoethical mean count when forest == 0
  wind.eff <- (B.wind * dat[i,"wind"])
  int <- a.sp[dat[i,"species_f"]]+a.reg[dat[i,"regions"]]
  lmbd <- exp(int+forst.eff+wind.eff+rnorm(1,0,noise))
  dat[i,"count"] <- rpois(1,lambda = lmbd)
}

ncounts = nrow(dat)

#########################
#     OBSERVER MODEL    #
#########################
## set up fake data structure ----------------------
dat_start <- expand.grid(species_obs = c("Red-eyed Vireo","American Robin","Ovenbird","Wood Thrush","Hermit Thrush","Black-capped Chickadee"),
                         route = rep(1:10))

# set dataframe i can merge
route <- as.integer(c(1,2,3,4,5,6,7,8,9,10)) # common column
observer <- c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10")
ecozone <- as.integer(c(2,1,1,2,2,1,2,1,2,1)) # random ecozones for each route

# 1 == ecozone 6
# 2 == ecozone 7

dat_merge <- data.frame(route, observer, ecozone)

dat_obs <- merge(dat_start, dat_merge, by="route")

# convert all to integer
for(i in c("species_obs","observer")){
  dat_obs[,paste0(i,"_f")] = as.integer(dat_obs[,i])
}

nobs <- length(unique(dat_obs$observer_f)) # number of observers
nroutes <- length(unique(dat_obs$route)) # number of routes
nspecies_obs <- length(unique(dat_obs$species_obs)) # number of species in observer dataset
necozones <- length(unique(dat_obs$ecozone)) # number of ecozones

a.sp_eff <- runif(nspecies_obs,0.5,2) #coarse mean counts overall
a.route_eff <- rnorm(nroutes,0,0.1) #random variation
a.ecozone_eff <- rnorm(necozones,0,0.1)
a.obs_off <- runif(nobs,-1,1) # 10 observers

species_effect <- a.sp_eff[dat_obs[i,"species_obs_f"]]
ecozone_effect <- a.ecozone_eff[dat_obs[i,"ecozone"]]
route_effect <- a.route_eff[dat_obs[i,"route"]]
obs_offset <- a.obs_off[dat_obs[i,"observer_f"]]

noise <- 0.1

for(i in 1:nrow(dat_obs)) {
  int <- species_effect + obs_offset + route_effect + ecozone_effect
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
sd_noiset ~ dt(0, 1, 20) T(0,) # absolute value (truncated >0 ) of student's-t centred on 0 / half-t prior on standard deviation
sd_noise <- 0.1*sd_noiset # puts 95% of sdnoise below ~0.5 on the log scale
taunoise <- pow(sd_noise, -2) # converts back to precision (inverse of variance)

sd_route ~ dt(0, 1, 4) T(0,)
tau_route <- pow(sd_route, -2)
  
sd_obs ~ dt(0, 1, 4) T(0,)
tau_obs <- pow(sd_obs, -2)
      
sd_noise_obs ~ dt(0, 1, 4) T(0,)
tau_noise_obs <- pow(sd_noise_obs, -2)
    
sd_beta_modt ~ dt(0,1,4) T(0,)
sd_beta_mod <- 0.1 * sd_beta_modt
tau_beta_mod <- pow(sd_beta_mod, -2)

  
######### observer model ###########
for(k in 1:ncounts_obs) {
  log(lambda_obs[k]) <- species_effect[species_obs[k]] + obs_offset[obs[k]] + route_effect[route[k]] + ecozone_effect[ecozone[k]] + noise_obs[k]
  
  noise_obs[k] ~ dnorm(0, tau_noise_obs)
  
  count_obs[k] ~ dpois(lambda_obs[k])
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
  log(lambda[k]) <- alpha[region[k],species[k]] + (beta_space_time[region[k],space.time[k]] * (p_forest[k] - 0.5)) + (beta_wind[k] * wind[k]) + obs_offset[observer[k]] + noise[k]
  count[k] ~ dpois(lambda[k])
  
  # priors
  noise[k] ~ dnorm(0, taunoise)
  beta_wind[k] ~ dnorm(0,0.01)
  
}
  
for(g in 1:nregions){

## priors on alpha
  alpha_bar[g] ~ dnorm(0,1) # weakly informative prior on REGION intercept
  
  for(s in 1:nspecies){
    alpha[g,s] ~ dnorm(alpha_bar[g], tau_species[g]) # region-level intercept for species-s, centered on region-level mean
  }
  
## priors on alpha vars
  sd_speciest[g] ~ dt(0, 1, 20) T(0,) 
  sd_species[g] <- 0.1*sd_speciest[g]
  tau_species[g] <- pow(sd_species[g], -2) # prior on precision

 ## priors on beta
beta_mod[g] ~ dnorm(0, tau_beta_mod)
beta_space_time[g,1] ~ dnorm(0,0.01) # or beta_space_time[g,1] ~ dnorm(0,tau.beta_space_time) if random effect
beta_space_time[g,2] <- beta_space_time[g,1] + beta_mod[g] # space slope == 2

beta_diff[g] <- beta_space_time[g,1] - beta_space_time[g,2]
}

}
"
cat(modl,file = "space_time_withobserver.r")



jags_dat <- list('count' = dat$count,
                 'region' = dat$regions,
                 'space.time' = dat$space_time_f,
                 'p_forest' = dat$p_forest,
                 'species' = dat$species_f,
                 'wind' = dat$wind,
                 'observer' = dat$observer,
                 'ncounts' = ncounts,
                 'nspecies' = nspecies,
                 'nregions' = nregions,
                 'nobservers' = nobservers,
                 # observer
                 'count_obs' = dat_obs$count_obs,
                 'obs' = dat_obs$observer_f,
                 'species_obs' = dat_obs$species_obs_f,
                 'route' = dat_obs$route,
                 'ecozone' = dat_obs$ecozone,
                 'nobs' = nobs,
                 'ncounts_obs' = ncounts_obs,
                 'nspecies_obs' = nspecies_obs,
                 'necozones_obs' = necozones,
                 'nroutes_obs' = nroutes,
                 'necozones' = necozones)


parms <- c("beta_space_time",
           "sd_noise",
           "beta_wind",
           "sd_species",
           "alpha",
           "beta_mod",
           "sd_beta_mod",
           "beta_diff",
           "sd_noise_obs",
           "obs_offset")

# re-set R memory limit to be really big -------------

memory.limit(56000)

# get posterior samples ----------------------------

out_small = jagsUI(data = jags_dat,
             parameters.to.save = parms,
             n.chains = 3,
             n.burnin = 5000,
             n.thin = 100,
             n.iter = 105000,
             parallel = T,
             modules = NULL,
             model.file = "space_time_withobserver.r")

library(rlist)
list.save(out_small,"simulation_output_small.RData")


summary(out_small)
print(out_small)
out_small$mean$beta_space_time #posterior means of the slope parameters
out_small$mean$beta_diff

out_small$summary
# have to do them separately b/c not enough memory
out_ggs_beta_space_time = ggs(x$samples,  family = "beta_space_time")
out_ggs_beta_mod = ggs(x$samples,  family = "beta_mod")
out_ggs_beta_diff = ggs(x$samples, family = "beta_diff")
out_ggs_beta_wind = ggs(x$samples,  family = "beta_wind") # really huge

out_ggs_sd_beta_mod = ggs(x$samples, family = "sd_beta_mod")
out_ggs_sd_noise = ggs(x$samples, family = "sd_noise")
out_ggs_obs_offset = ggs(x$samples, family = "obs_offset")
out_ggs_sd_noise_obs = ggs(x$samples, family = "sd_noise_obs")

ggmcmc(out_ggs_beta_space_time,file = "beta_space_time_summary_SIM.pdf", family = "beta_space_time", param_page = 8)
ggmcmc(out_ggs_beta_mod,file = "beta_mod_summary_SIM.pdf", family = "beta_mod", param_page = 8)
ggmcmc(out_ggs_beta_diff,file = "beta_diff_summary_SIM.pdf", family = "beta_diff", param_page = 8)
# ggmcmc(out_ggs_beta_wind,file = "beta_wind_summary_SIM.pdf", family = "beta_wind", param_page = 8)

ggmcmc(out_ggs_sd_beta_mod,file = "sd_beta_mod_summary_SIM.pdf", family = "sd_beta_mod", param_page = 8)
ggmcmc(out_ggs_sd_noise,file = "sd_noise_summary_SIM.pdf", family = "sd_noise", param_page = 8)
ggmcmc(out_ggs_obs_offset,file = "obs_offset_summary_SIM.pdf", family = "obs_offset", param_page = 8)
ggmcmc(out_ggs_sd_noise_obs,file = "sd_noise_obs_summary_SIM.pdf", family = "sd_noise_obs", param_page = 8)