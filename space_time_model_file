
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
