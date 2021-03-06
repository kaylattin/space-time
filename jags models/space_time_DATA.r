
model {

######### priors & constraints ###########
sd_noiset ~ dt(0, 1, 20) T(0,) # absolute value (truncated >0 ) of student's-t centred on 0 / half-t prior on standard deviation
sd_noise <- 0.1*sd_noiset # puts 95% of sdnoise below ~0.5 on the log scale
taunoise <- 1/pow(sd_noise, 2) # converts back to precision (inverse of variance)

sd_route ~ dt(0, 1, 4) T(0,) # 95% below ~0.1 on the log scale
tau_route <- 1/pow(sd_route, 2)
  
sd_obs ~ dt(0, 1, 4) T(0,)
tau_obs <- 1/pow(sd_obs, 2)
      
sd_noise_obs ~ dt(0, 1, 4) T(0,)
tau_noise_obs <- 1/pow(sd_noise_obs, 2)
    
sd_beta_modt ~ dt(0, 1, 4) T(0,)
sd_beta_mod <- 0.5 * sd_beta_modt
tau_beta_mod <- 1/pow(sd_beta_mod, 2)

# priors on alpha vars
#sd_alpha ~ dt(0, 1, 20) T(0,) 
#tau_alpha <- 1/pow(sd_alpha, 2) # prior on precision

#sd_betat ~ dt(0, 1, 20) T(0,)
#sd_beta <- 0.5 * sd_betat
#tau_beta <- 1/pow(sd_beta, 2)

  
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
alpha[s] ~ dnorm(0,  0.01)

## priors on beta
beta_mod[s] ~ dnorm(0, tau_beta_mod)
beta_space_time[s,1] ~ dnorm(0, 0.01) 
beta_space_time[s,2] <- beta_space_time[s,1] + beta_mod[s] # space slope == 2


}
}
