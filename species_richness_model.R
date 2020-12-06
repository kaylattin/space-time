library(jagsUI)
library(tidyverse)
library(ggmcmc)

setwd("/Users/kayla/Documents/space-time")
sr <- read.csv("wholedataset_richness.csv")
sr_obs <- read.csv("observerdataset_richness.csv")

obsID <- select(sr_obs, c(ObsN, Obs_ID))
obsID <- obsID %>% distinct(ObsN, Obs_ID)
sr <- merge(sr, obsID, by = "ObsN", all.x = TRUE)

### set up main analysis taa
space.time <- sr$space.time # categorical
forest <- sr$Forest.cover # continuous 
region <- sr$Region
richness <- sr$Richness # count
observer <- sr$Obs_ID # categorical

### set up observer model data
route <- sr_obs$Route_ID # categorical - index variable
obs <- sr_obs$Obs_ID # categorical - index variable
richness_obs <- sr_obs$Richness
ecozone <- sr_obs$Eco_ID # categorical - index variable


### convert to percentage
p_forest <- 0.01*forest

### set up n's
nrichness <- nrow(sr)
nobservers <- length(unique(observer)) # number of observers
nregions <- length(unique(region))
nrichness_obs <- nrow(sr_obs) # number of observer counts
nobs <- length(unique(obs)) # number of observers
nroutes_obs <- length(unique(route)) # number of routes
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

#sd_betat ~ dt(0, 1, 20) T(0,)
#sd_beta <- 0.5 * sd_betat
#tau_beta <- pow(sd_beta, -2)

  
######### observer model ###########
for(i in 1:nrichness_obs) {
  log(lambda_obs[i]) <- obs_offset[obs[i]] + route_effect[route[i]] + ecozone_effect[ecozone[i]] + noise_obs[i]
  
  noise_obs[i] ~ dnorm(0, tau_noise_obs)
  
  richness_obs[i] ~ dpois(lambda_obs[i])
  }
  
  for(o in 1:nobs) {
    obs_offset[o] ~ dnorm(0,tau_obs)
  }
  
  for(r in 1:nroutes_obs) {
    route_effect[r] ~ dnorm(0, tau_route)
  }
  
  for(e in 1:necozones_obs) {
    ecozone_effect[e] ~ dnorm(0, 0.01)
  }
  

######### MAIN model ###########
for(k in 1:nrichness) {
  log(lambda[k]) <- alpha[region[k]] + (beta_space_time[region[k],space.time[k]] * (p_forest[k] - 0.5)) + obs_offset[observer[k]] + noise[k]
  richness[k] ~ dpois(lambda[k])
  
  # priors
  noise[k] ~ dnorm(0, taunoise)
  
}

for(r in 1:nregions){

## prior on alpha
alpha[r] ~ dnorm(0, 0.01)

## priors on beta
beta_mod[r] ~ dnorm(0, tau_beta_mod)
beta_space_time[r,1] ~ dnorm(0,0.01) # or beta_space_time[r,1] ~ dnorm(0,tau.beta_space_time) if random effect
beta_space_time[r,2] <- beta_space_time[r,1] + beta_mod[r] # space slope == 2


}
}
"
cat(modl,file = "species_richness.r")



library(rlist)
jags_dat <- list('richness' = richness,
                 'space.time' = space.time,
                 'p_forest' = p_forest,
                 'observer' = observer,
                 'region' = region,
                 'nrichness' = nrichness,
                 'nregions' = nregions,
                 'nobservers' = nobservers,
                 # observer
                 'richness_obs' = richness_obs,
                 'obs' = obs,
                 'route' = route,
                 'ecozone' = ecozone,
                 'nobs' = nobs,
                 'nrichness_obs' = nrichness_obs,
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
           model.file = "species_richness.r")

list.save(x,"species_richness_fixedeffects_DEC5.RData")


summary(x)
print(x)
x$mean$beta_space_time #posterior means of the slope parameters
x$mean$beta_mod
x$n.eff

out_ggs = ggs(x$samples)
ggmcmc(out_ggs, file = "species_richness_summary_DEC5.pdf", param_page = 8)


plot(x$mean$beta_space_time,)
plot(x$mean$beta_mod)
plot(x$mean$alpha)

alpha_outcome <- exp(x$mean$alpha)
plot(alpha_outcome)


load("species_richness_fixedeffects_DEC5.RData")
time <- x$mean$beta_space_time[,1]
space <- x$mean$beta_space_time[,2]
b <- data.frame(time, space)
b$region <- seq(1:20)


s_rich <- ggplot(b, mapping = aes(space, time)) + 
  geom_point(
    colour = "#EDAE49",
    alpha = 0.7,
    size = 3
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()

s_rich <- s_rich + theme(legend.position = "none")


mod <- x$mean$beta_mod
index <- seq(1:20)
mod <- data.frame(mod, index)

r_mod <-  ggplot(mod, mapping = aes(index, mod)) +
  geom_point(
    colour = "#EDAE49",
    alpha = 0.5,
    size = 3
  ) +
  labs(
    x = "SpeciesRegion Index", 
    y = "",
    size = 4
  ) +
  theme_bw()

r_mod



beta <- data.frame(x$mean$beta_space_time, x$sd$beta_space_time, x$q97.5$beta_space_time)
alpha <- data.frame(x$mean$alpha, x$sd$alpha, x$q97.5$alpha)
beta_mod <- data.frame(x$mean$beta_mod, x$sd$beta_mod, x$q97.5$beta_mod)
obs <- data.frame(x$mean$obs_offset, x$sd$obs_offset, x$q97.5$obs_offset)


write.csv(beta, "richness_beta.csv")
write.csv(alpha, "richness_alpha.csv")
write.csv(beta_mod, "richness_beta_mod.csv")
write.csv(obs, "richness_obs_offset.csv")
