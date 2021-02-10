library(jagsUI)
library(tidyverse)
library(ggmcmc)
library(ggpubr)

setwd("/Users/kayla/Documents/space-time")
#div <- read.csv("wholedataset_diversity.csv")
#div_obs <- read.csv("observerdataset_diversity.csv")
div <- read.csv("wholedataset_diversity_ND.csv")
div_obs <- read.csv("observerdataset_diversity_ND.csv")


obsID <- select(div_obs, c(ObsN, Obs_ID))
obsID <- obsID %>% distinct(ObsN, Obs_ID)
div <- merge(div, obsID, by = "ObsN", all.x = TRUE)

### set up main analysis taa
space.time <- div$space.time # categorical
forest <- div$Forest.cover # continuous 
region <- as.integer(as.factor(div$Region))
diversity <- div$Hprime # count
observer <- div$Obs_ID # categorical

### set up observer model data
route <- div_obs$Route_ID # categorical - index variable
obs <- div_obs$Obs_ID # categorical - index variable
diversity_obs <- div_obs$Hprime
ecozone <- div_obs$Eco_ID # categorical - index variable


### convert to percentage
p_forest <- 0.01*forest

### set up n's
ndiversity <- nrow(div)
nobservers <- length(unique(observer)) # number of observers
nregions <- length(unique(region))
ndiversity_obs <- nrow(div_obs) # number of observer counts
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
sd_route ~ dt(0, 1, 4) T(0,) # 95% below ~0.1 on the log scale
tau_route <- 1/pow(sd_route, 2)
  
sd_obs ~ dt(0, 1, 4) T(0,)
tau_obs <- 1/pow(sd_obs, 2)
      
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
for(i in 1:ndiversity_obs) {
 diversity_obs[i] ~ dnorm(mu_obs[i], sigma_obs[i])
 
  mu_obs[i] <- obs_offset[obs[i]] + route_effect[route[i]] + ecozone_effect[ecozone[i]]
  sigma_obs[i] ~ dexp(1)
  
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
for(k in 1:ndiversity) {
  diversity[k] ~ dnorm(mu[k], sigma[k])
  mu[k] <- alpha[region[k]] + (beta_space_time[region[k],space.time[k]] * (p_forest[k] - 0.5)) + obs_offset[observer[k]]
  sigma[k] ~ dexp(1)
}

for(r in 1:nregions){

## prior on alpha
alpha[r] ~ dnorm(0,  0.01)

## priors on beta
beta_mod[r] ~ dnorm(0, tau_beta_mod)
beta_space_time[r,1] ~ dnorm(0, 0.01) 
beta_space_time[r,2] <- beta_space_time[r,1] + beta_mod[r]


}
}
"
cat(modl,file = "diversity_model.r")



library(rlist)
jags_dat <- list('diversity' = diversity,
                 'space.time' = space.time,
                 'p_forest' = p_forest,
                 'region' = region,
                 'observer' = observer,
                 'ndiversity' = ndiversity,
                 'nregions' = nregions,
                 'nobservers' = nobservers,
                 # observer
                 'diversity_obs' = diversity_obs,
                 'obs' = obs,
                 'route' = route,
                 'ecozone' = ecozone,
                 'nobs' = nobs,
                 'ndiversity_obs' = ndiversity_obs,
                 'necozones_obs' = necozones_obs,
                 'nroutes_obs' = nroutes_obs)


# skipping wind for now b/c it's so big
parms <- c("beta_space_time",
           "sd_noise",
           "alpha",
           "sd_alpha",
           "sd_beta",
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
           model.file = "diversity_model.r")

list.save(x,"fixedeffects_diversity_ND.RData")


summary(x)
print(x)
x$mean$beta_space_time #posterior means of the slope parameters
x$mean$beta_mod
x$n.eff

# diagnostics
out_ggs = ggs(x$samples)
ggmcmc(out_ggs, file = "fixedeffects_diversity_summary_ND.pdf", param_page = 8)


time <- x$mean$beta_space_time[,1]
space <- x$mean$beta_space_time[,2]
b <- data.frame(time, space)
b$region <- seq(1:14)

shannon <- ggplot(b, mapping = aes(space, time)) + 
  geom_point(
    colour = "#192e40",
    alpha = 0.7,
    size = 3
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()

shannon <- shannon + theme(legend.position = "none")
shannon

ggsave("diversity_effect_ND.png", device = "png",
       width = 12, height = 8, dpi = 300)
