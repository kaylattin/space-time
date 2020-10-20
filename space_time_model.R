library(jagsUI)
library(tidyverse)
library(ggmcmc)

##########################
#      MAIN MODEL        #
##########################

# setting up fake data structure------------------------------------------

dat <- expand.grid(species = c("Red-eyed Vireo","American Robin","Ovenbird","Wood Thrush","Hermit Thrush","Black-capped Chickadee"),
                   sites = rep(1:8), # 6 year sites and 6 space sites for 21 regions
                   regions = rep(1:21),
                   space_time = rep(c("time","space")),
                   p_forest = seq(0.1,0.8,length.out = 13))

dat$wind= floor(runif(8,min=1,max=5))
observer_ID <- rep(1:7)
dat$observer <- sample(observer_ID,replace=TRUE)
nobservers <- length(unique(dat$observer))

#changing the factor-level information in dat to unique integers
#this is required for JAGS, it can't handle factors or character variables, only numeric and integer
# so for example, this changes "Red-eyed Vireo" to 1, "American Robin" to 2, etc.
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

###############################
#    MODEL CODE!!!            #
###############################
modl <- "
model {

######### priors & constraints ###########
sd_noiset ~ dt(0, 1, 20) T(0,) # absolute value (truncated >0 ) of student's-t centred on 0 / half-t prior on standard deviation
sd_noise <- 0.1*sd_noiset # puts 95% of sdnoise below ~0.5 on the log scale
taunoise <- pow(sd_noise, -2) # converts back to precision (inverse of variance)

sd_beta_modt ~ dt(0,1,4) T(0,)
sd_beta_mod <- 0.1 * sd_beta_modt
tau_beta_mod <- pow(sd_beta_mod, -2)


######### main model ###########
for(k in 1:ncounts) {
  log(lambda[k]) <- alpha[region[k],species[k]] + (beta_space_time[region[k],space.time[k]] * (p_forest[k] - 0.5)) + (beta_wind[k] * wind[k]) + noise[k]
  count[k] ~ dpois(lambda[k])
  
  # priors
  noise[k] ~ dnorm(0, taunoise)
  beta_wind[k] ~ dnorm(0,0.01)
  
}


for(g in 1:nregions){

  # priors on alpha mu
  alpha_bar[g] ~ dnorm(0,1) # weakly informative prior on REGION intercept
  
  # priors on alpha vars
  sd_speciest[g] ~ dt(0, 1, 20) T(0,) 
  sd_species[g] <- 0.1*sd_speciest[g]
  tau_species[g] <- pow(sd_species[g], -2) # prior on precision
  
  for(s in 1:nspecies){
    alpha[g,s] ~ dnorm(alpha_bar[g], tau_species[g]) # region-level intercept for species-s, centered on region-level mean
  }


# priors on beta
beta_mod[g] ~ dnorm(0, tau_beta_mod)
beta_space_time[g,1] ~ dnorm(0,0.01) # or beta_space_time[g,1] ~ dnorm(0,tau.beta_space_time) if random effect
beta_space_time[g,2] <- beta_space_time[g,1] + beta_mod[g] # space slope == 2

beta_diff[g] <- beta_space_time[g,1] - beta_space_time[g,2]
}

}
"
cat(modl,file = "space_time_mainfile.r")



jags_dat <- list('count' = dat$count,
                 'region' = dat$regions,
                 'space.time' = dat$space_time_f,
                 'p_forest' = dat$p_forest,
                 'species' = dat$species_f,
                 'wind' = dat$wind,
                 'ncounts' = ncounts,
                 'nspecies' = nspecies,
                 'nregions' = nregions,
                 'nobservers' = nobservers)


parms <- c("beta_space_time",
           "sigma.noise",
           "beta_wind",
           "alpha_bar",
           "sd_species",
           "alpha",
           "beta_mod",
           "sd_beta_mod",
           "beta_diff")

burnInSteps = 5000            # Number of steps to "burn-in" the samplers. 
nChains = 4                  # Number of chains to run.
numSavedSteps=10000         # Total number of steps in each chain to save. 
thinSteps=10                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.


# re-set R memory limit to be really big -------------
memory.limit(56000)


# get posterior samples ----------------------------
# (10000 - 5000)/20 = 250
# 250 * 4 chains = 1000 values
out = jagsUI(data = jags_dat,
             parameters.to.save = parms,
             n.chains = 4,
             n.burnin = burnInSteps,
             n.thin = thinSteps,
             n.iter = nIter,
             parallel = T,
             modules = NULL,
             model.file = "space_time_mainfile.r")


summary(out)
print(out)
out$mean$beta_space_time #posterior means of the slope parameters
