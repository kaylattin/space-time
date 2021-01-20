## mock rstan code as if I could ever figure it out to run it properly
library(rstan)
setwd("/Users/kayla/Documents/space-time")
dat <- read.csv("wholedataset_speciesover40_NOV18.csv")

dat_obs <- read.csv("observerdataset_NOV23.csv")

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


### VERY SIMPLIFIED TEST MODEL WITHOUT OBSERVER SUBMODEL AND BETA_MOD

dat_slim <- list(
  ncounts = nrow(dat),
  nspregions = length(unique(sp.region)),
  nspacetime = length(unique(dat$space.time)),
  count = dat$Count,
  spacetime = dat$space.time,
  spregion = sp.region,
  pforest = dat$Forest.cover
)

code <- " data {
  int<lower=0> ncounts;
  int<lower=0> nspregions;
  int<lower=0> nspacetime;
  vector[ncounts] pforest;
  int count[ncounts];
  int spacetime[ncounts];
  int spregion[ncounts];

}
parameters {
  vector[nspregions] alpha;
  vector[ncounts] noise;
  matrix[nspregions, nspacetime] beta_space_time;
  real<lower=0> sdnoise; // noise sd;

}

model {
  vector[ncounts] lambda;
  sdnoise ~ gamma(0.001, 0.001);
  
  noise ~ normal(0, sdnoise);
  alpha[spregion] ~ normal(0, 0.1);
  beta_space_time[,1] ~ normal(0, 0.1);
  beta_space_time[,2] ~ normal(0, 0.1);
  
  for(i in 1:ncounts) {
    lambda[i] = alpha[spregion[i]] + beta_space_time[spregion[i],spacetime[i]] * pforest[i] + noise[i];
    
    count[i] ~ poisson_log(lambda[i]); // poisson with log link
  }
  
}


generated quantities{
  vector[nspregions] diff;

  diff = beta_space_time[,2] - beta_space_time[,1];
}

"

model <- stan(model_code = code,
              dat = dat_slim,
              chains = 3,
              cores = 3,
              iter = 4000,
              max_treedepth = 15)
