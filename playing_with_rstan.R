
## mock rstan code as if I could ever figure it out to run it properly
setwd("/Users/kayla/Documents/space-time/data prep")
library(tidyverse)
d <- read.csv("whole_dataset_over40_5p.csv")
d_obs <- read.csv("observer_dataset_over40.csv")

obsID <- select(d_obs, c(ObsN, Obs_ID))
obsID <- obsID %>% distinct(ObsN, Obs_ID)
d <- merge(d, obsID, by = "ObsN", all.x = TRUE)

d_obs <- d_obs[!is.na(d_obs$Eco_ID),]

### set up main analysis da
space.time <- d$space.time # categorical
p_forest <- d$Forest.cover # continuous 
count <- d$Count # count
observer <- d$Obs_ID # categorical
sp.region_f <- d$SpeciesRegion # categorical

### set up observer model da
route <- d_obs$Route_ID # categorical - index variable
species_obs_f <- d_obs$BBL # categorical - factor
obs <- d_obs$Obs_ID # categorical - index variable
count_obs <- d_obs$Count 
ecoreg_obs <- d_obs$Eco_ID # categorical - index variable

# convert species factor to integer
sp.region <- as.integer(as.factor(sp.region_f))
species_obs <- as.integer(as.factor(species_obs_f))

### set up n's
ncounts <- nrow(d)
nsp.regions <- length(unique(sp.region)) # number of species
nobservers <- length(unique(observer)) # number of observers
ncounts_obs <- nrow(d_obs) # number of observer counts
nobs <- length(unique(obs)) # number of observers
nroutes_obs <- length(unique(route)) # number of routes
nspecies_obs <- length(unique(species_obs)) # number of species in observer daset
necoregs_obs <- length(unique(ecoreg_obs)) # number of ecoregs


### VERY SIMPLIFIED TEST MODEL WITHOUT OBSERVER SUBMODEL AND BETA_MOD

d_slim <- list(
  ncounts = nrow(d),
  nspregions = nsp.regions,
  nspacetime = 2,
  nobs = nobservers,
  count = count,
  spacetime = space.time,
  spregion = sp.region,
  pforest = p_forest,
  obs = observer,
  
  
  ncounts_obs =  ncounts_obs,
  nspecies_obs = nspecies_obs,
  nroutes_obs = nroutes_obs,
  necoreg_obs = necoregs_obs,
  nobs_obs = nobs,
  count_obs = count_obs,
  species_obs = species_obs,
  route_obs = route,
  ecoreg_obs = ecoreg_obs,
  obs_obs = obs
  
)

code <- " data {

  // Main model.
  int<lower=1> ncounts;   // Number of observations
  int<lower=1> nspregions;  // Number of grouping species-regions
  int<lower=1> nspacetime;  // Number of grouping space or time
  int<lower=1> nobs;        // Number of unique observers
  
  
  // Observed data
  int count[ncounts];
  int spacetime[ncounts];
  int spregion[ncounts];
  int obs[ncounts];
  real pforest[ncounts];

  
  
  /// Observer submodel.
  
  int<lower=1> ncounts_obs;
  int<lower=1> nspecies_obs;
  int<lower=1> nroutes_obs;
  int<lower=1> necoreg_obs;
  int<lower=1> nobs_obs;

  // Observed data
  int count_obs[ncounts_obs];
  int species_obs[ncounts_obs];
  int route_obs[ncounts_obs];
  int ecoreg_obs[ncounts_obs];
  int obs_obs[ncounts_obs];
}
parameters {
  // Main model.
  vector[nspregions] alpha;   // Vector of species-regions intercepts
  matrix[nspregions, nspacetime] beta; // Matrix of coefficients
  matrix[nspregions, nspacetime] beta_mod;
  vector[ncounts] noise;      // Noise for over-dispersion
  real<lower=0> sdnoise;     // Variance of over-dispersion
  
  vector[ncounts_obs] noise_obs;
  vector[nspecies_obs] species_effect;
  vector[nroutes_obs] route_effect;
  vector[necoreg_obs] ecoreg_effect;
  vector[nobs] obs_offset;
  
  
}
model {
  vector[ncounts] lambda;
  vector[ncounts_obs] lambda_obs;
  sdnoise ~ gamma(0.001, 0.001);
  
  noise_obs ~ normal(0, sdnoise);
  species_effect ~ normal(0, 0.1);
  route_effect ~ normal(0, 0.1);
  ecoreg_effect ~ normal(0, 0.1);
  obs_offset ~ normal(0, 0.1);
  
  noise ~ normal(0, sdnoise);
  alpha[spregion] ~ normal(0, 0.1);
  beta[,1] ~ normal(0, 0.1); // Time beta
  beta[,2] ~ normal(0, 0.1); // Space beta

  
  for(k in 1:ncounts_obs) {
    lambda_obs[k] = species_effect[species_obs[k]] + route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]] + noise_obs[k];
    
    count_obs[k] ~ poisson_log(lambda_obs[k]);
  }
  
  
  for(i in 1:ncounts) {
    lambda[i] = alpha[spregion[i]] + beta[spregion[i], spacetime[i]] * pforest[i] + obs_offset[obs[i]] + noise[i];
    
    count[i] ~ poisson_log(lambda[i]); // poisson with log link
  }
  
}
generated quantities{
  vector[nspregions] diff;

  diff = beta[,2] - beta[,1];
}

"

model <- stan(model_code = code,
              data = d_slim,
              chains = 3,
              cores = 3,
              iter = 4000)
