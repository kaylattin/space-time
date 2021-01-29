## load up stuff
setwd("/Users/kayla/Documents/space-time/data prep")
library(tidyverse)
library(rstan)
library(bayesplot)
rm(list = ls())
gc()

d <- read.csv("whole_dataset_over40_5p.csv")
d_obs <- read.csv("observer_dataset_over40.csv")


# last-minute removal of ecoregion's with NAs for observer dataset
# doesn't change the unique # of observers
d_obs <- d_obs[!is.na(d_obs$Eco_ID),]


### cut down dataset to test (first 4 comparison regions)

d <- d %>% filter(Region == c(1,2,3,4))
d_obs <- d_obs %>% filter(ObsN %in% d$ObsN)

### my attempt at adapting this model to RStan --------------------------------------------------------

d_slim <- list(
  ncounts = nrow(d),
  nspreg = length(unique(d$SpeciesRegion)),
  nst = 2,
  nobs = length(unique(d$ObsN)),
  
  count = d$Count,
  st = d$space.time,
  spreg = as.integer(as.factor(d$SpeciesRegion)), 
  obs = as.integer(as.factor(d$ObsN)),
  pforest = (d$Forest.cover - mean(d$Forest.cover)), # centered
  
  
  count_obs = d_obs$Count,
  species_obs = as.integer(as.factor(d_obs$BBL)),
  route_obs = as.integer(as.factor(d_obs$RouteNumber)),
  ecoreg_obs = as.integer(as.factor(d_obs$Ecoregion_L1Code)),
  obs_obs = as.integer(as.factor(d_obs$ObsN)),
  
  ncounts_obs = nrow(d_obs),
  nspecies_obs = length(unique(d_obs$BBL)),
  nroutes_obs = length(unique(d_obs$Route_ID)),
  necoreg_obs = length(unique(d_obs$Eco_ID)), 
  nobs_obs = length(unique(d_obs$ObsN))
  
  
)


code <- " data {


// MAIN MODEL

  // groups and counts
  int<lower=1> ncounts;                         // Number of observations
  int<lower=1> nspreg;                          // Number of grouping species-regions
  int<lower=1> nst;                             // Number of grouping space or time
  int<lower=1> nobs;                            // Number of unique observers
  
  // observed data
  int count[ncounts];                           // Species abundance count observations
  int st[ncounts];                              // Space or time
  int spreg[ncounts];                           // Species-region combo
  int obs[ncounts];                             // bbs observer
  real pforest[ncounts];                        // Percent forest cover
  




// OBSERVER SUB-MODEL

  // groups and counts
  int<lower=1> ncounts_obs;                     // Number of observations in observer dataset
  int<lower=1> nspecies_obs;                    // Number of unique species in observer dataset
  int<lower=1> nroutes_obs;                     // Number of bbs routes in observer dataset
  int<lower=1> necoreg_obs;                     // Number of unique ecoregions in observer dataset
  int<lower=1> nobs_obs;                        // Number of unique observers in observer dataset
  
  // observed data
  int count_obs[ncounts_obs];                   // Species abundance counts
  int species_obs[ncounts_obs];                 // Species
  int route_obs[ncounts_obs];                   // bbs route
  int ecoreg_obs[ncounts_obs];                  // Ecoregion
  int obs_obs[ncounts_obs];                     // bbs observer
  
}

parameters {

// MAIN MODEL

  vector[nspreg] a;                             // Intercept mean varying by species-regions
 
  vector[ncounts] noise;                        // Over-dispersion noise parameter
  
  
  matrix[nst, nspreg] z_b;                      // Matrix of z-scores for slope coefficient b
  cholesky_factor_corr[nst] L_Rho;              // Cholesky correlation matrix for varying effects
  
  real<lower=0> sigma_n;                        // Variance for noise 
  vector<lower=0>[nst] sigma_b;                 // Variance for slope


// OBSERVER SUB-MODEL
  
  vector[nspecies_obs] species_effect;          // Species effect on counts
  vector[nroutes_obs] route_effect;             // Route effect on counts
  vector[necoreg_obs] ecoreg_effect;            // Ecoregion effect on counts
  vector[nobs_obs] obs_offset;                  // Unique offset term for each observer to feed into main model
  vector[ncounts_obs] noise_obs;                // Over-dispersion term for observer sub-model
  
  real<lower=0> sigma_n_obs;                    // Variance for noise
  real<lower=0> sigma_e_obs;                    // Variance for ecoregion
  real<lower=0> sigma_r_obs;                    // Variance for route
  

}

transformed parameters{
  matrix[nspreg, nst] b;                        // Slope mean measuring forest cover effect, varying by species-regions combos nested inside space-time
  

  b = (diag_pre_multiply(sigma_b, L_Rho) * z_b)';
}


model {

  vector[ncounts_obs] lambda_obs;
  vector[ncounts] lambda;
  
  
// OBSERVER SUB-MODEL


// priors

sigma_n_obs ~ cauchy(0,4);                      // prior for variances
sigma_e_obs ~ cauchy(0,4); 
sigma_r_obs ~ cauchy(0,4);

species_effect ~ normal(0, 0.01);               // Prior for species effect - fixed
route_effect ~ normal(0, sigma_r_obs);                 // Prior for bbs route effect - random
ecoreg_effect ~ normal(0, sigma_e_obs);                // Prior for ecoregion effect - random
obs_offset ~ normal(0, 0.01);                   // Prior for observer offset - fixed
noise_obs ~ normal(0, sigma_n_obs);             // Prior for over-dispersion term



// MAIN MODEL 

// priors

 sigma_n ~ cauchy(0, 4);                         // Prior for scale parameter for noise
 noise ~ normal(0, sigma_n);                     // Prior for noise


 to_vector(z_b) ~ normal(0, 1);                   // Prior for slope z-score
 L_Rho ~ lkj_corr_cholesky(2);                   // Prior for Cholesky correlation matrix
 sigma_b ~ cauchy(0, 4);                         // Prior for slope variance
 

 a ~ normal(0, 0.01);                                                   // Prior for intercept - fixed effect
  
  
    // model
    for(k in 1:ncounts_obs) {
    lambda_obs[k] = species_effect[species_obs[k]] + route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]] + noise_obs[k];
   
    count_obs[k] ~ poisson_log(lambda_obs[k]);
  }
 
  
  // model
  for(i in 1:ncounts) {
    lambda[i] = a[spreg[i]] + b[spreg[i], st[i]] * pforest[i] + obs_offset[obs[i]] + noise[i];
    
    
    count[i] ~ poisson_log(lambda[i]);            // poisson with log link

  }
  
    
   
}

generated quantities{
  vector[nspreg] diff;
  
  for(i in 1:nspreg){
    diff[i] = b[spreg[i],st[2]] - b[spreg[i],st[1]];    // Find difference
  
  }
  
  // Y_rep for posterior predictive check
  vector[ncounts] y_rep;
  for(i in 1:ncounts){
    y_rep[ncounts] = poisson_log_rng(a[spreg[i]] + b[spreg[i], st[i]] * pforest[i] + obs_offset[obs[i]] + noise[i]);
  
  }

}



"


# 2000 divergent transitions and low ESS
# might need to consider non-centered parameterization if adapt_delta and more iterations doesn't help


model <- stan(model_code = code,
              data = d_slim,
              chains = 1,
              cores = 3,
              iter = 500,
              control = list(adapt_delta = 0.99,
                             max_treedepth = 15))

save(model, file = "non_centered_test_2.RData")
# no divergent tranasitions with non-centered parameterization yay

y_rep <- as.matrix(model, pars = "y_rep")
ppc_dens_overlay(y = d$Count, yrep = y_rep)




# prior predictive check -------------------------------------------------------------------

dat_pp <- list(
  ncounts = nrow(d),
  pforest = (( d$Forest.cover - mean(d$Forest.cover)) / sd(d$Forest.cover))
)



pp <- " data {

  // No. observations & groups
  int<lower=1> ncounts;                         // Number of observations
  real pforest[ncounts];
  
}

generated quantities{
  real sigma_a = cauchy_rng(4, 0, 1);
  real a = normal_rng(0, sigma_a);
  real b = normal_rng(0, 0.01);
  int y_sim[ncounts];
  
  for(i in 1:ncounts) y_sim[i] = poisson_log_rng(a + b * pforest[i]);

}


"

model <- stan(model_code = pp,
              data = dat_pp,
              algorithm = "Fixed_param",
              chains = 1,
              cores = 3,
              iter = 1000)

y_rep <- as.matrix(model, pars = "y_sim")
dim(y_rep)


ppc_dens_overlay(y = d$Count, yrep = y_rep)
