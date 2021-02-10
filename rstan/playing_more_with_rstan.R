
## mock rstan code as if I could ever figure it out to run it properly
setwd("/Users/kayla/Documents/space-time/data prep")
library(tidyverse)
library(rstan)
rm(list = ls())
gc()
d <- read.csv("whole_dataset_over40_5p.csv")


### my attempt at adapting this model to RStan --------------------------------------------------------

d_slim <- list(
  ncounts = nrow(d),
  nspreg = length(unique(sp.region)),
  nst = 2,
  
  count = ((d$Count - mean(d$Count)) / sd(d$Count)),
  st = d$space.time,
  spreg = as.integer(as.factor(d$SpeciesRegion)), # standardized
  obs = d$Obs_ID,
  pforest = ((d$Forest.cover - mean(d$Forest.cover)) / sd(d$Forest.cover)) # standardized
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
  vector[nst] b[nspreg];                        // Slope mean measuring forest cover effect, varying by species-regions combos nested inside space-time
  vector[nspreg] b_mod;                         // Beta modifier - pools information and additive function of time slope to give space slope 
  vector[ncounts] noise;                        // Over-dispersion noise parameter
  
  real<lower=0> sigma_n;                        // Variance for noise 
  corr_matrix[nst] Rho;                         // Correlation matrix for varying effects
  vector<lower=0>[nst] sigma_b;                 // Variance for slope
  real<lower=0> sigma_bmod;
  

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


}



model {

// OBSERVER SUB-MODEL


// priors
vector[ncounts_obs] lambda_obs;

species_effect ~ normal(0, 0.01);               // Prior for species effect - fixed
route_effect ~ normal(0, sigma_r_obs);          // Prior for bbs route effect - random
ecoreg_effect ~ normal(0, sigma_e_obs);         // Prior for ecoregion effect - random
obs_offset ~ normal(0, 0.01);                   // Prior for observer offset - fixed
noise_obs ~ normal(0, sigma_n_obs);             // Prior for over-dispersion term
sigma_n_obs ~ half-t(4,0,1);                    // prior for variances
sigma_e_obs ~ half-t(4,0,1);
sigma_r_obs ~ half-t(4,0,1);

  // model
    for(k in 1:ncounts_obs) {
    lambda_obs[k] = species_effect[species_obs[k]] + route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]] + noise_obs[k];
    
  }
      count_obs[k] ~ poisson_log(lambda_obs[k]);
  

// MAIN MODEL 

// priors
 vector[ncounts] lambda;
 noise ~ normal(0, sigma_n)                     // Prior for noise
 sigma_n ~ half-t(4, 0, 1);                     // Prior for scale parameter for noise
 
 Rho ~ lkj_corr(2);                             // Prior for correlation matrix
 sigma_b ~ half-t(4, 0, 1);                     // Prior for slope variance
 
 b_mod ~ normal(0, sigma_bmod);                 // Prior for slope modifier
 sigma_bmod ~ half-t(4, 0, 1);                  // Prior for slope modifier variance
 
 
 b ~ multi_normal( rep_vector(0, 2) , quad_form_diag(Rho, sigma_b));    // Prior for slope - random effect covariance matrix
 a ~ normal(0, 0.01);                                                   // Prior for intercept - fixed effect
  
  // model
  for(i in 1:ncounts) {
    lambda[i] = a[spreg[i]] + b[spreg[i], st[i]] * pforest[i] + obs_offset[obs[i]] + noise[i];
    

  }
      
      
      b[,2] = b[,1] + b_mod               // space slope as an additive function of time slope and modifier term
      
      
      
       ~ poisson_log(lambda);            // poisson with log link
       
       

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


save(model, file = "first_model_run.RData")



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
  real sigma_a = half_t_rng(4, 0, 1);
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

