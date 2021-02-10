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

#d <- d %>% filter(Region == c(1,2,3,4))
#d_obs <- d_obs %>% filter(ObsN %in% d$ObsN)

### my attempt at adapting this model to RStan --------------------------------------------------------

d_slim <- list(
  ncounts = nrow(d),
  nspecies = length(unique(d$BBL)),
  nreg = length(unique(d$ref)),
  nst = 2,

  count = d$Count,
  st = d$space.time,
  species = as.integer(as.factor(d$SpeciesRegion)), 
  reg = as.integer(as.factor(d$ref)),
  pforest = (d$Forest.cover - mean(d$Forest.cover)) # centered
 
  
)


code <- " data {


// MAIN MODEL

  // groups and counts
  int<lower=1> ncounts;                         // Number of observations
  int<lower=1> nspecies,                        // Number of species
  int<lower=1> nreg;                            // Number of regions
  int<lower=1> nst;                             // Number of grouping space or time
  
  // observed data
  int count[ncounts];                           // Species abundance count observations
  int st[ncounts];                              // Space or time
  int species[ncounts];                         // Species 
  int reg[ncounts];                             // Regions
  real pforest[ncounts];                        // Percent forest cover
  


}

parameters {

// MAIN MODEL

  vector[nspreg] a;                             // Intercept mean varying by species-regions
  real<lower=0> sigma_a;
 
  vector[ncounts] noise;                        // Over-dispersion noise parameter
  
  
  matrix[nst, nspreg] z_b;                      // Matrix of z-scores for slope coefficient b
  cholesky_factor_corr[nst] L_Rho;              // Cholesky correlation matrix for varying effects
  
  real<lower=0> sigma_n;                        // Variance for noise 
  vector<lower=0>[nst] sigma_b;                 // Variance for slope

}

transformed parameters{
  matrix[nspreg, nst] b;                        // Slope mean measuring forest cover effect, varying by species-regions combos nested inside space-time
  

  b = (diag_pre_multiply(sigma_b, L_Rho) * z_b)';
}


model {

  vector[ncounts] lambda;
  
  


// MAIN MODEL 

// priors

 sigma_n ~ inv_gamma(0.001, 0.001);                         // Prior for scale parameter for noise
 noise ~ normal(0, sigma_n);                     // Prior for noise


 to_vector(z_b) ~ normal(0, 1);                   // Prior for slope z-score
 L_Rho ~ lkj_corr_cholesky(2);                   // Prior for Cholesky correlation matrix
 sigma_b ~ inv_gamma(0.001, 0.001);                         // Prior for slope variance
 
 sigma_a ~ inv_gamma(0.001, 0.001);
 a ~ normal(0, sigma_a);                        // Prior for intercept - fixed effect
  
 
  
  // model
  for(i in 1:ncounts) {
    lambda[i] = a[spreg[i]] + b[spreg[i], st[i]] * pforest[i] + noise[i];
    
    
    count[i] ~ poisson_log(lambda[i]);            // poisson with log link

  }
  
    
   
}

generated quantities{
  vector[nspreg] diff;
  vector[ncounts] y_rep;
  matrix[2,2] Rho;
  
    
  // Compute ordinary correlation matrices from Cholesky factors
  Rho = multiply_lower_tri_self_transpose(L_Rho);
  
  
  
  
  // Find difference between spatial & temporal slopes
  for(i in 1:nspreg){
    diff[i] = b[spreg[i],st[2]] - b[spreg[i],st[1]];    
  
  }

  
  
  // Y_rep for posterior predictive check
  for(i in 1:ncounts){
    y_rep[i] = poisson_log_rng(a[spreg[i]] + b[spreg[i], st[i]] * pforest[i] + noise[i]);
  }

}



"


# 2000 divergent transitions and low ESS
# might need to consider non-centered parameterization if adapt_delta and more iterations doesn't help


model <- stan(model_code = code,
              data = d_slim,
              iter = 1,
              warmup = 0,
              chains = 2)


stan.fit <- stan(fit = model,
                 data = d_slim,
                 iter = 2000,
                 warmup = 500,
                 chains = 3)


save(model, file = "non_centered_whole.RData")
# no divergent tranasitions with non-centered parameterization yay

y_rep <- as.matrix(model, pars = "y_rep")
ppc_dens_overlay(y = d$Count, yrep = y_rep)


fit_summary <- summary(model)
s <- print(fit_summary$summary, pars = "b")

# prior predictive check -------------------------------------------------------------------

dat_pp <- list(
  ncounts = nrow(d),
  pforest = d$Forest.cover - mean(d$Forest.cover)
)



pp <- " data {

  // No. observations & groups
  int<lower=1> ncounts;                         // Number of observations
  real pforest[ncounts];
  
}

generated quantities{
  real<lower=0> sigma_b;
  real a = normal_rng(0, 0.01);
  real b = normal_rng(0, sigma_b);
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
