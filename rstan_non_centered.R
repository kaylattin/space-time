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
  nobs = length(unique(d$ObsN)),
  
  count = d$Count,
  st = d$space.time,
  species = as.integer(as.factor(d$BBL)), 
  reg = as.integer(as.factor(d$ref)),
  pforest = (d$Forest.cover - mean(d$Forest.cover)), # centered
  obs = as.integer(as.factor(d$ObsN)),
  
  
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
  int<lower=1> nspecies;                        // Number of species
  int<lower=1> nreg;                            // Number of regions
  int<lower=1> nst;                             // Number of grouping space or time
  int<lower=1> nobs;
  
  // observed data
  int count[ncounts];                           // Species abundance count observations
  int st[ncounts];                              // Space or time
  int species[ncounts];                         // Species 
  int reg[ncounts];                             // Regions
  int obs[ncounts];
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

  matrix[nspecies, nreg] a;                     // Intercept mean varying by species-regions
  real mu_a;
  real<lower=0> sigma_a;
 
  vector[ncounts] noise;                        // Over-dispersion noise parameter
  
  
  matrix[nreg, nst] z_b[nspecies];              // an nspecies vector of matrix nreg x nst (40 x 2) but not every species will have a 40 x 2 matrix
                                                // because not all species are present in all 40 comparison regions - how to work around this?
                                                // and how to do a covariance matrix for a vector of matrices? a vector of covariance matrices?

  cholesky_factor_corr[nst] L_Rho;            // Cholesky correlation matrix for varying effects
  
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
  matrix[nreg, nst] b[nspecies];              // Slope mean measuring forest cover effect, varying by region and by space-time identifier
  
  for(n in 1:nspecies)                        // an nspecies vector of covariance matrices???
  b[n] = (diag_pre_multiply(sigma_b, L_Rho) * z_b[reg[n], st[n])';
}


model {

  vector[ncounts_obs] lambda_obs;
  vector[ncounts] lambda;
  
  
// OBSERVER SUB-MODEL

sigma_n_obs ~ inv_gamma(0.001, 0.001);           // prior for variances
sigma_e_obs ~ inv_gamma(0.001, 0.001); 
sigma_r_obs ~ inv_gamma(0.001, 0.001);
 
species_effect ~ normal(0, 0.01);                // Prior for species effect - fixed
route_effect ~ normal(0, sigma_r_obs);           // Prior for bbs route effect - random
ecoreg_effect ~ normal(0, sigma_e_obs);          // Prior for ecoregion effect - random
obs_offset ~ normal(0, 0.01);                    // Prior for observer offset - fixed
noise_obs ~ normal(0, sigma_n_obs);              // Prior for over-dispersion term
 
   // likelihood
    for(k in 1:ncounts_obs) 
    lambda_obs[k] = species_effect[species_obs[k]] + route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]] + noise_obs[k];

count_obs ~ poisson_log(lambda_obs);



// MAIN MODEL 

 sigma_n ~ student_t(4, 0, 1);                   // Prior for scale parameter for noise
 noise ~ normal(0, sigma_n);                     // Prior for noise


 to_vector(z_b) ~ normal(0, 1);                   // Prior for slope z-score
 //L_Rho ~ lkj_corr_cholesky(2);                  // Prior for Cholesky correlation matrix
 sigma_b ~ student_t(4, 0, 1);                    // Prior for slope variance
 
 sigma_a ~ student_t(4, 0, 1);
 mu_a ~ normal(0, 0.01)
 a ~ normal(mu_a, sigma_a);                       // Prior for intercept
  
  // likelihood
    for(i in 1:ncounts) 
    lambda[i] = a[species[i], reg[i]] + b[reg[i], st[i]] * pforest[i] + obs_offset[obs[i]] + noise[i];

count ~ poisson_log(lambda);          
   
}

generated quantities{
  vector[ncounts] y_rep;
  matrix[2,2] Rho;
  
    
  // Compute ordinary correlation matrices from Cholesky factors
  Rho = multiply_lower_tri_self_transpose(L_Rho);
  
  
  
  // Y_rep for posterior predictive check
  //for(i in 1:ncounts){
  //  y_rep[i] = poisson_log_rng(a[species[i], reg[i]] + b[reg[i], st[i]] * pforest[i] + obs_offset[obs[i]] + noise[i]);
  //}

}



"
# possible reasons this keeps crashing my session:
# generated quantities is too big (too much RAM) - do a PPC on a subset of the data and then omit for whole dataset?
# need to add gc()?
# didn't compile and sample the model properly - changed below (haven't tried yet)

# 2000 divergent transitions and low ESS
# might need to consider non-centered parameterization if adapt_delta and more iterations doesn't help

# compile the model?
model <- stan_model(model_name = "thesis_model",
                    model_code = code)

gc()
# fit the model ------------------------------
stan.fit <- sampling(object = model,
                     data = d_slim,
                     iter = 2000,
                     chains = 3,
                     cores = 3,
                     init = 'random',
                     show_messages = TRUE,
                     control = list(max_treedepth = 15,
                                    adapt_delta = 0.99))


y_rep <- as.matrix(stan.fit, pars = "y_rep")
ppc_dens_overlay(y = d$Count, yrep = y_rep)


fit_summary <- summary(stan.fit)
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
