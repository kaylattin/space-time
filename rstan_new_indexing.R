## code adapted from the stancode of a brm.fit object of a simpler model
### unbalanced num of observations across species and regions, so unable to assign to an nspecies x nreg matrix

## instead each group-level (species, region, st) gets their own matrix of intercepts & slopes (or slope only, depending)
## and subsequently their own covariance matrices that captures correlation between intercepts and slopes?

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
  N = nrow(d),
  N_species = length(unique(d$BBL)),
  M_species = 2,
  NC_species = 2,
  
  N_reg = length(unique(d$ref)),
  M_reg = 2,
  NC_reg = 2,
  
  N_st = 2,
  M_st = 1,
  NC_st = 1,
  
  N_obs = length(unique(d$ObsN)),
  
  count = d$Count,
  J_st = d$space.time,
  J_species = as.integer(as.factor(d$BBL)), 
  J_reg = as.integer(as.factor(d$ref)),
  pforest = (d$Forest.cover - mean(d$Forest.cover)), # centered
  J_obs = as.integer(as.factor(d$ObsN)),
  
  N_ob = nrow(d_obs),
  count_ob = d_obs$Count,
  N_species_ob = length(unique(d_obs$BBL)),
  J_species_ob = as.integer(as.factor(d_obs$BBL)),
  
  N_rte_ob = length(unique(d_obs$Route_ID)),
  J_rte_ob = as.integer(as.factor(d_obs$RouteNumber)),
 
  N_ecoreg_ob = length(unique(d_obs$Eco_ID)), 
  J_ecoreg_ob = as.integer(as.factor(d_obs$Ecoregion_L1Code)),
  
  
  N_obs_ob = length(unique(d_obs$ObsN)),
  J_obs_ob = as.integer(as.factor(d_obs$ObsN))

  
  
)



code <- " data {



// MAIN MODEL -------------

  int<lower=1> N;                               // Number of observations
  int count[N];                                 // Response variable - count observations
  real pforest[N];                              // Predictor - percent forest cover
  
  
  // data for group-level effects of species
  int<lower=1> N_species;                       // Number of species levels (407)
  int<lower=1> M_species;                       // Number of coefficients per level (2) - intercept & slope
  int<lower=1> J_species[N];                    // grouping indicator per observation
  // species group-level predictor values -- these were in the brm code but I haven't figured out what they mean
  // what data to feed into these 'Z' objects? just leaving out for now
  //vector[N] Z_species_a;
  //vector[N] Z_species_b;
  int<lower=1> NC_species;   // number of group-level correlations
  
  
  // data for group-level effects of region
  int<lower=1> N_reg;                            // Number of region levels (40)
  int<lower=1> M_reg;                            // Number of coefficients per level (2) - intercept & slope
  int<lower=1> J_reg[N];                         // grouping indicator per observation
  // region group-level predictor values
  //vector[N] Z_reg_a;
  //vector[N] Z_reg_b;
  int<lower=1> NC_reg;  // number of group-level correlations
  
  // data for group-level effects in space and time
  int<lower=1> N_st;                            // Number of space-time levels (2)
  int<lower=1> M_st;                            // Number of coefficients per level (1) - slope only
  int<lower=1> J_st[N];                         // grouping indicator per observation
  // space-time group-level predictor values
  //vector[N] Z_st_b;
  int<lower=1> NC_st;   // number of group-level correlations
  
  // data for observer effects
  int<lower=1> N_obs;                           // Number of observer levels
  int<lower=1> J_obs[N];




// OBSERVER SUB-MODEL --------------------

  
  int<lower=1> N_ob;                             // Number of observations for observer submodel
  int count_ob[N_ob];                            // Response variable - counts in observer dataset
  
  // data for group-level effect of species
  int<lower=1> N_species_ob;                    // Number of species levels for observer submodel
  int<lower=1> J_species_ob[N_ob];               // grouping indicator
  
  // data for group-level effect of bbs route
  int<lower=1> N_rte_ob;                        // Number of route grouping levels
  int<lower=1> J_rte_ob[N_ob];                  // grouping indicator
  
  // data for group-level effect of ecoregion
  int<lower=1> N_ecoreg_ob;                     // Number of ecoregion grouping levels
  int<lower=1> J_ecoreg_ob[N_ob];               // grouping indicator
  
  // data for group-level effect of observer effects
  int<lower=1> N_obs_ob;                         // Number of observer levels
  int<lower=1> J_obs_ob[N_ob];                  // grouping indicator

}



parameters {

// MAIN MODEL ----------------
  vector<lower=0>[M_species] sd_species;    // group-level standard deviations
  matrix[M_species, N_species] z_species;    // standardized group-level species effects
  cholesky_factor_corr[M_species] L_species;    // cholesky factor of correlation matrix
  real mu_s;
  real<lower=0> sigma_s;
  
  vector<lower=0>[M_reg] sd_reg;    // group-level standard deviations
  matrix[M_reg, N_reg] z_reg;    // standardized group-level region effects
  cholesky_factor_corr[M_reg] L_reg;    // cholesky factor
  real<lower=0> sigma_r;
  
  vector<lower=0>[M_st] sd_st;    // group-level standard deviations
  matrix[M_st, N_st] z_st;    // standardized group-level space-time effects
  cholesky_factor_corr[M_st] L_st;    // cholesky factor
  real<lower=0> sigma_st;
 
  vector[N] noise;                        // Over-dispersion noise parameter
  real<lower=0> sd_noise;


// OBSERVER SUB-MODEL ----------------
  
  vector[N_species_ob] species_effect; // species group effect
  vector[N_rte_ob] route_effect;  // route group effect 
  vector[N_ecoreg_ob] ecoreg_effect;  // ecoregion group effect
  vector[N_obs_ob] obs_offset;  // Unique offset term for each observer to feed into main model
  
  vector[N_ob] noise_ob;   // Over-dispersion term for observer sub-model
  
  real<lower=0> sd_noise_ob;                   
  real<lower=0> sd_ecoreg_ob;                
  real<lower=0> sd_rte_ob;  // group-level sd
  

}

transformed parameters{
  matrix[N_species, M_species] r_species;   // actual species group-level effects
  vector[N_species] r_species_a;   // intercept
  vector[N_species] r_species_b;   // slope
  
  matrix[N_reg, M_reg] r_reg;  // actual region group-level effects
  vector[N_reg] r_reg_a;   // intercept
  vector[N_reg] r_reg_b;   // slope
  
  matrix[N_st, M_st] r_st;  // actual space-time group-level effects
  vector[N_st] r_st_b;   // slope


  // compute actual group-level effect parameters from z-scores
  r_species = (diag_pre_multiply(sd_species, L_species) * z_species);
  r_species_a = r_species[, 1];   // intercept
  r_species_b = r_species[, 2];   // slope
  
  r_reg = (diag_pre_multiply(sd_reg, L_reg) * z_reg);
  r_reg_a = r_reg[, 1];     // intercept
  r_reg_b = r_reg[, 2];    // slope
  
  r_st = (diag_pre_multiply(sd_st, L_st) * z_st);
  r_st_b = r_reg[, 1];   // slope

}


model {

  vector[N] mu = rep_vector(0.0, N);
  vector[N_ob] mu_ob = rep_vector(0.0, N_ob);
    
    
  for (n in 1:N){
    mu[n] += r_species_a[J_species[n]] + r_reg_a[J_reg[n]] + r_species_b[J_species[n]] * pforest[n] + r_reg_b[J_reg[n]] * pforest[n] + r_st_b[J_st[n]] * pforest[n] + obs_offset[J_obs[n]] +  noise[n];
  }
  
  target += poisson_log_lpmf(count | mu);
  

  for(n in 1:N_ob){
    mu_ob[n] += species_effect[J_species_ob[n]] + route_effect[J_rte_ob[n]] + ecoreg_effect[J_ecoreg_ob[n]] + obs_offset[J_obs_ob[n]] + noise_ob[n];
  
  }
  
  target += poisson_log_lpmf(count_ob | mu_ob);
  
  // priors for main model -----------
  noise ~ normal(0, sd_noise);
  sd_noise ~ student_t(4, 0, 1);
  
  
  to_vector(z_species) ~ normal(mu_s, sigma_s);
  mu_s ~ normal(0, 0.01);
  sigma_s ~ student_t(4, 0, 1);
  sd_species ~ student_t(4, 0, 1);
  L_species ~ lkj_corr_cholesky(2);
  
  to_vector(z_reg) ~ normal(0, sigma_r);
  sigma_r ~ student_t(4, 0, 1);
  sd_reg ~ student_t(4, 0, 1);
  L_reg ~ lkj_corr_cholesky(2);
  
  to_vector(z_st) ~ normal(0, sigma_st);
  sigma_st ~ student_t(4, 0, 1);
  sd_st ~ student_t(4, 0, 1);
  L_st ~ lkj_corr_cholesky(2);


  
// priors for observer model ---------------
  noise_ob ~ normal(0, sd_noise_ob);
  sd_noise_ob ~ student_t(4, 0, 1);


  species_effect ~ normal(0, 0.01);
  
  route_effect ~ normal(0, sd_rte_ob);
  sd_rte_ob ~ student_t(4, 0, 1);
  
  ecoreg_effect ~ normal(0, sd_ecoreg_ob);
  sd_ecoreg_ob ~ student_t(4, 0, 1);
  
  obs_offset ~ normal(0, 0.01);


}

generated quantities{
  
  // compute group-level correlations 
  corr_matrix[M_species] Cor_species = multiply_lower_tri_self_transpose(L_species);
  
  corr_matrix[M_reg] Cor_reg = multiply_lower_tri_self_transpose(L_reg);
  
  corr_matrix[M_st] Cor_st = multiply_lower_tri_self_transpose(L_st);


}



"
# observer submodel increases runtime by a huge amount - maybe leave out noise_ob and other parameters to quicken the sampling time

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
