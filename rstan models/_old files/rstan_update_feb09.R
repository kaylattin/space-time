## code adapted from the stancode of a brm.fit object of a simpler object
### unbalanced num of observations across species and regions, so unable to assign to an nspecies x nreg matrix

## instead each group-level (species, region, st) gets their own matrix of intercepts & slopes (or slope only, depending)
## and subsequently their own covariance matrices that captures correlation between intercepts and slopes?

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
  // what data to feed in?
  vector[N] Z_species_a;
  vector[N] Z_species_b;
  int<lower=1> NC_species;   // number of group-level correlations
  
  
  // data for group-level effects of region
  int<lower=1> N_reg;                            // Number of region levels (40)
  int<lower=1> M_reg;                            // Number of coefficients per level (2) - intercept & slope
  int<lower=1> J_reg[N];                         // grouping indicator per observation
  // region group-level predictor values
  vector[N] Z_reg_a;
  vector[N] Z_reg_b;
  int<lower=1> NC_reg;  // number of group-level correlations
  
  // data for group-level effects in space and time
  int<lower=1> N_st;                            // Number of space-time levels (2)
  int<lower=1> M_st;                            // Number of coefficients per level (1) - slope only
  int<lower=1> J_st[N];                         // grouping indicator per observation
  // space-time group-level predictor values
  vector[N] Z_st_b;
  int<lower=1> NC_st;   // number of group-level correlations
  
  // data for observer effects
  int<lower=1> N_obs;                           // Number of observer levels
  int<lower=1> J_obs[N];




// OBSERVER SUB-MODEL --------------------

  
  int<lower=1> N_ob;                             // Number of observations for observer submodel
  int count_ob[N_ob];                            // Response variable - counts in observer dataset
  
  // data for group-level effect of species
  int<lower=1> N_species_ob;                    // Number of species levels for observer submodel
  int<lower=1> J_species_ob[N_ob]               // grouping indicator
  
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
  
  vector<lower=0>[M_reg] sd_reg;    // group-level standard deviations
  matrix[M_reg, N_reg] z_reg;    // standardized group-level region effects
  cholesky_factor_corr[M_reg] L_reg;    // cholesky factor
  
  vector<lower=0>[M_st] sd_st;    // group-level standard deviations
  matrix[M_st, N_st] z_st;    // standardized group-level space-time effects
  cholesky_factor_corr[M_st] L_st;    // cholesky factor
 
  vector[N] noise;                        // Over-dispersion noise parameter


// OBSERVER SUB-MODEL ----------------
  
  vector[N_species_ob] species_effect; // species group effect
  vector[N_rte_ob] route_effect;  // route group effect 
  vector[N_ecoreg_ob] ecoreg_effect;  // ecoregion group effect
  vector[N_obs_ob] obs_offset;  // Unique offset term for each observer to feed into main model
  
  vector[N_ob] noise_obs;   // Over-dispersion term for observer sub-model
  
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
  r_species = scale_r_corr(z_species, sd_species, L_species);
  r_species_a = r_species[, 1];   // intercept
  r_species_b = r_species[, 2];   // slope
  
  r_reg = scale_r_corr(z_reg, sd_reg, L_reg);
  r_reg_a = r_reg[, 1];     // intercept
  r_reg_b = r_reg[, 2];    // slope
  
  r_st = scale_r_corr(z_st, sd_st, L_st);
  r_st_b = r_reg[, 1];   // slope

}


model {

// not sure how to interpret the brm code in this situation?? attempt below
  vector[N] mu = rep_vector(0.0, N);
  for (n in 1:N){
    mu[n] += r_species_a[J_species[n]] + r_reg_a[J_reg[n]] + r_species_b[J_species[n]] * pforest[n] + r_reg_b[J_reg[n]] * pforest[n] + r_st_b[J_st[n]] * pforest[n] + noise[n];
  }
  
  target += poisson_log_glm_lpmf(count | mu);
  
  
  
  // priors for main model -----------
  noise ~ student_t(4, 0, 1);
  
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
  noise_obs ~ student_t(4, 0, 1);


  species_effect ~ normal(0, 0.01);
  
  route_effect ~ normal(0, sigma_r_ob);
  sigma_r_ob ~ student_t(4, 0, 1);
  
  ecoreg_effect ~ normal(0, sigma_e_ob);
  sigma_e_ob ~ student_t(4, 0, 1);
  
  obs_offset ~ normal(0, 0.01);


}

generated quantities{
  
  // compute group-level correlations 
  corr_matrix[M_species] Cor_species = multiply_lower_tri_self_transpose(L_species);
  
  corr_matrix[M_reg] Cor_reg = multiply_lower_tri_self_transpose(L_reg);
  
  corr_matrix[M_st] Cor_st = multiply_lower_tri_self_transpose(L_st);
  
  
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
