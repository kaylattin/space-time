data {

// OBSERVER SUB-MODEL --------------------
  // groups and counts
  int<lower=1> ncounts_obs;                     // Number of observations in observer dataset
  int<lower=1> nroutes_obs;                     // Number of bbs routes in observer dataset
  int<lower=1> necoreg_obs;                     // Number of unique ecoregions in observer dataset
  int<lower=1> nobs_obs;                        // Number of unique observers in observer dataset
  
  // observed data
  real diversity_obs[ncounts_obs];   // Shannon diversity
  int route_obs[ncounts_obs];    // bbs route
  int ecoreg_obs[ncounts_obs];   // Ecoregion
  int obs_obs[ncounts_obs];   // bbs observer
  
}


parameters {

// OBSERVER SUB-MODEL
  real<lower=0> sigma_obs;
  vector[nroutes_obs] route_effect;             // Route effect on counts
  vector[necoreg_obs] ecoreg_effect;            // Ecoregion effect on counts
  vector[nobs_obs] obs_offset;                  // Unique offset term for each observer to feed into main model

}


model {
  vector[ncounts_obs] mu_obs;

  
  
// OBSERVER SUB-MODEL
sigma_obs ~ student_t(4, 0, 1);

route_effect ~ std_normal();           // Prior for bbs route effect - random
ecoreg_effect ~ std_normal();          // Prior for ecoregion effect - random
obs_offset ~ std_normal();                 // Prior for observer offset - fixed



   // likelihood
    for(k in 1:ncounts_obs) {
    mu_obs[k] = route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]];
    }
    
diversity_obs ~ normal(mu_obs, sigma_obs);

}

generated quantities{
  real y_rep[ncounts_obs];
  
  
  // Y_rep for prior predictive check
  for(k in 1:ncounts_obs){
  y_rep[k] = normal_rng(route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]], sigma_obs);
  }
}
