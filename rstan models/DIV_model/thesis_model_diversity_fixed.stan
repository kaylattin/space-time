data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nobs;                           // number of unique observers
  int<lower=1> nst;
  
  real diversity[ncounts];   // Shannon diversity
  int<lower=0> spacetime[ncounts];
  int<lower=0> space[ncounts];  // 0-1 indicator for space
  int<lower=0> time[ncounts];  // 0-1 indicator for time
  int reg[ncounts];  // Regions


  int obs[ncounts];   // observers
  real pforest[ncounts];  // Percent forest cover
  



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
// MAIN MODEL
  real<lower=0> sigma;
  

  vector[nreg] a;
  matrix[nst, nreg] b;

  
// OBSERVER SUB-MODEL
  real<lower=0> sigma_obs;
  vector[nroutes_obs] route_effect;             // Route effect on counts
  vector[necoreg_obs] ecoreg_effect;            // Ecoregion effect on counts
  vector[nobs_obs] obs_offset;                  // Unique offset term for each observer to feed into main model

  real<lower=0> sigma_e_obs;                    // Variance for ecoregion
  real<lower=0> sigma_r_obs;                    // Variance for route

}

model {
  vector[ncounts_obs] mu_obs;
  vector[ncounts] mu;

  
  
// OBSERVER SUB-MODEL
sigma_obs ~ student_t(4, 0, 1);

sigma_e_obs ~ student_t(4, 0, 1); 
sigma_r_obs ~ student_t(4, 0, 1);
 
route_effect ~ normal(0, sigma_r_obs);           // Prior for bbs route effect - random
ecoreg_effect ~ normal(0, sigma_e_obs);          // Prior for ecoregion effect - random
obs_offset ~ normal(0, 0.1);                   // Prior for observer offset - fixed



   // likelihood
    for(k in 1:ncounts_obs) {
    mu_obs[k] = route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]];
    }
    
diversity_obs ~ normal(mu_obs, sigma_obs);




// MAIN MODEL 

 a ~ normal(0, 0.1);
 to_vector(b) ~ normal(0, 0.1);
 sigma ~ student_t(4, 0, 1);
  
  // likelihood
    for(i in 1:ncounts) {
      
    mu[i] = a[reg[i]] + b[spacetime[i], reg[i]] * pforest[i] + obs_offset[obs[i]];
    }
    
diversity ~ normal(mu, sigma);          
   
}

generated quantities{
  real y_rep[ncounts];
  
  
  vector[nreg]  b_dif_rg;

     for(g in 1:nreg){
         b_dif_rg[g] = b[1, g]-b[2, g];
     }
  


  // Y_rep for prior predictive check
  for(i in 1:ncounts){
  y_rep[i] = normal_rng(a[reg[i]] + b[spacetime[i], reg[i]] * pforest[i] + obs_offset[obs[i]], sigma);
  }
}
