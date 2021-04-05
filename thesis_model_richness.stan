data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nobs;                           // number of unique observers
  int<lower=1> nst;
  
  int<lower=0> spacetime[ncounts];
  int richness[ncounts];   // Species richness
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
  int richness_obs[ncounts_obs];   // Total abundance at a route
  int route_obs[ncounts_obs];    // bbs route
  int ecoreg_obs[ncounts_obs];   // Ecoregion
  int obs_obs[ncounts_obs];   // bbs observer
  
}



parameters {
// MAIN MODEL
  matrix[nreg, nst] a;

  vector[ncounts] noise;                        // Over-dispersion noise parameter
  real<lower=0> sigma_n;                        // Variance for noise 
  
  vector[nreg] b_space;
  vector[nreg] b_time;
  


  
// OBSERVER SUB-MODEL
  
  vector[nroutes_obs] route_effect;             // Route effect on counts
  vector[necoreg_obs] ecoreg_effect;            // Ecoregion effect on counts
  vector[nobs_obs] obs_offset;                  // Unique offset term for each observer to feed into main model
  vector[ncounts_obs] noise_obs;                // Over-dispersion term for observer sub-model
  
  real<lower=0> sigma_n_obs;                    // Variance for noise
  real<lower=0> sigma_e_obs;                    // Variance for ecoregion
  real<lower=0> sigma_r_obs;                    // Variance for route

}



model {
  vector[ncounts_obs] lambda_obs;
  vector[ncounts] lambda;
  
  
// OBSERVER SUB-MODEL
sigma_n_obs ~ student_t(20, 0, 1);                 // prior for variances
sigma_e_obs ~ student_t(20, 0, 1); 
sigma_r_obs ~ student_t(20, 0, 1);
 
route_effect ~ normal(0, sigma_r_obs);           // Prior for bbs route effect - random
ecoreg_effect ~ normal(0, sigma_e_obs);          // Prior for ecoregion effect - random
obs_offset ~ normal(0, 1);                   // Prior for observer offset - fixed
noise_obs ~ normal(0, sigma_n_obs);              // Prior for over-dispersion term
 
   // likelihood
    for(k in 1:ncounts_obs) {
    lambda_obs[k] = route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]] + noise_obs[k];
    }
    
richness_obs ~ poisson_log(lambda_obs);




// MAIN MODEL

for(i in 1:nst){
  for(g in 1:nreg){
    
     a[g,i] ~ std_normal();
    
  }
  
}


 b_space ~ normal(0, 0.1);
 b_time ~ normal(0, 0.1);


 sigma_n ~ student_t(4, 0, 1); // Prior for scale parameter for noise
 noise ~ normal(0, sigma_n);  // Prior for noise
 

  
  // likelihood
    for(i in 1:ncounts) {
      
    lambda[i] = a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] +  b_space[reg[i]] * space[i] * pforest[i] + obs_offset[obs[i]] + noise[i];
    }
    
richness ~ poisson_log(lambda);          
   
}

generated quantities{
  int<lower=0> y_rep[ncounts];
  
  vector[nreg]  b_dif_rg;

     for(g in 1:nreg){
         b_dif_rg[g] = b_time[g]-b_space[g];
     }
  


  // Y_rep for prior predictive check
  for(i in 1:ncounts){
  y_rep[i] = poisson_log_rng(a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] +  b_space[reg[i]] * space[i] * pforest[i] + obs_offset[obs[i]] + noise[i]);
  }
}
