data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nobs;                           // number of unique observers
  
  int diversity[ncounts];   // Shannon diversity
  int<lower=0> space[ncounts];  // 0-1 indicator for space
  int<lower=0> time[ncounts];  // 0-1 indicator for time
  int reg[ncounts];  // Regions


  int obs[ncounts];   // observers
  real pforest[ncounts];  // Percent forest cover
  



// OBSERVER SUB-MODEL --------------------
  // groups and counts
  int<lower=1> ncounts_obs;                     // Number of observations in observer dataset
  int<lower=1> nspecies_obs;                    // Number of unique species in observer dataset
  int<lower=1> nroutes_obs;                     // Number of bbs routes in observer dataset
  int<lower=1> necoreg_obs;                     // Number of unique ecoregions in observer dataset
  int<lower=1> nobs_obs;                        // Number of unique observers in observer dataset
  
  // observed data
  int diversity_obs[ncounts_obs];   // Shannon diversity
  int route_obs[ncounts_obs];    // bbs route
  int ecoreg_obs[ncounts_obs];   // Ecoregion
  int obs_obs[ncounts_obs];   // bbs observer
  
}


parameters {
// MAIN MODEL
  vector[nreg] a_raw;   // intercept measuring mean total abundance in a given comparison region
  vector[nreg] mu_a;  // hyperparameter on mean total abundance
  vector<lower=0>[nreg] sigma_a; // sd - variance of total abundance across all regions
  
  vector[ncounts] noise;                        // Over-dispersion noise parameter
  real<lower=0> sigma_n;                        // Variance for noise 
  
  vector[nreg] b_time_raw;              // z-score filled time slope estimates
  vector[nreg] b_space_raw;           // z-score filled space slope estimates
  vector<lower=0>[nreg] sigma_time;
  vector<lower=0>[nreg] sigma_space;
  vector[nreg] B_TIME;
  vector[nreg] B_SPACE;
  

  
// OBSERVER SUB-MODEL
  
  vector[nroutes_obs] route_effect;             // Route effect on counts
  vector[necoreg_obs] ecoreg_effect;            // Ecoregion effect on counts
  vector[nobs_obs] obs_offset;                  // Unique offset term for each observer to feed into main model
  vector[ncounts_obs] noise_obs;                // Over-dispersion term for observer sub-model
  
  real<lower=0> sigma_n_obs;                    // Variance for noise
  real<lower=0> sigma_e_obs;                    // Variance for ecoregion
  real<lower=0> sigma_r_obs;                    // Variance for route
  
}

transformed parameters{
  vector[nreg] a;
  vector[nreg] b_time;
  vector[nreg] b_space;

 // non-centered parameterization for slope
  b_time =  b_time_raw * sigma_time + B_TIME;   
 
  b_space = b_space_raw * sigma_space + B_SPACE;

 

// non-centered parameterization for intercept
  
  a = a_raw * sigma_a + mu_a;
  



}

model {
  vector[ncounts_obs] mu_obs;
  vector[ncounts_obs] sigma_obs;
  vector[ncounts] mu;
  vector[ncounts] sigma;
  
  
// OBSERVER SUB-MODEL
sigma_e_obs ~ student_t(4, 0, 1); 
sigma_r_obs ~ student_t(4, 0, 1);
 
route_effect ~ normal(0, sigma_r_obs);           // Prior for bbs route effect - random
ecoreg_effect ~ normal(0, sigma_e_obs);          // Prior for ecoregion effect - random
obs_offset ~ std_normal();                    // Prior for observer offset - fixed

sigma_obs ~ exponential(1);

   // likelihood
    for(k in 1:ncounts_obs) {
    mu_obs[k] = route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]];
    }
    
diversity_obs ~ normal(mu_obs, sigma_obs);




// MAIN MODEL 

   a_raw ~ std_normal();
   mu_a ~ normal(0, 0.1);
   sigma_a ~ student_t(4,0,1);
   
   b_time_raw ~ std_normal(); // prior for uncentered raw slopes, Z-score variation among regions after accounting for species mean slope
   sigma_time ~ student_t(4, 0, 1);
   B_TIME ~ normal(0, 0.1); // hyperprior for species mean slope
   
   b_space_raw ~ std_normal(); // space slope priors
   sigma_space ~ student_t(4, 0, 1);
   B_SPACE ~ normal(0, 0.1);
  

 sigma ~ exponential(1);
  
  // likelihood
    for(i in 1:ncounts) {
      
    mu[i] = a[reg[i]] + b_time[reg[i]] * time[i] * pforest[i] + b_space[reg[i]] * space[i] * pforest[i] + obs_offset[obs[i]];
    }
    
diversity ~ normal(mu, sigma);          
   
}

generated quantities{
  int<lower=0> y_rep[ncounts];
  

  // Y_rep for prior predictive check
  for(i in 1:ncounts){
  y_rep[i] = normal_rng(a[reg[i]] + b_time[reg[i]] * time[i] * pforest[i] + b_space[reg[i]] * space[i] * pforest[i] + obs_offset[obs[i]]);
  }
}
