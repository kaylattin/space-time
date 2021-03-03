 data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nspecies;                       // Number of species total across whole dataset
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nobs;                           // number of unique observers
  int<lower=1> nst;  // spacetime levels
  

  int<lower=0> sp_reg_mat[nspecies, nreg];   // matrix of region indicators for each species, that is 0-filled for non-occupied regions
  int<lower=0> nreg_s[nspecies];             // number of reg that a species is present in
  
  
  int count[ncounts];                       // Species abundance count observations
  int<lower=0> space[ncounts];  // 0-1 indicator for space
  int<lower=0> time[ncounts];  // 0-1 indicator for time
  int species[ncounts];                     // Species 
  int reg[ncounts];                        // Regions


  int obs[ncounts];                       // observers
  real pforest[ncounts];                  // Percent forest cover
  



// OBSERVER SUB-MODEL --------------------
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
  matrix[nspecies, nreg] a_raw;                     // intercept measuring mean species abundance
  vector[nspecies] mu_a;                         // hyperparameter on mean species abundance
  vector<lower=0>[nspecies] sigma_a;             // sd - variance of each species across reg - used to shrink toward mean species abundance
  
  vector[ncounts] noise;                        // Over-dispersion noise parameter
  real<lower=0> sigma_n;                        // Variance for noise 
  
  matrix[nspecies, nreg] b_time_raw;              // z-score filled time slope estimates
  matrix[nspecies, nreg] b_space_raw;           // z-score filled space slope estimates
  vector<lower=0>[nspecies] sigma_time;
  vector<lower=0>[nspecies] sigma_space;
  vector[nspecies] B_TIME;
  vector[nspecies] B_SPACE;
  

  
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
  matrix[nspecies, nreg] a;
  matrix[nspecies, nreg] b_time;
  matrix[nspecies, nreg] b_space;

 // non-centered parameterization for slope
for(s in 1:nspecies){
  b_time[s,] =  b_time_raw[s,] * sigma_time[s] + B_TIME[s];   
 
  b_space[s,] = b_space_raw[s,] * sigma_space[s] + B_SPACE[s];

 } 
 

// non-centered parameterization for intercept

for(s in 1:nspecies){
  
  a[s,] = a_raw[s,] * sigma_a[s] + mu_a[s];
  
}



}

model {
  vector[ncounts_obs] lambda_obs;
  vector[ncounts] lambda;
  
  
// OBSERVER SUB-MODEL
sigma_n_obs ~ student_t(4, 0, 1);                 // prior for variances
sigma_e_obs ~ student_t(4, 0, 1); 
sigma_r_obs ~ student_t(4, 0, 1);
 
species_effect ~ std_normal();                // Prior for species effect - fixed
route_effect ~ normal(0, sigma_r_obs);           // Prior for bbs route effect - random
ecoreg_effect ~ normal(0, sigma_e_obs);          // Prior for ecoregion effect - random
obs_offset ~ std_normal();                    // Prior for observer offset - fixed
noise_obs ~ normal(0, sigma_n_obs);              // Prior for over-dispersion term
 
   // likelihood
    for(k in 1:ncounts_obs) {
    lambda_obs[k] = species_effect[species_obs[k]] + route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]] + noise_obs[k];
    }
    
count_obs ~ poisson_log(lambda_obs);




// MAIN MODEL 

 for(s in 1:nspecies){
   
   a_raw[s,] ~ std_normal();
   mu_a[s] ~ normal(0, 0.1);
   sigma_a[s] ~ student_t(4,0,1);
   
   b_time_raw[s,] ~ std_normal(); // prior for uncentered raw slopes, Z-score variation among regions after accounting for species mean slope
   sigma_time[s] ~ student_t(4, 0, 1);
   B_TIME[s] ~ normal(0, 0.1); // hyperprior for species mean slope
   
   b_space_raw[s,] ~ std_normal(); // space slope priors
   sigma_space[s] ~ student_t(4, 0, 1);
   B_SPACE[s] ~ normal(0, 0.1);
   
   
 }

 
 
 sigma_n ~ student_t(4, 0, 1); // Prior for scale parameter for noise
 noise ~ normal(0, sigma_n);  // Prior for noise
 

  
  // likelihood
    for(i in 1:ncounts) {
      
    lambda[i] = a[species[i], reg[i]] + b_time[species[i], reg[i]] * time[i] * pforest[i] + b_space[species[i], reg[i]] * space[i] * pforest[i] + obs_offset[obs[i]] + noise[i];
    }
    
count ~ poisson_log(lambda);          
   
}


generated quantities{
  int<lower=0> y_rep[ncounts];
  
  // Find slope values I'm after using the tracking matrix 
  // for every species, cycle through the regions that species is found in and pull out the slopevalues?
  vector[nreg] b_time_vect[nspecies];
  vector[nreg] b_space_vect[nspecies];
    
for(s in 1:nspecies){
  
  vector[nreg] b_time_index;  // 
  
  for(i in 1:nreg_s[s]){
    
    b_time_index[i] = b[1, s, sp_reg_mat[s,i]];
  }
  
  b_time_vect[s,] = b_time_index; // store slope values for comparison regions in a nspecies-long vector 
  
}
  
    // Repeat for space slope
for(s in 1:nspecies){
  
  vector[nreg] b_space_index;  // 
  
  for(i in 1:nreg_s[s]){
    
    b_space_index[i] = b[2, s, sp_reg_mat[s,i]];
  }
  
  b_space_vect[s,] = b_space_index; // store slope values for comparison regions in a nspecies-long vector 
  
}
  
  


  // Y_rep for prior predictive check
  for(i in 1:ncounts){
  y_rep[i] = poisson_log_rng(a[species[i], reg[i]] + b[spacetime[i], species[i], reg[i]] * pforest[i] + obs_offset[obs[i]] + noise[i]);
  }
}
