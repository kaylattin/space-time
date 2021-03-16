 data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nspecies;                       // Number of species total across whole dataset
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nobs;                           // number of unique observers


  int<lower=0> sp_reg_inclusion[nspecies, nreg];   // matrix of 0-1 indicators
  int<lower=0> sp_reg_mat[nspecies, nreg];  // matrix of regions for each species
  int<lower=0> reg_sp_mat[nreg, nspecies]; // matrix of species for each region
  int<lower=0> nreg_s[nspecies];    // number of reg that a species is present in
  int<lower=0> nsp_r[nreg]; // number of species in each region
  
  
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
  real<lower=0> sdnoise;                        // Variance for noise 
  
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
  
  real<lower=0> sdnoise_obs;                    // Variance for noise
  real<lower=0> sdeco_obs;                    // Variance for ecoregion
  real<lower=0> sdrte_obs;                    // Variance for route
  
}

transformed parameters{
  matrix[nspecies, nreg] a;
  matrix[nspecies, nreg] b_time;
  matrix[nspecies, nreg] b_space;


 for(s in 1:nspecies){
   
   for(g in 1:nreg){
     
       // if species-reg combo exists in dataset, pool information across
      if(sp_reg_inclusion[s,g] == 1){
        
        b_time[s,g] =  b_time_raw[s,g] * sigma_time[s] + B_TIME[s];   
        
        b_space[s,g] = b_space_raw[s,g] * sigma_space[s] + B_SPACE[s];
  
      }else{
        
        // if species-reg combo doesn't exist in dataset, treat as a fixed effect (should only be informed by the prior)
        b_time[s,g] = 0;
        
        b_space[s,g] = 0;
        
        
      }
   }
 }

// non-centered parameterization for intercept

 for(s in 1:nspecies){
   
   for(g in 1:nreg){
         // if species-reg combo exists in dataset, pool information across
      if(sp_reg_inclusion[s,g] == 1){
       
      a[s,g] = a_raw[s,g] * sigma_a[s] + mu_a[s];
      
      }else{
        
        a[s,g] = 0;
        
        
      }
   }
 }
}


model {
  vector[ncounts_obs] lambda_obs;
  vector[ncounts] lambda;
  
  
// OBSERVER SUB-MODEL
sdnoise_obs ~ student_t(20, 0, 1);                 // prior for variances
sdeco_obs ~ student_t(20, 0, 1); 
sdrte_obs ~ student_t(20, 0, 1);
 
species_effect ~ normal(0, 0.1);                // Prior for species effect - fixed
route_effect ~ normal(0, sdrte_obs);           // Prior for bbs route effect - random
ecoreg_effect ~ normal(0, sdeco_obs);          // Prior for ecoregion effect - random
obs_offset ~ normal(0, 0.1);                // Prior for observer offset - fixed
noise_obs ~ normal(0, sdnoise_obs);              // Prior for over-dispersion term
 
   // likelihood
    for(k in 1:ncounts_obs) {
    lambda_obs[k] = species_effect[species_obs[k]] + route_effect[route_obs[k]] + ecoreg_effect[ecoreg_obs[k]] + obs_offset[obs_obs[k]] + noise_obs[k];
    }
    
count_obs ~ poisson_log(lambda_obs);




// MAIN MODEL 

 for(s in 1:nspecies){
   
   a_raw[s,] ~ std_normal();
   mu_a[s] ~ normal(0, 0.1);
   sigma_a[s] ~ student_t(20,0,1);
   
   b_time_raw[s,] ~ std_normal(); // prior for uncentered raw slopes, Z-score variation among regions after accounting for species mean slope
   sigma_time[s] ~ student_t(20, 0, 1);
   B_TIME[s] ~ normal(0, 0.1); // hyperprior for species mean slope
   
   b_space_raw[s,] ~ std_normal(); // space slope priors
   sigma_space[s] ~ student_t(20, 0, 1);
   B_SPACE[s] ~ normal(0, 0.1);
   
   
 }

 
 
 sdnoise ~ student_t(20, 0, 1); // Prior for scale parameter for noise
 noise ~ normal(0, sdnoise);  // Prior for noise
 

  
  // likelihood
    for(i in 1:ncounts) {
      
    lambda[i] = a[species[i], reg[i]] + b_time[species[i], reg[i]] * time[i] * pforest[i] + b_space[species[i], reg[i]] * space[i] * pforest[i] + obs_offset[obs[i]] + noise[i];
    }
    
count ~ poisson_log(lambda);          
   
}


generated quantities{
  int<lower=0> y_rep[ncounts];  // y_rep stores posterior predictive draws
  matrix[nspecies, nreg]  b_dif_sp;
  vector[nspecies] b_dif_sp_mean;
  matrix[nspecies, nreg] b_dif_reg;
  vector[nreg] b_dif_reg_mean;
    
// differences for each species summarized across regions
  for(s in 1:nspecies){
     for(g in 1:nreg){
       if(sp_reg_inclusion[s,g] == 1){
         b_dif_sp[s,g] = b_time[s,g]-b_space[s,g];
       }else{
         b_dif_sp[s,g] = 0;
       }
       
     }
    
    
     
    
    
    b_dif_sp_mean[s] = mean(b_dif_sp[s,sp_reg_mat[s,1:nreg_s[s]]]); // uses only the regions included for a given species to calculation the means

  }
  

  for(g in 1:nreg){
     for(s in 1:nspecies){
       if(sp_reg_inclusion[s,g] == 1){
         b_dif_reg[s,g] = b_time[s,g]-b_space[s,g];
       }else{
         b_dif_reg[s,g] = 0;
       }
       
     }
    

    b_dif_reg_mean[g] = mean(b_dif_reg[reg_sp_mat[g, 1:nsp_r[g]], g]); // uses only the species in that region to calcualte means
  }
  


  

  // Y_rep for prior predictive check
  for(i in 1:ncounts){
  y_rep[i] = poisson_log_rng(a[species[i], reg[i]] + b_time[species[i], reg[i]] * time[i] * pforest[i] + b_space[species[i], reg[i]] * space[i] * pforest[i] + obs_offset[obs[i]] + noise[i]);
  }
  
}