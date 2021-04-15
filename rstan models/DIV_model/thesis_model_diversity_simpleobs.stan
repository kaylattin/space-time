data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nst;
  int<lower=1> nobs;
  
  real diversity[ncounts];   // Shannon diversity
  int<lower=0> spacetime[ncounts];
  int<lower=0> space[ncounts];  // 0-1 indicator for space
  int<lower=0> time[ncounts];  // 0-1 indicator for time
  int reg[ncounts];  // Regions

  int obs[ncounts];
  real pforest[ncounts];  // Percent forest cover
  

}


parameters {
// MAIN MODEL
  matrix[nreg, nst] a;
  real<lower=0> sigma;

  vector[nreg] b_space;
  vector[nreg] b_time;
  
  vector[nobs] observer;
  

}


model {
  vector[ncounts] mu;

   observer ~ std_normal();
// MAIN MODEL 
   to_vector(a) ~ std_normal();
   b_space ~ normal(0, 0.1);
   b_time ~ normal(0, 0.1);


 sigma ~ student_t(4, 0, 1);
  
  // likelihood
    for(i in 1:ncounts) {
      
    mu[i] = a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] + b_space[reg[i]] * space[i] * pforest[i] + observer[obs[i]];
    }
    
diversity ~ normal(mu, sigma);          
   
}

generated quantities{
  real y_rep[ncounts];
  
  
  vector[nreg]  b_dif_rg;

     for(g in 1:nreg){
         b_dif_rg[g] = b_time[g]-b_space[g];
     }
  


  // Y_rep for prior predictive check
  for(i in 1:ncounts){
  y_rep[i] = normal_rng(a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] + b_space[reg[i]] * space[i] * pforest[i], sigma);
  }
}