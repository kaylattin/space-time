data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nobs;                           // number of unique observers
  int<lower=1> nst;
  
  int<lower=0> spacetime[ncounts];
  int ta[ncounts];   // Species ta
  int<lower=0> space[ncounts];  // 0-1 indicator for space
  int<lower=0> time[ncounts];  // 0-1 indicator for time
  int reg[ncounts];  // Regions


  int obs[ncounts];   // observers
  real pforest[ncounts];  // Percent forest cover
  
  
}



parameters {
// MAIN MODEL
  matrix[nreg, nst] a;

  vector[ncounts] noise;                        // Over-dispersion noise parameter
  real<lower=0> sigma_n;                        // Variance for noise 
  
  vector[nreg] b_space;
  vector[nreg] b_time;
  
  vector[nobs] observer;
  
}



model {
  vector[ncounts] lambda;



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
 
 observer ~ std_normal();

  
  // likelihood
    for(i in 1:ncounts) {
      
    lambda[i] = a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] +  b_space[reg[i]] * space[i] * pforest[i] + observer[obs[i]] + noise[i];
    }
    
ta ~ poisson_log(lambda);          
   
}

generated quantities{
  int<lower=0> y_rep[ncounts];
  
  vector[nreg]  b_dif_rg;

     for(g in 1:nreg){
         b_dif_rg[g] = b_time[g]-b_space[g];
     }
  


  // Y_rep for prior predictive check
  for(i in 1:ncounts){
  y_rep[i] = poisson_log_rng(a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] +  b_space[reg[i]] * space[i] * pforest[i] + observer[obs[i]] + noise[i]);
  }
}
