data{
  int<lower = 1> N;
  vector<lower = 0>[N] exposure; 
  vector[N] response;
}

parameters{
  real emax; 
  real<lower = 0> ec50; 
  real e0; 
  // real<lower = 0> gamma; 
  
  real<lower = 0> sigma;
}

transformed parameters{
  vector[N] respHat;
  vector[N] exposure_exp;
  
  real gamma;
  
  gamma = 1;
  
  for(i in 1:N) exposure_exp[i] = exposure[i]^gamma;
  
  respHat = e0 + emax * exposure_exp ./ (ec50^gamma + exposure_exp);
}

model{
  response ~ normal(respHat, sigma); 
  e0 ~ normal(0, 10); 
  ec50 ~ normal(0, 1000); 
  // gamma  ~ normal(0, 10); 
  
  sigma  ~ cauchy(0, 10);
}


