data{
  int<lower = 1> N;
  vector<lower = 0>[N] exposure;
  vector[N] response;

  int<lower=0,upper=1> gamma_fix_flg;
  int<lower=0,upper=1> e0_fix_flg;
  real<lower=0> gamma_fix_value;
  real e0_fix_value;

}

parameters{
  real emax;
  real<lower = 0> ec50;
  real e0_par[1-e0_fix_flg];
  real<lower = 0> gamma_par[1-gamma_fix_flg];

  real<lower = 0> sigma;
}

transformed parameters{
  vector[N] respHat;
  vector[N] exposure_exp;

  real gamma;
  real e0;

  gamma = gamma_fix_flg ? gamma_fix_value : gamma_par[1];
  e0    = e0_fix_flg    ? e0_fix_value   : e0_par[1];

  for(i in 1:N) exposure_exp[i] = exposure[i]^gamma;

  respHat = e0 + emax * exposure_exp ./ (ec50^gamma + exposure_exp);
}

model{
  response ~ normal(respHat, sigma);

  ec50 ~ normal(0, 1000);
  e0_par ~ normal(0, 10);
  gamma_par  ~ normal(0, 10);

  sigma  ~ cauchy(0, 10);
}


