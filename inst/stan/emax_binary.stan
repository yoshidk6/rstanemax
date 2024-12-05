data{
  int<lower = 1> N;
  vector<lower = 0>[N] exposure;
  array[N] int<lower=0, upper=1> response;

  // Covariates
  array[N] int<lower = 1> covemax;
  array[N] int<lower = 1> covec50;
  array[N] int<lower = 1> cove0;
  int<lower = 1> n_covlev_emax;
  int<lower = 1> n_covlev_ec50;
  int<lower = 1> n_covlev_e0;

  // Fixed parameters
  int<lower=0,upper=1> gamma_fix_flg;
  int<lower=0,upper=1> e0_fix_flg;
  int<lower=0,upper=1> emax_fix_flg;
  real<lower=0> gamma_fix_value;
  real e0_fix_value;
  real emax_fix_value;

  // priors
  //// mu
  real prior_emax_mu;
  real<lower=0> prior_ec50_mu;
  real<lower=0> prior_gamma_mu;
  real prior_e0_mu;
  //// sigma
  real<lower=0> prior_emax_sig;
  real<lower=0> prior_ec50_sig;
  real<lower=0> prior_gamma_sig;
  real<lower=0> prior_e0_sig;
}

parameters{
  // vector[n_covlev_emax] emax;
  vector<lower = 0>[n_covlev_ec50] ec50;
  // array[1-e0_fix_flg] vector[n_covlev_e0] e0_par;
  array[n_covlev_e0, 1-e0_fix_flg] real e0_par;
  array[n_covlev_emax, 1-emax_fix_flg] real emax_par;
  array[1-gamma_fix_flg] real<lower = 0> gamma_par;
}

transformed parameters{
  vector[N] mu_logit;
  vector[N] exposure_exp;

  real gamma;
  vector[n_covlev_e0] e0;
  vector[n_covlev_emax] emax;

  vector[N] emaxvec;
  vector[N] ec50vec;
  vector[N] ec50vec_exp;
  vector[N] e0vec;

  // Prep gamma and e0 - fixed or not fixed
  gamma = gamma_fix_flg ? gamma_fix_value : gamma_par[1];
  for(i in 1:n_covlev_e0) e0[i] = e0_fix_flg ? e0_fix_value : e0_par[i, 1];
  for(i in 1:n_covlev_emax) emax[i] = emax_fix_flg ? emax_fix_value : emax_par[i, 1];

  // Prep param for each individual
  emaxvec = emax[covemax];
  ec50vec = ec50[covec50];
  e0vec   = e0[cove0];

  for(i in 1:N) {
    exposure_exp[i] = exposure[i]^gamma;
    ec50vec_exp[i]  = ec50vec[i]^gamma;
  }

  mu_logit = e0vec + emaxvec .* exposure_exp ./ (ec50vec_exp + exposure_exp);
}

model{
  response ~ bernoulli_logit(mu_logit);

  // emax       ~ normal(prior_emax_mu,  prior_emax_sig);
  ec50       ~ normal(prior_ec50_mu,  prior_ec50_sig);
  gamma_par  ~ normal(prior_gamma_mu, prior_gamma_sig);
  for(i in 1:n_covlev_e0) {
    e0_par[i] ~ normal(prior_e0_mu,    prior_e0_sig);
  }
  for(i in 1:n_covlev_emax) {
    emax_par[i] ~ normal(prior_emax_mu,    prior_emax_sig);
  }
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(response[n] | mu_logit[n]);
  }
}
