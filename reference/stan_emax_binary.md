# Bayesian Emax model fit with Stan for binary endpoint

Bayesian Emax model fit with Stan for binary endpoint

## Usage

``` r
stan_emax_binary(
  formula,
  data,
  gamma.fix = 1,
  e0.fix = NULL,
  emax.fix = NULL,
  priors = NULL,
  param.cov = NULL,
  ...
)
```

## Arguments

- formula:

  a symbolic description of variables for Emax model fit.

- data:

  an optional data frame containing the variables in the model.

- gamma.fix:

  a (positive) numeric or NULL to specify gamma (Hill coefficient) in
  the sigmoidal Emax model. If NULL, gamma will be estimated from the
  data. If numeric, gamma is fixed at the number provided. Default = 1
  (normal Emax model).

- e0.fix:

  a numeric or NULL to specify E0 in the Emax model. If NULL, E0 will be
  estimated from the data. If numeric, E0 is fixed at the number
  provided. Default = NULL (estimate from the data).

- emax.fix:

  a numeric or NULL to specify Emax in the Emax model. If NULL, Emax
  will be estimated from the data. If numeric, Emax is fixed at the
  number provided. Default = NULL (estimate from the data).

- priors:

  a named list specifying priors of parameters (ec50, emax, e0, gamma,
  sigma). Each list item should be length 2 numeric vector, one
  corresponding to mean and another corresponding to standard deviation.
  Currently only supports normal distribution for priors.

- param.cov:

  a named list specifying categorical covariates on parameters (ec50,
  emax, e0). Convert a column into factor if specific order of
  covariates are needed.

- ...:

  Arguments passed to rstan::sampling (e.g. iter, chains).

## Examples

``` r
if (FALSE) { # \dontrun{
data(exposure.response.sample.binary)
fit1 <- stan_emax_binary(
  y ~ conc,
  data = exposure.response.sample.binary,
  # the next line is only to make the example go fast enough
  chains = 2, iter = 500, seed = 12345
)
print(fit1)

# Specify covariates
fit2 <- stan_emax_binary(
  formula = y ~ conc, data = exposure.response.sample.binary,
  param.cov = list(emax = "sex"),
  # the next line is only to make the example go fast enough
  chains = 2, iter = 500, seed = 12345
)
print(fit2)
} # }
```
