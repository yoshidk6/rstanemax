# Changelog

## rstanemax 0.1.10 (in progress)

## rstanemax 0.1.9

CRAN release: 2025-02-17

### Minor changes

- Define
  [`log_lik()`](https://yoshidk6.github.io/rstanemax/reference/log_lik.md)
  to calculate log likelihood for new data

## rstanemax 0.1.8

CRAN release: 2025-02-05

### Major changes

- Supporting `tidybayes` package integration such as
  `tidybayes::add_epred_draws()`
  ([@djnavarro](https://github.com/djnavarro))

### Minor changes

- Deprecate `returnType` argument for
  [`posterior_predict()`](https://yoshidk6.github.io/rstanemax/reference/posterior_predict.md)
  in favor of `tidybayes` package integration
  ([@djnavarro](https://github.com/djnavarro))

## rstanemax 0.1.7

CRAN release: 2024-12-06

### Patch release

- Fix match.arg(newDataType) for posterior_predict.stanemaxbin
- Correctly return prediction for posterior_predict.stanemaxbin

## rstanemax 0.1.6

CRAN release: 2024-12-05

### Major changes

- `as_draws_*()` functions implemented
  ([@djnavarro](https://github.com/djnavarro))
- [`stan_emax_binary()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax_binary.md)
  function implemented to perform Emax model for binary endpoint

## rstanemax 0.1.5

CRAN release: 2023-09-12

### Minor changes

- Update deprecated syntax for future rstan compatibility
  ([@andrjohns](https://github.com/andrjohns))

## rstanemax 0.1.4

CRAN release: 2023-03-12

### Minor changes

- Added
  [`extract_param()`](https://yoshidk6.github.io/rstanemax/reference/extract_param.md)
  function to easily retrieve posterior draws of key parameters
- Delegate installation to `rstantools`
  ([@andrjohns](https://github.com/andrjohns))

## rstanemax 0.1.3

CRAN release: 2020-11-24

### Minor changes

- Bug fix - disable `options(lifecycle_verbosity = "error")` to avoid
  unnecessary errors

## rstanemax 0.1.2

CRAN release: 2020-06-18

### Minor changes

- You can now fix Emax with `emax.fix` argument for
  [`stan_emax()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax.md)
  function.

## rstanemax 0.1.1

CRAN release: 2019-11-19

### Breaking changes

- For posterior prediction, the column name of the newdata needs to be
  the same as the one in the original input data, instead of `exposure`
  in the previous version.

### Major changes

- [`stan_emax()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax.md)
  can now incorporate categorical covariates in select parameters. See
  [`vignette("emaxmodel")`](https://yoshidk6.github.io/rstanemax/articles/emaxmodel.md)
  for detail.

## rstanemax 0.1.0

CRAN release: 2019-05-31

Initial CRAN release
