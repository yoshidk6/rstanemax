
<!-- README.md is generated from README.Rmd. Please edit that file -->
rstanemax
=========

[![Travis-CI Build Status](https://travis-ci.org/yoshidk6/rstanemax.svg?branch=master)](https://travis-ci.org/yoshidk6/rstanemax) [![CRAN status](https://www.r-pkg.org/badges/version/rstanemax)](https://cran.r-project.org/package=rstanemax) [![downloads](https://cranlogs.r-pkg.org/badges/grand-total/rstanemax)](https://CRAN.R-project.org/package=rstanemax)

This small package performs simple sigmoidal Emax model fit using Stan, without the need of writing Stan model code, inspired by **rstanarm** package.

**rstanarm** package ([link](https://mc-stan.org/users/interfaces/rstanarm)) is a very flexible, general purpose tool to perform various Bayesian modeling with formula notations, such as generalized mixed effect models or joint models. One small gap it has is in nonlinear model fitting, where it only accepts nonlinear functions defined in stats package with `SS` prefixes ([link](http://mc-stan.org/rstanarm/articles/glmer.html#relationship-to-nlmer)). Unfortunately the (sigmoidal) Emax model, one of the most commonly used nonlinear functions in the field of pharmacometrics, is not among the available functions. The **rstanarm** package also seems to be assuming that we fit nonlinear mixed effect models, but not simple nonlinear models without mixed effects.

I hope this **rstanemax** package will fill the niche gap, allow for easier implementation of Emax model in Bayesian framework, and enable routine uses in the pharmacokinetic/pharmacodynamic field.

This package was build using **rstantools** ([link](https://mc-stan.org/rstantools/)) following a very helpful step-by-step guide ([link](https://mc-stan.org/rstantools/articles/minimal-rstan-package.html)) on creating a package that depends on RStan.

Installation
------------

### From CRAN

You can install the released version of rstanemax from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rstanemax")
```

### From source

You can alternatively install the package from source.
Before doing so, you first have to install RStan and C++ Toolchain.
[RStan Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)
Also, you have to follow the instruction below if you are using Windows PC.
[Installing RStan from source on Windows](https://github.com/stan-dev/rstan/wiki/Installing-RStan-from-source-on-Windows)

After this step you should be able to install the package from GitHub using **devtools**.

``` r
install.packages("devtools")
library(devtools)
devtools::install_github("yoshidk6/rstanemax")
```

Example
-------

[This GitHub pages](https://yoshidk6.github.io/rstanemax) contains function references and vignette.

``` r
# Load rstanemax
library(rstanemax)
#> Loading required package: Rcpp
#> Warning: package 'Rcpp' was built under R version 3.5.3

# Run model with a sample dataset
set.seed(12345)

data(exposure.response.sample)

fit.emax <- stan_emax(response ~ exposure, data = exposure.response.sample,
                      # the next line is only to make the output short
                      chains = 1, iter = 500, seed = 12345)
#> 
#> SAMPLING FOR MODEL 'emax' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 0 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:   1 / 500 [  0%]  (Warmup)
#> Chain 1: Iteration:  50 / 500 [ 10%]  (Warmup)
#> Chain 1: Iteration: 100 / 500 [ 20%]  (Warmup)
#> Chain 1: Iteration: 150 / 500 [ 30%]  (Warmup)
#> Chain 1: Iteration: 200 / 500 [ 40%]  (Warmup)
#> Chain 1: Iteration: 250 / 500 [ 50%]  (Warmup)
#> Chain 1: Iteration: 251 / 500 [ 50%]  (Sampling)
#> Chain 1: Iteration: 300 / 500 [ 60%]  (Sampling)
#> Chain 1: Iteration: 350 / 500 [ 70%]  (Sampling)
#> Chain 1: Iteration: 400 / 500 [ 80%]  (Sampling)
#> Chain 1: Iteration: 450 / 500 [ 90%]  (Sampling)
#> Chain 1: Iteration: 500 / 500 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.305 seconds (Warm-up)
#> Chain 1:                0.088 seconds (Sampling)
#> Chain 1:                0.393 seconds (Total)
#> Chain 1:
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> http://mc-stan.org/misc/warnings.html#tail-ess
```

``` r
fit.emax
#> ---- Emax model fit with rstanemax ----
#> 
#>        mean se_mean    sd  2.5%   25%   50%   75%  97.5%  n_eff Rhat
#> emax  92.21    0.57  6.17 79.44 88.09 92.63 96.96 102.45 118.05    1
#> e0     5.59    0.43  4.38 -2.89  2.60  5.79  8.69  13.94 104.78    1
#> ec50  74.15    1.09 17.24 45.44 62.52 72.85 84.23 108.22 249.22    1
#> gamma  1.00     NaN  0.00  1.00  1.00  1.00  1.00   1.00    NaN  NaN
#> sigma 16.52    0.12  1.44 14.01 15.49 16.47 17.54  19.36 155.65    1
#> 
#> * Use `extract_stanfit()` function to extract raw stanfit object
#> * Use `plot()` function to visualize model fit
#> * Use `posterior_predict()` or `posterior_predict_quantile()` function to get
#>   raw predictions or make predictions on new data
#> * Use `extract_obs_mod_frame()` function to extract raw data 
#>   in a processed format (useful for plotting)
```

``` r
plot(fit.emax)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
