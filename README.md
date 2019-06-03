
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
#> Chain 1:  Elapsed Time: 0.284 seconds (Warm-up)
#> Chain 1:                0.102 seconds (Sampling)
#> Chain 1:                0.386 seconds (Total)
#> Chain 1:
```

``` r
fit.emax
#> Inference for Stan model: emax.
#> 1 chains, each with iter=500; warmup=250; thin=1; 
#> post-warmup draws per chain=250, total post-warmup draws=250.
#> 
#>         mean se_mean    sd  2.5%    25%    50%    75%  97.5% n_eff Rhat
#> emax  107.33    0.32  4.65 98.67 103.91 106.99 110.80 116.55   212    1
#> e0      6.23    0.27  3.71 -0.33   3.66   6.15   8.87  14.12   190    1
#> ec50   98.72    1.56 20.47 67.31  83.27  95.45 111.43 144.57   172    1
#> gamma   1.00     NaN  0.00  1.00   1.00   1.00   1.00   1.00   NaN  NaN
#> sigma  19.99    0.09  1.32 17.64  19.12  19.91  20.78  23.15   204    1
#> 
#> Samples were drawn using NUTS(diag_e) at Mon Jun 03 14:59:47 2019.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
```

``` r
plot(fit.emax)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
