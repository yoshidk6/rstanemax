[![Travis-CI Build Status](https://travis-ci.org/yoshidk6/rstanemax.svg?branch=master)](https://travis-ci.org/yoshidk6/rstanemax)


# Introduction to __rstanemax__

This small package performs simple sigmoidal Emax model fit using Stan, without the need of writing Stan model code, inspired by __rstanarm__ package.

__rstanarm__ package ([link](https://mc-stan.org/users/interfaces/rstanarm)) is a very flexible, general purpose tool to perform various Bayesian modeling with formula notations, such as generalized mixed effect models or joint models.
One small gap it has is nonlinear model fitting, where it only accepts nonlinear functions defined in stats package with `SS` prefixes ([link](http://mc-stan.org/rstanarm/articles/glmer.html#relationship-to-nlmer)). 
Unfortunately the (sigmoidal) Emax model, one of the most commonly used nonlinear functions in the field of pharmacometrics, is not among the available functions.
The __rstanarm__ package also seems to be assuming that we fit nonlinear mixed effect models, but not simple nonlinear models without mixed effects. 

I hope this __rstanemax__ package will fill the niche gap, allow for easier implementation of Emax model in Bayesian framework, and enable routine uses in the pharmacokinetic/pharmacodynamic field.


# Installation
Before installing this package from source, you first have to install RStan and C++ Toolchain.  
[RStan Getting Started](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)  
Also, you have to follow the instruction below if you are using Windows PC.  
[Installing RStan from source on Windows](https://github.com/stan-dev/rstan/wiki/Installing-RStan-from-source-on-Windows)  

After this step you should be able to install the package from GitHub using __devtools__.

```
install.packages(c("devtools"))
library(devtools)
devtools::install_github("yoshidk6/rstanemax")
```

See [this blog post](http://yoshidk6.hatenablog.com/entry/2019/02/19/061100) (written in Japanese) if the above process doesn't work. 


# How to use

[This GitHub pages](https://yoshidk6.github.io/rstanemax) contains function references and vignette.


## Load __rstanemax__
```
library(rstanemax)
```

## Run model with a sample dataset

```
data(exposure.response.sample)

fit.emax <- stan_emax(response ~ exposure, data = exposure.response.sample)

fit.emax
```




