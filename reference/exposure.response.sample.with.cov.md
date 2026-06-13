# Sample simulated data for exposure-response with covariates

Sample simulated data for exposure-response with covariates

## Usage

``` r
exposure.response.sample.with.cov
```

## Format

A data frame with columns:

- dose:

  Dose levels used for simulation of pharmacokinetics

- conc:

  Simulated exposure

- resp:

  Simulated pharmacodynamic response

- cov1:

  Covariate 1 for e0

- cov2:

  Covariate 2 for emax

- cov3:

  Covariate 3 for ec50 (data type factor)

- cov3num:

  Covariate 3 for ec50 (data type numeric)

## Examples

``` r
exposure.response.sample
#> # A tibble: 60 × 3
#>     dose exposure response
#>    <dbl>    <dbl>    <dbl>
#>  1     0        0    17.2 
#>  2     0        0     9.78
#>  3     0        0    -8.81
#>  4     0        0     7.68
#>  5     0        0    -6.30
#>  6     0        0    14.8 
#>  7     0        0   -18.5 
#>  8     0        0    17.3 
#>  9     0        0    15.0 
#> 10     0        0    11.3 
#> # ℹ 50 more rows
```
