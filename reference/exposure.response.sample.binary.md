# Sample simulated data for exposure-response for binary endpoint

Sample simulated data for exposure-response for binary endpoint

## Usage

``` r
exposure.response.sample.binary
```

## Format

A data frame with columns:

- conc:

  Simulated exposure

- y_logit:

  Simulated logit(p)

- y_prob:

  Simulated p

- y:

  Simulated event (1 or 0)

- y_cov_logit:

  Simulated logit(p) for model with covariate

- y_cov_prob:

  Simulated p for model with covariate

- y_cov:

  Simulated event (1 or 0) for model with covariate

- sex_num:

  1 or 0

- sex:

  female or male

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
