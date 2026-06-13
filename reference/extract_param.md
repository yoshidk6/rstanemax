# Extract posterior draws of key parameters

Extract posterior draws of key parameters

## Usage

``` r
extract_param(object)
```

## Arguments

- object:

  A `stanemax` class object

## Value

A tibble containing posterior draws of key parameters. If covariate(s)
are included in the model, posterior draws for different combinations of
covariates are supplied in a long format - e.g. if there are posterior
draws of 100 samples and 4 levels of the covariates, the returned tibble
will have the length of 400
