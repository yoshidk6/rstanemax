# Convert stanemax object to a posterior draws object

Convert stanemax object to a posterior draws object

## Usage

``` r
as_draws(x, ...)

as_draws_list(x, ...)

as_draws_array(x, ...)

as_draws_df(x, ...)

as_draws_matrix(x, ...)

as_draws_rvars(x, ...)

# S3 method for class 'stanemax'
as_draws(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemaxbin'
as_draws(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemax'
as_draws_list(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemaxbin'
as_draws_list(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemax'
as_draws_array(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemaxbin'
as_draws_array(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemax'
as_draws_df(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemaxbin'
as_draws_df(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemax'
as_draws_matrix(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemaxbin'
as_draws_matrix(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemax'
as_draws_rvars(x, inc_warmup = FALSE, ...)

# S3 method for class 'stanemaxbin'
as_draws_rvars(x, inc_warmup = FALSE, ...)
```

## Arguments

- x:

  An object of class stanemax.

- ...:

  Arguments passed to individual methods (if applicable).'

- inc_warmup:

  Should warmup draws be included? Defaults to `FALSE`.

## Value

A draws object of the appropriate subclass

## See also

[`draws`](https://mc-stan.org/posterior/reference/draws.html)
[`subset_draws`](https://mc-stan.org/posterior/reference/subset_draws.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data(exposure.response.sample)
fit <- stan_emax(response ~ exposure, exposure.response.sample)
posterior::as_draws_list(fit)
posterior::as_draws_array(fit)
posterior::as_draws_df(fit)
posterior::as_draws_matrix(fit)
posterior::as_draws_rvars(fit)
} # }
```
