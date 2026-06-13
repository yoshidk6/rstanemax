# Calculate log-likelihoods from posterior samples. Data can be either original data used for model fit or new data.

See
[rstantools::log_lik](https://mc-stan.org/rstantools/reference/log_lik.html)
for more details.

## Usage

``` r
log_lik(object, ...)

# S3 method for class 'stanemax'
log_lik(object, newdata = NULL, ...)

# S3 method for class 'stanemaxbin'
log_lik(object, newdata = NULL, ...)
```

## Arguments

- object:

  A `stanemax` or `stanemaxbin` object

- ...:

  Currently unused arguments

- newdata:

  New data used for prediction. If NULL, original data is used.

## Value

\\S\\ by \\N\\ matrix of log-likelihoods, where each row corresponds to
a draw from the posterior distribution and each column corresponds to a
data point.
