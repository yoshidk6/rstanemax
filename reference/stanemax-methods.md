# Methods for stanemax objects

Methods for stanemax objects

## Usage

``` r
# S3 method for class 'stanemax'
print(x, digits_summary = 2, ...)

# S3 method for class 'stanemaxbin'
print(x, digits_summary = 2, ...)

extract_stanfit(x)

extract_obs_mod_frame(x)

# S3 method for class 'stanemax'
plot(x, show.ci = TRUE, show.pi = FALSE, ci = 0.9, pi = 0.9, ...)

# S3 method for class 'stanemaxbin'
plot(x, show.ci = TRUE, show.pi = FALSE, ci = 0.9, pi = 0.9, ...)
```

## Arguments

- x:

  An object of class `stanemax` or `stanemaxbin`

- digits_summary:

  The number of significant digits to use when printing the summary,
  defaulting to 2. Applies to the quantities other than the effective
  sample size, which is always rounded to the nearest integer.

- ...:

  Additional arguments passed to methods.

- show.ci:

  An logical specifying if the output figure include credible interval
  of posterior prediction. Default TRUE.

- show.pi:

  An logical specifying if the output figure include prediction
  interval. Default FALSE.

- ci:

  Credible interval range.

- pi:

  Prediction interval range.
