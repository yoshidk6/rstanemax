# Outcome prediction from posterior distribution of parameters

Compute outcome predictions using posterior samples. Exposure data for
prediction can be either original data used for model fit or new data.

## Usage

``` r
posterior_predict(object, ...)

posterior_epred(object, ...)

posterior_linpred(object, transform = FALSE, ...)

# S3 method for class 'stanemax'
posterior_predict(
  object,
  newdata = NULL,
  returnType = "matrix",
  newDataType = "raw",
  ...
)

# S3 method for class 'stanemaxbin'
posterior_predict(
  object,
  newdata = NULL,
  returnType = "matrix",
  newDataType = "raw",
  ...
)

# S3 method for class 'stanemax'
posterior_epred(object, newdata = NULL, newDataType = "raw", ...)

# S3 method for class 'stanemaxbin'
posterior_epred(object, newdata = NULL, newDataType = "raw", ...)

# S3 method for class 'stanemax'
posterior_linpred(
  object,
  transform = FALSE,
  newdata = NULL,
  newDataType = "raw",
  ...
)

# S3 method for class 'stanemaxbin'
posterior_linpred(
  object,
  transform = FALSE,
  newdata = NULL,
  newDataType = "raw",
  ...
)

posterior_predict_quantile(
  object,
  newdata = NULL,
  ci = 0.9,
  pi = 0.9,
  newDataType = c("raw", "modelframe")
)
```

## Arguments

- object:

  A `stanemax` or `stanemaxbin` object

- ...:

  Additional arguments passed to methods. Arguments that can be passed
  via the dots include `ndraws`, for compatibility with `rstantools`
  conventions

- transform:

  Should the linear predictor be transformed to response scale?

- newdata:

  An optional data frame that contains columns needed for model to run
  (exposure and covariates). If the model does not have any covariate,
  this can be a numeric vector corresponding to the exposure metric.

- returnType:

  **\[deprecated\]** An optional string specifying the type of return
  object (one of "matrix", "dataframe", or "tibble")

- newDataType:

  An optional string specifying the type of newdata input, whether in
  the format of an original data frame ("raw", the default) or a
  processed model frame ("modelframe"). Mostly used for internal
  purposes and users can usually leave at default.

- ci:

  Credible interval of the response without residual variability.

- pi:

  Prediction interval of the response with residual variability.

## Value

An object that contain predicted response with posterior distribution of
parameters. The default is a matrix containing predicted `response` for
[`stan_emax()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax.md)
and `.epred` for
[`stan_emax_binary()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax_binary.md).
Each row of the matrix is a vector of predictions generated using a
single draw of the model parameters from the posterior distribution.

If either `dataframe` or `tibble` is specified, the function returns a
data frame or tibble object in a long format - each row is a prediction
generated using a single draw of the model parameters and a
corresponding exposure.

Several types of predictions are generated with this function.

For continuous endpoint model
([`stan_emax()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax.md)),

- `.linpred` & `.epred`: prediction without considering residual
  variability and is intended to provide credible interval of "mean"
  response.

- `.prediction`: include residual variability in its calculation,
  therefore the range represents prediction interval of observed
  response.

- **\[deprecated\]** `respHat`: replaced by `.linpred` and `.epred`

- **\[deprecated\]** `response`: replaced by `.prediction`

For binary endpoint model
([`stan_emax_binary()`](https://yoshidk6.github.io/rstanemax/reference/stan_emax_binary.md)),

- `.linpred`: predicted probability on logit scale

- `.epred`: predicted probability on probability scale

- `.prediction`: predicted event (1) or non-event (0)

  The return object also contains exposure and parameter values used for
  calculation.

With `posterior_predict_quantile()` function, you can obtain quantiles
of `respHat` and `response` as specified by `ci` and `pi`.

## Details

Run
[`vignette("emaxmodel", package = "rstanemax")`](https://yoshidk6.github.io/rstanemax/articles/emaxmodel.md)
to see how you can use the posterior prediction for plotting estimated
Emax curve.
