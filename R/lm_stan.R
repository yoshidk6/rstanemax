# Save this file as `R/lm_stan.R`

#' Bayesian linear regression with Stan
#'
#' @export
#' @param x Numeric vector of input values.
#' @param y Numberic vector of output values.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
lm_stan <- function(x, y, ...) {
  standata <- list(x = x, y = y, N = length(y))
  out <- rstan::sampling(stanmodels$lm, data = standata, ...)
  return(out)
}


#' Bayesian linear regression with Stan using formula notation
#'
#' @export
#' @param formula a symbolic description of variables for Emax model fit.
#' @param data an optional data frame containing the variables in the model.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#' @examples
#' sum(1:10)
#'
#' \dontrun{
#' df <- data.frame(a = rnorm(10), b = rnorm(10))
#' fit <- lm_stan_formula(b ~ a, data = df)
#'
#' }
#'
lm_stan_formula <- function(formula, data, ...){
  call <- match.call(expand.dots = TRUE)
  mf <- match.call(expand.dots = FALSE)

  mf[[1L]] <- as.name("lm")
  mf$method <- "model.frame"
  modelframe <- suppressWarnings(eval(mf, parent.frame()))

  mt <- attr(modelframe, "terms")
  Y <- model.response(modelframe)
  X <- model.matrix(mt, modelframe)

  if(NCOL(Y) != 1) stop("Only one response variable is allowed")
  if(NCOL(X) != 2) stop("Only one exposure variable is allowed")

  X <- X[,2]

  standata <- list(x = X, y = Y, N = length(Y))
  out <- rstan::sampling(stanmodels$lm, data = standata, ...)
}
