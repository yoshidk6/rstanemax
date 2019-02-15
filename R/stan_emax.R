
#
# data <- dplyr::tibble(a = c(4,6,7),
#                       b = c(8,11,15),
#                       c = c(23,45,76))
#
# stan_emax(b ~ a, data)
#
# modelframe <- lm(formula = b ~ a, data = data, method = "model.frame")

#' Bayesian Emax model fit with Stan
#'
#' @export
#' @param formula a symbolic description of variables for Emax model fit.
#' @param data an optional data frame containing the variables in the model.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
stan_emax <- function(formula, data, priors = NULL, ...){
  call <- match.call(expand.dots = TRUE)
  mf <- match.call(expand.dots = FALSE)

  mf[[1L]] <- as.name("lm")
  mf$method <- "model.frame"
  modelframe <- suppressWarnings(eval(mf, parent.frame()))

  mt <- attr(modelframe, "terms")
  Y <- model.response(modelframe)
  X <- model.matrix(mt, modelframe)

  browser()
  if(NCOL(Y) != 1) stop("Only one response variable is allowed")
  if(NCOL(X) != 2) stop("Only one exposure variable is allowed")

  X <- X[,2]

  standata <- list(x = X, y = Y)

  browser()

  call
  mf
}


