#' Bayesian Emax model fit with Stan
#'
#' Add explanations
#'
#' @export
#' @param formula a symbolic description of variables for Emax model fit.
#' @param data an optional data frame containing the variables in the model.
#' @param priors a list specifying priors of parameters.
#' Not implemented yet.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanemax`
#' @examples
#' data(exposure.response.sample)
#' ggplot2::qplot(exposure, response, data = exposure.response.sample)
#'
#' df <- data.frame(a = rnorm(10), b = rnorm(10))
#' fit <- lm_stan_formula(b ~ a, data = df)
#' print(fit)
#'
#' \dontrun{
#' data(exposure.response.sample)
#' fit <- stan_emax(response ~ exposure, data = exposure.response.sample)
#'
#' }
#'
stan_emax <- function(formula, data, priors = NULL, ...){
  # Parse formula and put together stan data object
  # Actual run is in `stan_emax_run` function

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

  standata <- list(exposure = X, response = Y, N = length(Y))
  out <- stan_emax_run(stanmodels$mod_emax_e0, standata = standata, ...)
}


#' Run Emax model
#'
#' Add explanations
#'
#' @rdname stan_emax
#' @param stanmodel a Stan model object.
#' @param standata a data file for model fit
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#'
stan_emax_run <- function(stanmodel, standata, ...){
  # Run stan model and prepare `stanemax` object

  stanfit <- rstan::sampling(stanmodel, data = standata, ...)

  out <- list(stanfit = stanfit,
              standata= standata)

  structure(out, class = c("stanemax"))

}




