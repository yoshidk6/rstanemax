#' Bayesian Emax model fit with Stan
#'
#' Run sigmoidal Emax model fit with formula notation
#'
#' @export
#' @param formula a symbolic description of variables for Emax model fit.
#' @param data an optional data frame containing the variables in the model.
#' @param gamma.fix a numeric or NULL to specify gamma (Hill coefficient) in the sigmoidal Emax model.
#' If NULL, gamma will be estimated from the data.
#' If numeric, gamma is fixed at the number provided.
#' Default = 1 (normal Emax model).
#' @param e0.fix a numeric or NULL to specify E0 in the Emax model.
#' If NULL, E0 will be estimated from the data.
#' If numeric, E0 is fixed at the number provided.
#' Default = NULL (estimate from the data).
#' @param priors a named list specifying priors of parameters (ec50, emax, e0, gamma, sigma).
#' Each list item should be length 2 numeric vector, one corresponding to mean and
#' another corresponding to standard deviation.
#' Currently only supports normal distribution for priors.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanemax`
#' @examples
#' data(exposure.response.sample)
#' fit1 <- stan_emax(response ~ exposure, data = exposure.response.sample,
#'                   # the next line is only to make the example go fast enough
#'                   chains = 1, iter = 500, seed = 12345)
#' print(fit1)
#'
#' \dontrun{
#' # Set priors manually, also estimate gamma instead of the default of fix to 1
#' fit2 <- stan_emax(response ~ exposure, data = exposure.response.sample, gamma.fix = NULL,
#'                  priors = list(ec50  = c(100, 30), emax  = c(100, 30), e0 = c(10, 5),
#'                                gamma = c(0, 3), sigma = c(0, 30)))
#' )
#'
#' }
#'
stan_emax <- function(formula, data,
                      gamma.fix = 1, e0.fix = NULL,
                      priors = NULL, ...){


  standata.wo.prior <- stan_emax_prep(formula, data, gamma.fix, e0.fix)

  standata <- set_prior(standata.wo.prior, priors)

  out <- stan_emax_run(stanmodels$emax, standata = standata, ...)
}


# Parse formula and put together stan data object
stan_emax_prep <- function(formula, data,
                           gamma.fix = 1, e0.fix = NULL){

  call <- match.call(expand.dots = TRUE)
  mf <- match.call(expand.dots = FALSE)

  mf[[1L]] <- as.name("lm")
  mf$method <- "model.frame"
  modelframe <- suppressWarnings(eval(mf, parent.frame()))

  mt <- attr(modelframe, "terms")
  Y <- stats::model.response(modelframe)
  X <- stats::model.matrix(mt, modelframe)

  if(NCOL(Y) != 1) stop("Only one response variable is allowed")
  if(NCOL(X) != 2) stop("Only one exposure variable is allowed")

  X <- X[,2]

  standata <-
    create_standata(X, Y, gamma.fix, e0.fix)
}


# Run Emax model
# @param stanmodel a Stan model object.
# @param standata a data file for model fit
# @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
# @return An object of class `stanemax`
#
stan_emax_run <- function(stanmodel, standata, ...){
  # Run stan model and prepare `stanemax` object

  stanfit <- rstan::sampling(stanmodel, data = standata, ...)

  out <- list(stanfit = stanfit,
              standata= standata)

  structure(out, class = c("stanemax"))

}


create_standata <- function(X, Y, gamma.fix = 1, e0.fix = NULL){

  out <- list(exposure = X,
              response = Y,
              N = length(Y),
              gamma_fix_flg = 1,
              gamma_fix_value = 1,
              e0_fix_flg = 0,
              e0_fix_value = 0)

  if(!is.null(gamma.fix) && !is.na(gamma.fix)){
    if(!is.numeric(gamma.fix)) stop("gamma.fix must be NULL or numeric")

    out$gamma_fix_flg <- 1
    out$gamma_fix_value <- gamma.fix
  } else {
    out$gamma_fix_flg <- 0
  }

  if(!is.null(e0.fix) && !is.na(e0.fix)){
    if(!is.numeric(e0.fix)) stop("e0.fix must be NULL or numeric")

    out$e0_fix_flg <- 1
    out$e0_fix_value <-  e0.fix
  } else {
    out$e0_fix_flg <- 0
  }

  return(out)
}





