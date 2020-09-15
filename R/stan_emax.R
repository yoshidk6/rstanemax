#' Bayesian Emax model fit with Stan
#'
#' Run sigmoidal Emax model fit with formula notation
#'
#' @export
#' @param formula a symbolic description of variables for Emax model fit.
#' @param data an optional data frame containing the variables in the model.
#' @param gamma.fix a (positive) numeric or NULL to specify gamma (Hill coefficient) in the sigmoidal Emax model.
#' If NULL, gamma will be estimated from the data.
#' If numeric, gamma is fixed at the number provided.
#' Default = 1 (normal Emax model).
#' @param e0.fix a numeric or NULL to specify E0 in the Emax model.
#' If NULL, E0 will be estimated from the data.
#' If numeric, E0 is fixed at the number provided.
#' Default = NULL (estimate from the data).
#' @param emax.fix a numeric or NULL to specify Emax in the Emax model.
#' If NULL, Emax will be estimated from the data.
#' If numeric, Emax is fixed at the number provided.
#' Default = NULL (estimate from the data).
#' @param priors a named list specifying priors of parameters (ec50, emax, e0, gamma, sigma).
#' Each list item should be length 2 numeric vector, one corresponding to mean and
#' another corresponding to standard deviation.
#' Currently only supports normal distribution for priors.
#' @param param.cov a named list specifying categorical covariates on parameters (ec50, emax, e0).
#' Convert a column into factor if specific order of covariates are needed.
#' @param ... Arguments passed to [rstan::sampling] (e.g. iter, chains).
#' @return An object of class `stanemax`
#' @details The following structure is used for the Emax model:
#' \deqn{Response = e_0 + e_{max} \times exposure ^{\gamma} / (ec50 ^{\gamma} + exposure ^ {\gamma}) + \epsilon}{Response = e0 + emax * exposure ^ gamma / (ec50 ^ gamma + exposure ^ gamma)  + epsilon}
#' \deqn{\epsilon \sim N(0, \sigma^2)}{epsilon ~ N(0, sigma ^ 2)}
#'
#' @examples
#' \dontrun{
#' data(exposure.response.sample)
#' fit1 <- stan_emax(response ~ exposure, data = exposure.response.sample,
#'                   # the next line is only to make the example go fast enough
#'                   chains = 1, iter = 500, seed = 12345)
#' print(fit1)
#'
#' # Set priors manually, also estimate gamma instead of the default of fix to 1
#' fit2 <- stan_emax(response ~ exposure, data = exposure.response.sample, gamma.fix = NULL,
#'                   priors = list(ec50  = c(100, 30), emax  = c(100, 30), e0 = c(10, 5),
#'                                 gamma = c(0, 3), sigma = c(0, 30)),
#'                   # the next line is only to make the example go fast enough
#'                   chains = 1, iter = 500, seed = 12345)
#' print(fit2)
#'
#' data(exposure.response.sample.test)
#' # Specify covariates
#' fit3 <- stan_emax(formula = resp ~ conc, data = exposure.response.sample.test,
#'                   param.cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"),
#'                   # the next line is only to make the example go fast enough
#'                   chains = 1, iter = 500, seed = 12345)
#' print(fit3)
#'}
#'

# Remove NA data, show warning
stan_emax <- function(formula, data,
                      gamma.fix = 1, e0.fix = NULL, emax.fix = NULL,
                      priors = NULL, param.cov = NULL, ...){

  out.prep <- stan_emax_prep(formula, data, gamma.fix, e0.fix, emax.fix, param.cov)

  standata <- set_prior(out.prep$standata, priors)

  out <- stan_emax_run(stanmodels$emax, standata = standata, ...)

  out$prminput <- out.prep$prminput

  return(out)
}



# Parse formula and put together stan data object
stan_emax_prep <- function(formula, data,
                           gamma.fix = 1, e0.fix = NULL, emax.fix = NULL, param.cov = NULL){


  check_param_cov(param.cov)
  cov.levels <- covs_get_levels(data, param.cov)
  df.model <- create_model_frame(formula, data, param.cov, cov.levels)


  standata <-
    create_standata(df.model, gamma.fix, e0.fix, emax.fix)


  out.prep <- list()
  out.prep$standata <- standata
  out.prep$prminput <- list()
  out.prep$prminput$formula    <- formula
  out.prep$prminput$df.model   <- df.model
  out.prep$prminput$cov.levels <- cov.levels
  out.prep$prminput$param.cov  <- param.cov

  return(out.prep)
}



# Check param.cov input
check_param_cov <- function(param.cov = NULL){
  if(sum(!(names(param.cov) %in% c("emax", "ec50", "e0")))){
    stop("Covariates can be specified only to emax, ec50, or e0")
  }
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


create_standata <- function(df.model, gamma.fix = 1, e0.fix = NULL, emax.fix = NULL){

  out <- list(exposure = df.model$exposure,
              response = df.model$response,
              covemax  = as.numeric(df.model$covemax),
              covec50  = as.numeric(df.model$covec50),
              cove0    = as.numeric(df.model$cove0),
              n_covlev_emax = length(levels(df.model$covemax)),
              n_covlev_ec50 = length(levels(df.model$covec50)),
              n_covlev_e0   = length(levels(df.model$cove0)),
              N = nrow(df.model),
              gamma_fix_flg = 1,
              gamma_fix_value = 1,
              e0_fix_flg = 0,
              e0_fix_value = 0,
              emax_fix_flg = 0,
              emax_fix_value = 0)

  if(!is.null(gamma.fix) && !is.na(gamma.fix)){
    if(!is.numeric(gamma.fix)) stop("gamma.fix must be NULL or numeric")
    if(gamma.fix <= 0) stop("gamma.fix must be NULL or positive numeric")

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

  if(!is.null(emax.fix) && !is.na(emax.fix)){
    if(!is.numeric(emax.fix)) stop("emax.fix must be NULL or numeric")

    out$emax_fix_flg <- 1
    out$emax_fix_value <-  emax.fix
  } else {
    out$emax_fix_flg <- 0
  }

  return(out)
}





