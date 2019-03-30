#' Bayesian Emax model fit with Stan
#'
#' Add explanations
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
#' @param priors a list specifying priors of parameters.
#' Not implemented yet.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanemax`
#' @examples
#' data(exposure.response.sample)
#' fit <- stan_emax(response ~ exposure, data = exposure.response.sample)
#' print(fit)
#'
#' \dontrun{
#'
#' }
#'
stan_emax <- function(formula, data,
                      gamma.fix = 1, e0.fix = NULL,
                      priors = NULL, ...){
  # Parse formula and put together stan data object
  # Actual run is in `stan_emax_run` function

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
    create_standata(X, Y, gamma.fix, e0.fix) %>%
    set_prior(priors)

  out <- stan_emax_run(stanmodels$emax, standata = standata, ...)
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





# Maybe separate a file for prior setting


# Cleaner to create some S3 class to contain all prior information?
# Check building tidy tools materials

# Write test for cases where any of list items have != 2 elements
# Make sure there is no coding error in passing `priors` into standata

# priors = list(ec50 = c(10, 10))

# How should we assign distribution type?
# Maybe another list item like `priors$sigmadist`?

set_prior <- function(standata, priors = NULL){
  standata <- set_prior_auto(standata)

  if(!is.null(priors$ec50)){
    standata$prior_ec50_mu  <- priors$ec50[[1]]
    standata$prior_ec50_sig <- priors$ec50[[2]]
  }
  if(!is.null(priors$emax)){
    standata$prior_emax_mu  <- priors$emax[[1]]
    standata$prior_emax_sig <- priors$emax[[2]]
  }
  if(!is.null(priors$e0)){
    standata$prior_e0_mu  <- priors$e0[[1]]
    standata$prior_e0_sig <- priors$e0[[2]]
  }
  if(!is.null(priors$gamma)){
    standata$prior_gamma_mu  <- priors$gamma[[1]]
    standata$prior_gamma_sig <- priors$gamma[[2]]
  }
  if(!is.null(priors$sigma)){
    standata$prior_sigma_mu  <- priors$sigma[[1]]
    standata$prior_sigma_sig <- priors$sigma[[2]]
  }


}



set_prior_auto <- function(standata){

  # EC50
  standata$prior_ec50_mu  <- stats::median(standata$exposure)
  standata$prior_ec50_sig <- stats::median(standata$exposure) * 2

  # Emax
  delta <- max(standata$response) - min(standata$response)
  coeflm <- stats::lm(response ~ exposure, data = standata) %>% stats::coef()
  slope <- coeflm[[2]]

  if(slope > 0){
    standata$prior_emax_mu <- delta
  } else {
    standata$prior_emax_mu <- - delta
  }
  standata$prior_emax_sig <- delta

  # E0
  resp.low.exp <- standata$response[standata$exposure <= stats::quantile(standata$exposure, 0.25)]
  standata$prior_e0_mu <- stats::median(resp.low.exp)
  standata$prior_e0_sig <- abs(stats::median(resp.low.exp)) * 2

  # Gamma
  standata$prior_gamma_mu  <- 0
  standata$prior_gamma_sig <- 5

  # Sigma
  standata$prior_sigma_mu  <- 0
  standata$prior_sigma_sig <- stats::sd(standata$response)


  return(standata)
}
