#' @rdname posterior_predict
#' @importFrom rstantools posterior_predict
#' @export
rstantools::posterior_predict

#' @rdname posterior_predict
#' @importFrom rstantools posterior_epred
#' @export
rstantools::posterior_epred

#' @rdname posterior_predict
#' @importFrom rstantools posterior_linpred
#' @export
rstantools::posterior_linpred

#' Outcome prediction from posterior distribution of parameters
#'
#' Compute outcome predictions using posterior samples. Exposure data for
#' prediction can be either original data used for model fit or new data.
#'
#' Run \code{vignette("emaxmodel", package = "rstanemax")} to see how you can
#' use the posterior prediction for plotting estimated Emax curve.
#'
#' @name posterior_predict
#' @importFrom rstantools posterior_predict
#' @param object A `stanemax` or `stanemaxbin` object
#' @param transform Should the linear predictor be transformed to response
#'   scale?
#' @param newdata An optional data frame that contains columns needed for model
#'   to run (exposure and covariates). If the model does not have any covariate,
#'   this can be a numeric vector corresponding to the exposure metric.
#' @param returnType `r lifecycle::badge("deprecated")` An optional string
#'   specifying the type of return object (one of "matrix", "dataframe", or
#'   "tibble")
#' @param newDataType An optional string specifying the type of newdata input,
#'   whether in the format of an original data frame ("raw", the default) or a
#'   processed model frame ("modelframe"). Mostly used for internal purposes and
#'   users can usually leave at default.
#' @param ... Additional arguments passed to methods. Arguments that can be
#'   passed via the dots include `ndraws`, for compatibility with functions in
#'   the tidybayes package
#' @return An object that contain predicted response with posterior distribution
#'   of parameters. The default is a matrix containing predicted `response` for
#'   [stan_emax()] and `.epred` for [stan_emax_binary()]. Each row of the matrix
#'   is a vector of predictions generated using a single draw of the model
#'   parameters from the posterior distribution.
#'
#'   If either `dataframe` or `tibble` is specified, the function returns a data
#'   frame or tibble object in a long format - each row is a prediction
#'   generated using a single draw of the model parameters and a corresponding
#'   exposure.
#'
#'   Several types of predictions are generated with this function.
#'
#'   For continuous endpoint model ([stan_emax()]),
#'
#'   - `.linpred` & `.epred`: prediction without considering residual
#'   variability and is intended to provide credible interval of "mean"
#'   response.
#'   - `.prediction`: include residual variability in its calculation,
#'   therefore the range represents prediction interval of observed response.
#'   - `r lifecycle::badge("deprecated")` `respHat`: replaced by `.linpred`
#'   and `.epred`
#'   - `r lifecycle::badge("deprecated")` `response`: replaced by `.prediction`
#'
#'   For binary endpoint model ([stan_emax_binary()]),
#'
#' - `.linpred`: predicted probability on logit scale
#' - `.epred`: predicted probability on probability scale
#' - `.prediction`: predicted event (1) or non-event (0)
#'
#'   The return object also contains exposure and parameter values used for
#'   calculation.
NULL

# S3 methods --------------------------------------------------------------

#' @rdname posterior_predict
#' @importFrom rstantools posterior_predict
#' @export
posterior_predict.stanemax <- function(object,
                                       newdata = NULL,
                                       returnType = "matrix",
                                       newDataType = "raw",
                                       ...) {
  if (returnType != "matrix") {
    lifecycle::deprecate_soft(
      when = "0.1.8",
      what = "posterior_predict(returnType)"
    )
  }
  .posterior_predict(
    object = object,
    transform = FALSE,
    column = ".prediction",
    newdata = newdata,
    returnType = returnType,
    newDataType = newDataType,
    ...
  )
}

#' @rdname posterior_predict
#' @importFrom rstantools posterior_predict
#' @export
posterior_predict.stanemaxbin <- function(object,
                                          newdata = NULL,
                                          returnType = "matrix",
                                          newDataType = "raw",
                                          ...) {
  if (returnType != "matrix") {
    lifecycle::deprecate_soft(
      when = "0.1.8",
      what = "posterior_predict(returnType)"
    )
  }
  .posterior_predict(
    object = object,
    transform = FALSE,
    column = ".prediction",
    newdata = newdata,
    returnType = returnType,
    newDataType = newDataType,
    ...
  )
}

#' @rdname posterior_predict
#' @importFrom rstantools posterior_epred
#' @export
posterior_epred.stanemax <- function(object,
                                     newdata = NULL,
                                     newDataType = "raw",
                                     ...) {
  .posterior_predict(
    object = object,
    transform = FALSE,
    column = ".epred",
    newdata = newdata,
    returnType = "matrix",
    newDataType = newDataType,
    ...
  )
}

#' @rdname posterior_predict
#' @importFrom rstantools posterior_epred
#' @export
posterior_epred.stanemaxbin <- function(object,
                                        newdata = NULL,
                                        newDataType = "raw",
                                        ...) {
  .posterior_predict(
    object = object,
    transform = FALSE,
    column = ".epred",
    newdata = newdata,
    returnType = "matrix",
    newDataType = newDataType,
    ...
  )
}


#' @rdname posterior_predict
#' @importFrom rstantools posterior_linpred
#' @export
posterior_linpred.stanemax <- function(object,
                                       transform = FALSE,
                                       newdata = NULL,
                                       newDataType = "raw",
                                       ...) {
  .posterior_predict(
    object = object,
    transform = transform,
    column = ".linpred",
    newdata = newdata,
    returnType = "matrix",
    newDataType = newDataType,
    ...
  )
}

#' @rdname posterior_predict
#' @importFrom rstantools posterior_linpred
#' @export
posterior_linpred.stanemaxbin <- function(object,
                                          transform = FALSE,
                                          newdata = NULL,
                                          newDataType = "raw",
                                          ...) {
  .posterior_predict(
    object = object,
    transform = transform,
    column = ".linpred",
    newdata = newdata,
    returnType = "matrix",
    newDataType = newDataType,
    ...
  )
}

# helper functions --------------------------------------------------------

.posterior_predict <- function(object,
                               transform,
                               column,
                               newdata,
                               returnType,
                               newDataType,
                               ndraws = NULL,
                               ...) {
  returnType <- match.arg(returnType, c("matrix", "dataframe", "tibble"))
  newDataType <- match.arg(newDataType, c("raw", "modelframe"))
  df.model <- pp_model_frame(object, newdata, newDataType)
  pred.response.raw <- pp_calc(
    stanfit = object$stanfit,
    df.model = df.model,
    mod_type = class(object),
    transform = transform,
    ndraws = ndraws
  )
  pred.response <- pp_update_cov_levels(pred.response.raw, df.model)
  if (returnType == "matrix") {
    output <- pred.response[[column]]
    return(matrix(output, ncol = nrow(df.model), byrow = TRUE))
  }
  if (returnType == "dataframe") {
    return(as.data.frame(pred.response))
  }
  if (returnType == "tibble") {
    return(dplyr::as_tibble(pred.response))
  }
}

# Construct model frame as needed for posterior_predict and similar
pp_model_frame <- function(object, newdata, newDataType) {
  if (is.null(newdata)) {
    return(object$prminput$df.model)
  }
  if (newDataType == "modelframe") {
    return(newdata)
  }
  if (is.vector(newdata)) {
    newdata <- data.frame(newdata)
    names(newdata) <- as.character(object$prminput$formula[[3]])
  }
  create_model_frame(
    formula = object$prminput$formula,
    data = newdata,
    param.cov = object$prminput$param.cov,
    cov.levels = object$prminput$cov.levels,
    is.model.fit = FALSE
  )
}

# Calculate posterior prediction from stanfit object and exposure data
# data.pp is a data frame with column named `exposure`
pp_calc <- function(stanfit,
                    df.model,
                    mod_type = c("stanemax", "stanemaxbin"),
                    transform = FALSE,
                    ndraws = NULL) {
  mod_type <- match.arg(mod_type)
  param.fit <- extract_param_fit(stanfit, mod_type)

  if (is.null(ndraws)) {
    draw_inds <- 1:max(param.fit$mcmcid)
  } else {
    draw_inds <- sample(
      x = max(param.fit$mcmcid),
      size = ndraws,
      replace = FALSE
    )
  }

  df <-
    df.model %>%
    dplyr::mutate(
      covemax = as.numeric(covemax),
      covec50 = as.numeric(covec50),
      cove0 = as.numeric(cove0)
    ) %>%
    tidyr::expand_grid(mcmcid = draw_inds, .) %>%
    dplyr::left_join(param.fit, by = c("mcmcid", "covemax", "covec50", "cove0"))

  if (mod_type == "stanemax") {
    out <-
      df %>%
      dplyr::mutate(
        respHat = e0 + emax * exposure^gamma / (ec50^gamma + exposure^gamma),
        response = stats::rnorm(respHat, respHat, sigma)
      ) %>%
      dplyr::mutate(
        .linpred = respHat,
        .epred = respHat,
        .prediction = response
      ) %>%
      dplyr::select(mcmcid, exposure, dplyr::everything())
  } else if (mod_type == "stanemaxbin") {
    out <-
      df %>%
      dplyr::mutate(
        .linpred = e0 + emax * exposure^gamma / (ec50^gamma + exposure^gamma),
        .epred = stats::plogis(.linpred),
        .prediction = stats::rbinom(.epred, 1, .epred)
      ) %>%
      dplyr::select(mcmcid, exposure, dplyr::everything())

    # transform arg supported for consistency with posterior_linpred generic
    if (transform == TRUE) out$.linpred <- out$.epred
  }

  return(out)
}

# Convert the posterior prediction output to tidied data frame
pp_update_cov_levels <- function(pred.response.raw, df.model) {
  cov.fct.numeric <-
    df.model %>%
    dplyr::select(
      covemaxfct = covemax,
      covec50fct = covec50,
      cove0fct = cove0
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      covemax = as.numeric(covemaxfct),
      covec50 = as.numeric(covec50fct),
      cove0 = as.numeric(cove0fct)
    )

  pred.response <-
    dplyr::left_join(pred.response.raw,
      cov.fct.numeric,
      by = c("covemax", "covec50", "cove0")
    ) %>%
    dplyr::select(-(covemax:cove0)) %>%
    dplyr::select(mcmcid,
      exposure,
      covemax = covemaxfct,
      covec50 = covec50fct,
      cove0   = cove0fct,
      dplyr::everything()
    )

  return(pred.response)
}

# required by pp_calc
extract_param_fit <- function(stanfit,
                              mod_type = c("stanemax", "stanemaxbin")) {
  mod_type <- match.arg(mod_type)
  param.extract.1 <- rstan::extract(stanfit, pars = c("emax", "e0", "ec50"))
  if (mod_type == "stanemax") {
    pars2 <- c("gamma", "sigma")
  } else if (mod_type == "stanemaxbin") {
    pars2 <- c("gamma")
  }
  param.extract.2 <- rstan::extract(stanfit, pars = pars2)

  extract_params_covs <- function(k) {
    vec.param <- param.extract.1[[k]]
    colnames(vec.param) <- paste0("V", seq_len(ncol(vec.param)))

    out <-
      dplyr::as_tibble(vec.param, .name_repair = "unique") %>%
      dplyr::mutate(mcmcid = dplyr::row_number()) %>%
      tidyr::pivot_longer(-mcmcid,
        names_to = paste0("cov", k),
        values_to = k,
        names_prefix = "V"
      )

    return(out)
  }

  param.fit.withcov <-
    dplyr::full_join(extract_params_covs("emax"),
      extract_params_covs("e0"),
      by = "mcmcid",
      relationship = "many-to-many"
    ) %>%
    dplyr::full_join(extract_params_covs("ec50"),
      by = "mcmcid",
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      covemax = as.numeric(covemax),
      covec50 = as.numeric(covec50),
      cove0 = as.numeric(cove0)
    )

  param.fit <-
    param.extract.2 %>%
    dplyr::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate(mcmcid = dplyr::row_number()) %>%
    dplyr::full_join(param.fit.withcov, ., by = "mcmcid")

  return(param.fit)
}


# additional rstanemax functions ------------------------------------------

#' @rdname posterior_predict
#' @export
#' @param ci Credible interval of the response without residual variability.
#' @param pi Prediction interval of the response with residual variability.
#' @return With [posterior_predict_quantile()] function, you can obtain quantiles
#' of `respHat` and `response` as specified by `ci` and `pi`.
#'
posterior_predict_quantile <- function(object,
                                       newdata = NULL,
                                       ci = 0.9,
                                       pi = 0.9,
                                       newDataType = c("raw", "modelframe")) {
  mod_type <- class(object)

  pp.raw <- .posterior_predict(
    object = object,
    transform = FALSE,
    column = ".prediction",
    newdata = newdata,
    returnType = "tibble",
    newDataType = newDataType
  )

  ndata <-
    dplyr::filter(pp.raw, mcmcid == 1) %>%
    nrow()

  pp.raw.2 <-
    pp.raw %>%
    dplyr::mutate(dataid = rep(1:ndata, length.out = nrow(.)))

  if (mod_type == "stanemax") {
    pp.quantile <-
      pp.raw.2 %>%
      dplyr::group_by(exposure, covemax, covec50, cove0, Covariates, dataid) %>%
      dplyr::summarize(
        ci_low = stats::quantile(respHat, probs = 0.5 - ci / 2),
        ci_med = stats::quantile(respHat, probs = 0.5),
        ci_high = stats::quantile(respHat, probs = 0.5 + ci / 2),
        pi_low = stats::quantile(response, probs = 0.5 - pi / 2),
        pi_med = stats::quantile(response, probs = 0.5),
        pi_high = stats::quantile(response, probs = 0.5 + pi / 2)
      ) %>%
      dplyr::arrange(dataid) %>%
      dplyr::ungroup() %>%
      dplyr::select(-dataid)
  } else if (mod_type == "stanemaxbin") {
    pp.quantile <-
      pp.raw.2 %>%
      dplyr::group_by(exposure, covemax, covec50, cove0, Covariates, dataid) %>%
      dplyr::summarize(
        ci_low = stats::quantile(.epred, probs = 0.5 - ci / 2),
        ci_med = stats::quantile(.epred, probs = 0.5),
        ci_high = stats::quantile(.epred, probs = 0.5 + ci / 2)
      ) %>%
      dplyr::arrange(dataid) %>%
      dplyr::ungroup() %>%
      dplyr::select(-dataid)
  }

  return(pp.quantile)
}
