#' Outcome prediction from posterior distribution of parameters
#'
#' Compute outcome predictions using posterior samples.
#' Exposure data for prediction can be either original data used for model fit or new data.
#'
#' Run \code{vignette("emaxmodel", package = "rstanemax")} to see
#' how you can use the posterior prediction for plotting estimated Emax curve.
#'
#' @export
#' @export posterior_predict
#' @name posterior_predict
#' @param object A `stanemax` class object
#' @param newdata An optional data frame with a column named `exposure` or a numeric vector
#' @param returnType An optional string specifying the type of return object.
#' @param ... Additional arguments passed to methods.
#' @return An object that contain predicted response with posterior distribution of parameters.
#' The default is a matrix containing predicted response.
#' Each row of the matrix is a vector of predictions generated using a single draw of the model parameters from the posterior distribution.
#'
#' If either _dataframe_ or _tibble_ is specified, the function returns a data frame or tibble object in a long format -
#' each row is a prediction generated using a single draw of the model parameters and a corresponding exposure.
#'
#' Two types of predictions are generated with this function.
#' _respHat_ corresponds to the prediction without considering residual variability and is intended to provide credible interval of "mean" response.
#' _response_ include residual variability in its calculation, therefore the range represents prediction interval of observed response.
#'
#' The return object also contains exposure and parameter values used for calculation.
#'
posterior_predict.stanemax <- function(object, newdata = NULL,
                                       returnType = c("matrix", "dataframe", "tibble"),
                                       ...){

  returnType <- match.arg(returnType)

  if(is.null(newdata)) {
    newdata <- data.frame(exposure = object$standata$exposure,
                          response = object$standata$response)
  } else {
    if(is.vector(newdata)) newdata <- dplyr::tibble(exposure = newdata)
  }

  pred.response <- pp_calc(object$stanfit, newdata)

  if(returnType == "matrix") {
    return(matrix(pred.response$response, ncol = nrow(newdata), byrow = TRUE))
  } else if(returnType == "dataframe") {
    return(as.data.frame(pred.response))
  } else if(returnType == "tibble") {
    return(dplyr::as_tibble(pred.response))
  }

}


# Calculate posterior prediction from stanfit object and exposure data
## data.pp is a data frame with column named `exposure`
pp_calc <- function(stanfit, data.pp){

  param.extract <- c("emax", "e0", "ec50", "gamma", "sigma")

  param.fit  <-
    rstan::extract(stanfit, pars = param.extract) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(mcmcid = dplyr::row_number())

  df <- tidyr::crossing(param.fit, data.pp)

  out <-
    df %>%
    dplyr::mutate(respHat = e0 + emax * exposure^gamma / (ec50^gamma + exposure^gamma),
                  response= stats::rnorm(respHat, respHat, sigma)) %>%
    dplyr::select(mcmcid, exposure, dplyr::everything())

}



#' @rdname posterior_predict
#' @export
#' @param ci Credible interval of the response without residual variability.
#' @param pi Prediction interval of the response with residual variability.
#'
posterior_predict_quantile <- function(object, newdata = NULL, ci = 0.9, pi = 0.9){

  pp.raw <- posterior_predict.stanemax(object, newdata, returnType = c("tibble"))

  pp.quantile <-
    pp.raw %>%
    dplyr::group_by(exposure) %>%
    dplyr::summarize(ci_low = stats::quantile(respHat, probs = 0.5 - ci/2),
                     ci_med = stats::quantile(respHat, probs = 0.5),
                     ci_high= stats::quantile(respHat, probs = 0.5 + ci/2),
                     pi_low = stats::quantile(response, probs = 0.5 - pi/2),
                     pi_med = stats::quantile(response, probs = 0.5),
                     pi_high= stats::quantile(response, probs = 0.5 + pi/2))
}




