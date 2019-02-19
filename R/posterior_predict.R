#' Outcome prediction from posterior distribution of parameters
#'
#' Add explanations
#'
#' Run \code{vignette("emaxmodel", package = "rstanemax")} to see
#' how you can use the posterior prediction for plotting estimated Emax curve.
#'
#' @export
#' @export posterior_predict
#' @name posterior_predict
#' @param object a `stanemax` class object
#' @param newdata an optional data frame with a column named `exposure` or a numeric vector
#' @param returnType an optional string specifying the type of return object.
#' @return An object that contain predicted response with posterior distribution of parameters.
#' The default is a matrix containing predicted response.
#' Each row of the matrix is a vector of predictions generated using a single draw of the model parameters from the posterior distribution.
#'
#' If either `dataframe` or `tibble` is specified, the function returns a data frame or tibble object in a long format -
#' each row is a prediction generated using a single draw of the model parameters and a corresponding exposure.
#'
#' Two types of predictions are generated with this function.
#' respHat corresponds to the prediction without considering residual variability and is intended to provide credible interval of "mean" response.
#' response include residual variability in its calculation, therefore the range represents prediction interval of observed response.
#'
#' The return object also contains exposure and parameter values used for calculation.
#'
posterior_predict.stanemax <- function(object, newdata = NULL,
                                       returnType = c("matrix", "dataframe", "tibble"),
                                       ...){

  if(is.null(newdata)) {
    newdata <- data.frame(exposure = object$standata$exposure,
                          response = object$standata$response)
  } else {
    if(is.vector(newdata)) newdata <- dplyr::tibble(exposure = newdata)
  }

  pred.response <- pp_calc(object$stanfit, newdata)

  if(returnType[[1]] == "matrix") {
    return(matrix(pred.response$response, ncol = nrow(newdata), byrow = TRUE))
  } else if(returnType == "dataframe") {
    return(as.data.frame(pred.response))
  } else if(returnType == "tibble") {
    return(dplyr::as_tibble(pred.response))
  } else {
    stop('returnType must be either "matrix", "dataframe", or "tibble"')
    }

}


# Calculate posterior prediction from stanfit object and exposure data
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
#'
posterior_predict_quantile <- function(object, newdata = NULL){

  pp.raw <- posterior_predict.stanemax(object, newdata, returnType = c("tibble"))

  pp.quantile <-
    pp.raw %>%
    dplyr::group_by(exposure) %>%
    dplyr::summarize(respHat025 = quantile(respHat, probs = 0.025),
                     respHat500 = quantile(respHat, probs = 0.5),
                     respHat975 = quantile(respHat, probs = 0.975),
                     response025 = quantile(response, probs = 0.025),
                     response500 = quantile(response, probs = 0.5),
                     response975 = quantile(response, probs = 0.975))
}




