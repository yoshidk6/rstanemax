#' @rdname log_lik
#' @importFrom rstantools log_lik
#' @export
rstantools::log_lik

#' Calculate log-likelihoods from posterior samples. Data can be either
#' original data used for model fit or new data.
#'
#' See [rstantools::log_lik] for more details.
#'
#' @name log_lik
#' @importFrom rstantools log_lik
#' @param object A `stanemax` or `stanemaxbin` object
#' @param newdata New data used for prediction. If NULL, original data is used.
#' @param ... Currently unused arguments
#' @return \eqn{S} by \eqn{N} matrix of log-likelihoods, where each row
#' corresponds to a draw from the posterior distribution and each column corresponds to a data point.
#' @export
log_lik.stanemax <- function(object, newdata = NULL, ...) {
  # Extract posterior predictions
  pred.response <- .posterior_predict(
    object = object,
    transform = FALSE,
    column = ".linpred",
    newdata = newdata,
    returnType = "matrix",
    newDataType = "raw"
  )

  # Extract relevant parameters from the stanfit object
  param.fit <- extract_param_fit(object$stanfit, mod_type = "stanemax")

  # Compute log-likelihood
  log_lik_matrix <- matrix(nrow = nrow(pred.response), ncol = ncol(pred.response))

  # Get observed response
  if (is.null(newdata)) {
    obs <- object$prminput$df.model$response
  } else {
    obs <- newdata[[object$prminput$formula[[2]]]]
  }

  for (i in seq_len(nrow(pred.response))) {
    log_lik_matrix[i, ] <- stats::dnorm(
      x = obs,
      mean = pred.response[i, ],
      sd = param.fit$sigma[i],
      log = TRUE
    )
  }

  return(log_lik_matrix)
}

#' @rdname log_lik
#' @export
log_lik.stanemaxbin <- function(object, newdata = NULL, ...) {
  # Extract posterior predictions (logit scale)
  pred.logit <- .posterior_predict(
    object = object,
    transform = FALSE,
    column = ".linpred",
    newdata = newdata,
    returnType = "matrix",
    newDataType = "raw"
  )

  # Transform logit predictions to probabilities
  pred.prob <- stats::plogis(pred.logit)

  # Compute log-likelihood
  log_lik_matrix <- matrix(nrow = nrow(pred.prob), ncol = ncol(pred.prob))

  # Get observed response
  if (is.null(newdata)) {
    obs <- object$prminput$df.model$response
  } else {
    obs <- newdata[[object$prminput$formula[[2]]]]
  }

  for (i in seq_len(nrow(pred.prob))) {
    log_lik_matrix[i, ] <- stats::dbinom(
      x = obs,
      size = 1,
      prob = pred.prob[i, ],
      log = TRUE
    )
  }

  return(log_lik_matrix)
}
