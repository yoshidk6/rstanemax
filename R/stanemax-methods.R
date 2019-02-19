#' Methods for stanemax objects
#'
#' @name stanemax-methods
#'
NULL


#' @rdname stanemax-methods
#' @export
print.stanemax <- function(object, ...) {
  param.extract <- c("emax", "e0", "ec50", "gamma", "sigma")

  print(object$stanfit, pars = param.extract)
}


#' @rdname stanemax-methods
#' @export
#' @param object An object of class `stanemax`
#' @param show.ci An logical specifying if the output figure include
#' credible interval of posterior prediction. Default TRUE.
#' @param show.pi An logical specifying if the output figure include
#' prediction interval. Default FALSE.
plot.stanemax <- function(object, show.ci = TRUE, show.pi = FALSE, ...) {

  obs  <- data.frame(exposure = object$standata$exposure,
                     response = object$standata$response)
  newdata <- create_new_data(object$standata)
  pred <- posterior_predict.stanemax(object, newdata = newdata, returnType = "tibble")

  pred.quantile <-
    pred %>%
    dplyr::group_by(exposure) %>%
    dplyr::summarize(rH025 = quantile(respHat, probs = 0.025),
                     rH500 = quantile(respHat, probs = 0.5),
                     rH975 = quantile(respHat, probs = 0.975),
                     re025 = quantile(response, probs = 0.025),
                     re500 = quantile(response, probs = 0.5),
                     re975 = quantile(response, probs = 0.975))
  obs
  g <-
    ggplot2::ggplot(pred.quantile, ggplot2::aes(exposure, rH500)) +
    ggplot2::geom_line()

  if(show.ci) g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin=rH025, ymax=rH975), alpha = .5)
  if(show.pi) g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin=re025, ymax=re975), alpha = .2)

  g <-
    g +
    ggplot2::geom_point(data = obs, ggplot2::aes(y = response)) +
    ggplot2::labs(y = "response")

  print(g)

}


create_new_data <- function(standata, length.out = 50){

  min.newdata <- min(standata$exposure)
  min.nozero.newdata <- min(standata$exposure[standata$exposure>0])
  max.newdata <- max(standata$exposure)

  seq.normal.scale <- seq(min.newdata, max.newdata, length.out = length.out)
  seq.log.scale <- seq(min.nozero.newdata, max.newdata, length.out = length.out)

  newdata <- dplyr::tibble(exposure = sort(c(seq.normal.scale, seq.log.scale)))

}


