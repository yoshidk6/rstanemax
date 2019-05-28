#' Methods for stanemax objects
#'
#' @name stanemax-methods
#'
NULL


#' @rdname stanemax-methods
#' @export
print.stanemax <- function(x, ...) {
  param.extract <- c("emax", "e0", "ec50", "gamma", "sigma")

  print(x$stanfit, pars = param.extract)
}


#' @rdname stanemax-methods
#' @export
#' @param x An object of class `stanemax`
#' @param show.ci An logical specifying if the output figure include
#' credible interval of posterior prediction. Default TRUE.
#' @param show.pi An logical specifying if the output figure include
#' prediction interval. Default FALSE.
#' @param ... Additional arguments passed to methods.
plot.stanemax <- function(x, show.ci = TRUE, show.pi = FALSE, ...) {

  obs  <- data.frame(exposure = x$standata$exposure,
                     response = x$standata$response)
  newdata <- create_new_data(x$standata)

  pred.quantile <- posterior_predict_quantile(x, newdata = newdata)

  g <-
    ggplot2::ggplot(pred.quantile, ggplot2::aes(exposure, ci_med)) +
    ggplot2::geom_line()

  if(show.ci) g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin=ci_low, ymax=ci_high), alpha = .5)
  if(show.pi) g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin=pi_low, ymax=pi_high), alpha = .2)

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


