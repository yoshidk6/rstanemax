#' Methods for stanemax objects
#'
#' @name stanemax-methods
#'
NULL


#' @rdname stanemax-methods
#' @export
print.stanemax <- function(x, digits_summary = 2, ...) {

  param.extract <- c("emax", "e0", "ec50", "gamma", "sigma")
  # print(x$stanfit, pars = param.extract)

  s <- replace_prm_names(x, param.extract)

  cat("---- Emax model fit with rstanemax ----\n")
  cat("\n")
  print(round(s, digits_summary))
  cat("\n")
  cat("* Use `posterior_predict()` or `posterior_predict_quantile()` function to get\n")
  cat("  raw predictions or make predictions on new data\n")
  cat("* Use `plot()` function to visualize model fit\n")
  cat("* Use `extract_stanfit()` function to extract raw stanfit object")
}


replace_prm_names <- function(x, pars) {

  # x <- test.fit.cov
  s <- rstan::summary(x$stanfit, pars = pars)$summary

  stbl <- tibble::as_tibble(s, rownames = "prmname")

  ## Get parameter names in stanfit
  prm.df <-
    stbl %>%
    tidyr::separate(prmname, into = c("prm", "index", "del"), fill = "right", remove = FALSE) %>%
    dplyr::mutate(index = as.numeric(index)) %>%
    dplyr::select(prmname, prm, index)


  ## Get factor levels in covariates
  cov.levels <- x$prminput$cov.levels

  prm.cov.df.list <- list()

  for(k in names(cov.levels)){
    if(length(cov.levels[[k]]) > 1){
      prm.cov.df.list[[k]] <-
        tibble::tibble(prm = k,
                       level = cov.levels[[k]]) %>%
        dplyr::mutate(index = dplyr::row_number(),
                      level = as.character(level))
    } else{
      prm.cov.df.list[[k]] <-
        tibble::tibble(prm = k, index = 1, level = NA_character_)
    }
  }
  prm.cov.df <-
    dplyr::bind_rows(prm.cov.df.list)

  ## Merge back to the original data frame
  stbl2 <-
    dplyr::left_join(prm.df, prm.cov.df, by = c("prm", "index")) %>%
    dplyr::mutate(prmname2 = ifelse(is.na(level), prm, paste0(prm, "[", level, "]"))) %>%
    dplyr::left_join(stbl, ., by = "prmname") %>%
    dplyr::select(-c(prmname, prm, index, level))

  s2 <- as.matrix(dplyr::select(stbl2, -prmname2))
  rownames(s2) <- stbl2$prmname2

  return(s2)
}



#' @rdname stanemax-methods
#'
#' Extract stanfit object
#'
#' @export
#' @param x An object of class `stanemax`
extract_stanfit <- function(x) {
  return(x$stanfit)
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


