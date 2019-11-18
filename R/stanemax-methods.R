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

  obs <- x$prminput$df.model
  param.cov <- x$prminput$param.cov

  cov.group.for.plot <-
    obs %>%
    as_tibble() %>%
    dplyr::mutate(covemaxstr = paste0("Emax:", covemax),
                  covec50str = paste0("EC50:", covec50),
                  cove0str   = paste0("E0:",   cove0)) %>%
    dplyr::select(covemax, covec50, cove0, covemaxstr, covec50str, cove0str) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Covariates = "",
                  Covariates = purrr::map2_chr(Covariates, covemaxstr, ~ifelse(is.null(param.cov$emax), .x, paste(.x, .y))),
                  Covariates = purrr::map2_chr(Covariates, covec50str, ~ifelse(is.null(param.cov$ec50), .x, paste(.x, .y))),
                  Covariates = purrr::map2_chr(Covariates, cove0str,   ~ifelse(is.null(param.cov$e0),   .x, paste(.x, .y))),
                  Covariates = trimws(Covariates))

  obs <- dplyr::left_join(obs, cov.group.for.plot, by = c("covemax", "covec50", "cove0"))

  exposure.range <- create_exposure_range(x$standata)

  cov.list <-
    obs %>%
    dplyr::select(covemax, covec50, cove0) %>%
    dplyr::distinct()

  newdata <- tidyr::crossing(exposure.range, cov.list)

  pred.quantile <- posterior_predict_quantile(x, newdata = newdata, newDataType = "modelframe")


  if(is.null(x$prminput$param.cov)){
    g <- ggplot2::ggplot(pred.quantile, ggplot2::aes(exposure, ci_med))
  } else {
    pred.quantile <- dplyr::left_join(pred.quantile, cov.group.for.plot, by = c("covemax", "covec50", "cove0"))

    g <- ggplot2::ggplot(pred.quantile, ggplot2::aes(exposure, ci_med,
                                                     group = Covariates,
                                                     color = Covariates,
                                                     fill = Covariates))
  }


  if(show.ci) g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin=ci_low, ymax=ci_high), alpha = .5, color = NA)
  if(show.pi) g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin=pi_low, ymax=pi_high), alpha = .2, color = NA)

  g <- g + ggplot2::geom_line()

  g <-
    g +
    ggplot2::geom_point(data = obs, ggplot2::aes(y = response)) +
    ggplot2::labs(y = "response")

  print(g)

}


create_exposure_range <- function(standata, length.out = 50){

  min.newdata <- min(standata$exposure)
  min.nozero.newdata <- min(standata$exposure[standata$exposure>0])
  max.newdata <- max(standata$exposure)

  seq.normal.scale <- seq(min.newdata, max.newdata, length.out = length.out)
  seq.log.scale <- seq(min.nozero.newdata, max.newdata, length.out = length.out)

  exposure.range <- dplyr::tibble(exposure = sort(c(seq.normal.scale, seq.log.scale)))

}


