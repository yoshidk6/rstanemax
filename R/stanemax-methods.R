#' Methods for stanemax objects
#'
#' @name stanemax-methods
#'
NULL


#' @rdname stanemax-methods
#' @param x An object of class `stanemax`
#' @param digits_summary The number of significant digits to use when printing
#' the summary, defaulting to 2. Applies to the quantities other than the
#' effective sample size, which is always rounded to the nearest integer.
#' @export
print.stanemax <- function(x, digits_summary = 2, ...) {

  param.extract <- c("emax", "e0", "ec50", "gamma", "sigma")
  # print(x$stanfit, pars = param.extract)

  s <- replace_prm_names(x, param.extract)

  cat("---- Emax model fit with rstanemax ----\n")
  cat("\n")
  print(round(s, digits_summary))
  cat("\n")
  cat("* Use `extract_stanfit()` function to extract raw stanfit object\n")
  cat("* Use `extract_param()` function to extract posterior draws of key parameters\n")
  cat("* Use `plot()` function to visualize model fit\n")
  cat("* Use `posterior_predict()` or `posterior_predict_quantile()` function to get\n")
  cat("  raw predictions or make predictions on new data\n")
  cat("* Use `extract_obs_mod_frame()` function to extract raw data \n")
  cat("  in a processed format (useful for plotting)\n")
}


#' Convert stanemax object to a posterior draws object
#'
#' @param x An object of class stanemax.
#' @param variable A character vector providing the variables to extract.
#'   By default, all variables are extracted.
#' @param regex Logical; Should variable should be treated as a (vector of)
#'   regular expressions? Any variable in \code{x} matching at least one of the
#'   regular expressions will be selected. Defaults to \code{FALSE}.
#' @param inc_warmup Should warmup draws be included? Defaults to \code{FALSE}.
#' @param ... Arguments passed to individual methods (if applicable).'
#' @return A draws object of the appropriate subclass
#' @examples
#' data(exposure.response.sample)
#' fit <- stan_emax(response ~ exposure, exposure.response.sample)
#' if(require(posterior, quietly = TRUE)) {
#'   posterior::as_draws_list(fit)
#'   posterior::as_draws_array(fit)
#'   posterior::as_draws_df(fit)
#'   posterior::as_draws_matrix(fit)
#'   posterior::as_draws_rvars(fit)
#' }
#' @seealso \code{\link[posterior:draws]{draws}}
#'   \code{\link[posterior:subset_draws]{subset_draws}}
#' @name as_draws
NULL

#' @rdname as_draws
#' @exportS3Method posterior::as_draws
as_draws.stanemax <- function(x, variable = NULL, regex = FALSE,
                                   inc_warmup = FALSE, ...) {
  .as_draws_list(
    x$stanfit,
    variable = variable,
    regex = regex,
    inc_warmup = inc_warmup,
    ...
  )
}

#' @rdname as_draws
#' @exportS3Method posterior::as_draws_list
as_draws_list.stanemax <- function(x, variable = NULL, regex = FALSE,
                                   inc_warmup = FALSE, ...) {
  .as_draws_list(
    x$stanfit,
    variable = variable,
    regex = regex,
    inc_warmup = inc_warmup,
    ...
  )
}

#' @rdname as_draws
#' @exportS3Method posterior::as_draws_array
as_draws_array.stanemax <- function(x, variable = NULL, regex = FALSE,
                                    inc_warmup = FALSE, ...) {
  posterior::as_draws_array(
    as_draws_list(
      x,
      variable = variable,
      regex = regex,
      inc_warmup = inc_warmup,
      ...
    )
  )
}

#' @rdname as_draws
#' @exportS3Method posterior::as_draws_df
as_draws_df.stanemax <- function(x, variable = NULL, regex = FALSE,
                                 inc_warmup = FALSE, ...) {
  posterior::as_draws_df(
    as_draws_list(
      x,
      variable = variable,
      regex = regex,
      inc_warmup = inc_warmup,
      ...
    )
  )
}

#' @rdname as_draws
#' @exportS3Method posterior::as_draws_matrix
as_draws_matrix.stanemax <- function(x, variable = NULL, regex = FALSE,
                                     inc_warmup = FALSE, ...) {
  posterior::as_draws_matrix(
    as_draws_list(
      x,
      variable = variable,
      regex = regex,
      inc_warmup = inc_warmup,
      ...
    )
  )
}

#' @rdname as_draws
#' @exportS3Method posterior::as_draws_rvars
as_draws_rvars.stanemax <- function(x, variable = NULL, regex = FALSE,
                                    inc_warmup = FALSE, ...) {
  posterior::as_draws_rvars(
    as_draws_list(
      x,
      variable = variable,
      regex = regex,
      inc_warmup = inc_warmup,
      ...
    )
  )
}

# mirror the approach in brms, converting a stanfit object to draws_list
.as_draws_list <- function(x, variable = NULL, regex = FALSE,
                           inc_warmup = FALSE, ...) {
  stopifnot(inherits(x, "stanfit"))
  if (!length(x@sim$samples)) {
    stop("The model does not contain posterior draws.", call. = FALSE)
  }
  out <- as_draws_list(x@sim$samples)

  # first subset variables then remove warmup as removing warmup
  # will take a lot of time when extracting many variables
  out <- subset_draws(out, variable = variable, regex = regex)
  if (!inc_warmup) {
    nwarmup <- x@sim$warmup2[1] %||% 0
    warmup_ids <- seq_len(nwarmup)
    iteration_ids <- posterior::iteration_ids(out)
    if (length(warmup_ids)) {
      iteration_ids <- iteration_ids[-warmup_ids]
    }
    out <- posterior::subset_draws(out, iteration = iteration_ids)
  }
  out
}


replace_prm_names <- function(x, pars) {

  # x <- test.fit.cov
  s <- rstan::summary(x$stanfit, pars = pars)$summary

  stbl <- dplyr::as_tibble(s, rownames = "prmname")

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
        dplyr::tibble(prm = k,
                      level = cov.levels[[k]]) %>%
        dplyr::mutate(index = dplyr::row_number(),
                      level = as.character(level))
    } else{
      prm.cov.df.list[[k]] <-
        dplyr::tibble(prm = k, index = 1, level = NA_character_)
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
#' @export
extract_stanfit <- function(x) {
  return(x$stanfit)
}

#' @rdname stanemax-methods
#' @export
extract_obs_mod_frame <- function(x) {
  return(x$prminput$df.model)
}


#' @rdname stanemax-methods
#' @export
#' @param show.ci An logical specifying if the output figure include
#' credible interval of posterior prediction. Default TRUE.
#' @param show.pi An logical specifying if the output figure include
#' prediction interval. Default FALSE.
#' @param ci Credible interval range.
#' @param pi Prediction interval range.
#' @param ... Additional arguments passed to methods.
plot.stanemax <- function(x, show.ci = TRUE, show.pi = FALSE,
                          ci = 0.9, pi = 0.9, ...) {

  obs <- x$prminput$df.model

  exposure.range <- create_exposure_range(x$standata)

  cov.list <-
    obs %>%
    dplyr::select(covemax, covec50, cove0) %>%
    dplyr::distinct()

  cov.list.2 <- create_cov_groups(cov.list, x$prminput$param.cov)

  newdata <- tidyr::crossing(exposure.range, cov.list.2)

  pred.quantile <- posterior_predict_quantile(x, newdata = newdata, newDataType = "modelframe",
                                              ci = ci, pi = pi)


  if(is.null(x$prminput$param.cov)){
    g <- ggplot2::ggplot(pred.quantile, ggplot2::aes(exposure, ci_med))
  } else {
    g <- ggplot2::ggplot(pred.quantile, ggplot2::aes(exposure, ci_med,
                                                     group = Covariates,
                                                     color = Covariates,
                                                     fill = Covariates))
  }

  g <-
    g +
    ggplot2::geom_point(data = obs, ggplot2::aes(y = response)) +
    ggplot2::labs(y = "response")

  if(show.ci) g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin=ci_low, ymax=ci_high), alpha = .5, color = NA)
  if(show.pi) g <- g + ggplot2::geom_ribbon(ggplot2::aes(ymin=pi_low, ymax=pi_high), alpha = .2, color = NA)

  g <- g + ggplot2::geom_line()


  g

}


create_exposure_range <- function(standata, length.out = 50){

  min.newdata <- min(standata$exposure)
  min.nozero.newdata <- min(standata$exposure[standata$exposure>0])
  max.newdata <- max(standata$exposure)

  seq.normal.scale <- seq(min.newdata, max.newdata, length.out = length.out)
  seq.log.scale <- exp(seq(log(min.nozero.newdata),
                           log(max.newdata),
                           length.out = length.out))

  exposure.range <- dplyr::tibble(exposure = sort(c(seq.normal.scale, seq.log.scale)))

}


