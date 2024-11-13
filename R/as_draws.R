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
