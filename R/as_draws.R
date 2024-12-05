#' @rdname as_draws
#' @importFrom posterior as_draws
#' @export
posterior::as_draws

#' @rdname as_draws
#' @importFrom posterior as_draws_list
#' @export
posterior::as_draws_list

#' @rdname as_draws
#' @importFrom posterior as_draws_array
#' @export
posterior::as_draws_array

#' @rdname as_draws
#' @importFrom posterior as_draws_df
#' @export
posterior::as_draws_df

#' @rdname as_draws
#' @importFrom posterior as_draws_matrix
#' @export
posterior::as_draws_matrix

#' @rdname as_draws
#' @importFrom posterior as_draws_rvars
#' @export
posterior::as_draws_rvars


#' Convert stanemax object to a posterior draws object
#'
#' @param x An object of class stanemax.
#' @param inc_warmup Should warmup draws be included? Defaults to \code{FALSE}.
#' @param ... Arguments passed to individual methods (if applicable).'
#' @return A draws object of the appropriate subclass
#' @examples
#' \dontrun{
#' data(exposure.response.sample)
#' fit <- stan_emax(response ~ exposure, exposure.response.sample)
#' posterior::as_draws_list(fit)
#' posterior::as_draws_array(fit)
#' posterior::as_draws_df(fit)
#' posterior::as_draws_matrix(fit)
#' posterior::as_draws_rvars(fit)
#' }
#' @seealso \code{\link[posterior:draws]{draws}}
#'   \code{\link[posterior:subset_draws]{subset_draws}}
#' @name as_draws
NULL

#' @rdname as_draws
#' @export
as_draws.stanemax <- function(x, inc_warmup = FALSE, ...) {
  .as_draws_list(x, inc_warmup = inc_warmup, ...)
}

#' @rdname as_draws
#' @export
as_draws.stanemaxbin <- function(x, inc_warmup = FALSE, ...) {
  .as_draws_list(x, inc_warmup = inc_warmup, ...)
}

#' @rdname as_draws
#' @export
as_draws_list.stanemax <- function(x, inc_warmup = FALSE, ...) {
  .as_draws_list(x, inc_warmup = inc_warmup, ...)
}

#' @rdname as_draws
#' @export
as_draws_list.stanemaxbin <- function(x, inc_warmup = FALSE, ...) {
  .as_draws_list(x, inc_warmup = inc_warmup, ...)
}

#' @rdname as_draws
#' @export
as_draws_array.stanemax <- function(x, inc_warmup = FALSE, ...) {
  posterior::as_draws_array(.as_draws_list(x, inc_warmup = inc_warmup, ...))
}

#' @rdname as_draws
#' @export
as_draws_array.stanemaxbin <- function(x, inc_warmup = FALSE, ...) {
  posterior::as_draws_array(.as_draws_list(x, inc_warmup = inc_warmup, ...))
}

#' @rdname as_draws
#' @export
as_draws_df.stanemax <- function(x, inc_warmup = FALSE, ...) {
  posterior::as_draws_df(.as_draws_list(x, inc_warmup = inc_warmup, ...))
}

#' @rdname as_draws
#' @export
as_draws_df.stanemaxbin <- function(x, inc_warmup = FALSE, ...) {
  posterior::as_draws_df(.as_draws_list(x, inc_warmup = inc_warmup, ...))
}

#' @rdname as_draws
#' @export
as_draws_matrix.stanemax <- function(x, inc_warmup = FALSE, ...) {
  posterior::as_draws_matrix(.as_draws_list(x, inc_warmup = inc_warmup, ...))
}

#' @rdname as_draws
#' @export
as_draws_matrix.stanemaxbin <- function(x, inc_warmup = FALSE, ...) {
  posterior::as_draws_matrix(.as_draws_list(x, inc_warmup = inc_warmup, ...))
}

#' @rdname as_draws
#' @export
as_draws_rvars.stanemax <- function(x, inc_warmup = FALSE, ...) {
  posterior::as_draws_rvars(.as_draws_list(x, inc_warmup = inc_warmup, ...))
}

#' @rdname as_draws
#' @export
as_draws_rvars.stanemaxbin <- function(x, inc_warmup = FALSE, ...) {
  posterior::as_draws_rvars(.as_draws_list(x, inc_warmup = inc_warmup, ...))
}

# regex to match "emax", "emax[1]", "emax[1,1]", but not "emaxvec", etc.
.draws_vars_regex <- "((emax)|(ec50)|(e0)|(sigma)|(gamma))(\\[|$)"

# mirror the approach in brms, converting a stanfit object to draws_list
.as_draws_list <- function(x, inc_warmup = FALSE, ...) {
  # verify the input object
  stopifnot(inherits(x[["stanfit"]], "stanfit"))
  if (!length(x$stanfit@sim$samples)) {
    stop("The model does not contain posterior draws.", call. = FALSE)
  }

  # construct draws list for emax model parameters only
  out <- posterior::as_draws_list(x$stanfit@sim$samples)
  out <- posterior::subset_draws(out, variable = .draws_vars_regex, regex = TRUE)

  # rename emax model parameters using covariate levels
  out <- .add_covariate_labels(out, x$prminput$cov.levels)

  # remove warmup samples, if requested
  if (!inc_warmup) {
    n_warmup <- x$stanfit@sim$warmup2[1]
    if (!is.null(n_warmup) && n_warmup > 0) {
      iteration_ids <- posterior::iteration_ids(out)
      iteration_ids <- iteration_ids[-seq_len(n_warmup)]
      out <- posterior::subset_draws(out, iteration = iteration_ids)
    }
  }

  out
}

.add_covariate_labels <- function(draws_list, labels) {
  # build old and new labels, stripping indices if scalar
  old_l <- list()
  new_l <- list()
  param <- names(labels)
  for (i in seq_along(param)) {
    p <- param[i]
    if (is.null(labels[[p]])) {
      old_l[[i]] <- paste0(p, "[1]")
      new_l[[i]] <- p
    } else {
      old_l[[i]] <- paste0(p, "[", seq_along(labels[[p]]), "]")
      new_l[[i]] <- paste0(p, "[", labels[[p]], "]")
    }
  }
  old_l <- unlist(old_l)
  new_l <- unlist(new_l)

  # apply new labels to draws list and return
  var_names <- names(draws_list[[1]])
  names(var_names) <- var_names
  var_names[old_l] <- new_l
  for (i in seq_along(draws_list)) {
    names(draws_list[[i]]) <- var_names
  }
  draws_list
}
