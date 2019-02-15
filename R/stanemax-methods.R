#' Methods for stanemax objects
#'
NULL


#' @rdname stanemax-methods
#' @export
print.stanemax <- function(object) {
  param.extract <- c("emax", "e0", "ec50", "gamma", "sigma")

  print(object$stanfit, pars = param.extract)
}
