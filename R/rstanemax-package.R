#' The 'rstanemax' package.
#'
#' @description Perform sigmoidal Emax model fit using Stan without writing Stan model code.
#'
#' @name rstanemax-package
#' @aliases rstanemax
#' @useDynLib rstanemax, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom boot inv.logit
#' @importFrom rstantools posterior_predict
#' @importFrom posterior as_draws
#'
#' @references
#' Stan Development Team (2018). RStan: the R interface to Stan. R package version 2.18.2. http://mc-stan.org
#'
"_PACKAGE"
