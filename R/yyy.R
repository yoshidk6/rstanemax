# Set globalVariables to minimize R CMD check notes


if (getRversion() >= "2.15.1") {
  # General
  utils::globalVariables(c("."))

  # posterior predict
  utils::globalVariables(c("e0", "emax", "emax", "exposure", "ec50", "respHat", "sigma", "mcmcid", "dataid", ".linpred", ".epred"))

  # plot.stanemax
  utils::globalVariables(c("response", "ci_low", "ci_med", "ci_high", "pi_low", "pi_high"))

  # create cov groups
  utils::globalVariables(c(
    "covemax", "covec50", "cove0", "covemaxstr", "covec50str", "cove0str",
    "covemaxfct", "covec50fct", "cove0fct", "Covariates"
  ))

  # replace_prm_names
  utils::globalVariables(c("prmname", "prmname2", "index", "prm", "level"))
}


# Data description
#' Sample simulated data for exposure-response.
#'
#' @format A data frame with columns:
#' \describe{
#' \item{dose}{Dose levels used for simulation of pharmacokinetics}
#' \item{exposure}{Simulated exposure}
#' \item{response}{Simulated pharmacodynamic response}
#' }
#' @examples
#' exposure.response.sample
"exposure.response.sample"



#' Sample simulated data for exposure-response with covariates
#'
#' @format A data frame with columns:
#' \describe{
#' \item{dose}{Dose levels used for simulation of pharmacokinetics}
#' \item{conc}{Simulated exposure}
#' \item{resp}{Simulated pharmacodynamic response}
#' \item{cov1}{Covariate 1 for e0}
#' \item{cov2}{Covariate 2 for emax}
#' \item{cov3}{Covariate 3 for ec50 (data type factor)}
#' \item{cov3num}{Covariate 3 for ec50 (data type numeric)}
#' }
#' @examples
#' exposure.response.sample
"exposure.response.sample.with.cov"

#' Sample simulated data for exposure-response for binary endpoint
#'
#' @format A data frame with columns:
#' \describe{
#' \item{conc}{Simulated exposure}
#' \item{y_logit}{Simulated logit(p)}
#' \item{y_prob}{Simulated p}
#' \item{y}{Simulated event (1 or 0)}
#' \item{y_cov_logit}{Simulated logit(p) for model with covariate}
#' \item{y_cov_prob}{Simulated p for model with covariate}
#' \item{y_cov}{Simulated event (1 or 0) for model with covariate}
#' \item{sex_num}{1 or 0}
#' \item{sex}{female or male}
#' }
#' @examples
#' exposure.response.sample
"exposure.response.sample.binary"
