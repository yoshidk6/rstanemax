# Set globalVariables to minimize R CMD check notes


if(getRversion() >= "2.15.1"){
  # pp_calc
  utils::globalVariables(c('e0', 'emax', 'emax', 'exposure', 'ec50', 'respHat', 'sigma', 'mcmcid'))

  # plot.stanemax
  utils::globalVariables(c('response', 'rH025', 'rH500', 'rH975', 're025', 're500', 're975'))
}




# Data description
#' Sample simulated data for exposure-response.
#'
#' @format A data frame with columns:
#' \describe{
#' \item{dose}{Dose levels used for simulation of pharmacokientics}
#' \item{exposure}{Simulated exposure}
#' \item{response}{Simulated pharmacodynamic response}
#' }
#' @examples
#' exposure.response.sample
"exposure.response.sample"
