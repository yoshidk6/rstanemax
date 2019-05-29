# Set globalVariables to minimize R CMD check notes


if(getRversion() >= "2.15.1"){
  # pp_calc
  utils::globalVariables(c('e0', 'emax', 'emax', 'exposure', 'ec50', 'respHat', 'sigma', 'mcmcid'))

  # plot.stanemax
  utils::globalVariables(c('response', 'ci_low', 'ci_med', 'ci_high', 'pi_low', 'pi_high'))
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
