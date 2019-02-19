# Set globalVariables to minimize R CMD check notes

# pp_calc
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c('e0', 'emax', 'emax', 'exposure', 'ec50', 'respHat', 'sigma', 'mcmcid'))
}


