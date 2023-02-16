
set_prior <- function(standata, priors = NULL){
  # First assign prior automatically
  standata <- set_prior_auto(standata)

  # Replace with manual priors if provided
  if(!is.null(priors$ec50)){
    check_prior(priors$ec50)
    standata$prior_ec50_mu  <- priors$ec50[[1]]
    standata$prior_ec50_sig <- priors$ec50[[2]]
  }
  if(!is.null(priors$emax)){
    check_prior(priors$emax)
    standata$prior_emax_mu  <- priors$emax[[1]]
    standata$prior_emax_sig <- priors$emax[[2]]
  }
  if(!is.null(priors$e0)){
    check_prior(priors$e0)
    standata$prior_e0_mu  <- priors$e0[[1]]
    standata$prior_e0_sig <- priors$e0[[2]]
  }
  if(!is.null(priors$gamma)){
    check_prior(priors$gamma)
    standata$prior_gamma_mu  <- priors$gamma[[1]]
    standata$prior_gamma_sig <- priors$gamma[[2]]
  }
  if(!is.null(priors$sigma)){
    check_prior(priors$sigma)
    standata$prior_sigma_mu  <- priors$sigma[[1]]
    standata$prior_sigma_sig <- priors$sigma[[2]]
  }

  return(standata)
}


check_prior <- function(x){
  if(!inherits(x, "numeric") | length(x) !=2)
    stop("Priors need to be defined with length 2 numeric vectors")
}


set_prior_auto <- function(standata){

  # EC50
  standata$prior_ec50_mu  <- stats::median(standata$exposure)
  standata$prior_ec50_sig <- stats::median(standata$exposure) * 2

  # Emax
  delta <- max(standata$response) - min(standata$response)
  coeflm <- stats::lm(response ~ exposure, data = standata) %>% stats::coef()
  slope <- coeflm[[2]]

  if(slope > 0){
    standata$prior_emax_mu <- delta
  } else {
    standata$prior_emax_mu <- - delta
  }
  standata$prior_emax_sig <- delta

  # E0
  resp.low.exp <- standata$response[standata$exposure <= stats::quantile(standata$exposure, 0.25)]
  standata$prior_e0_mu <- stats::median(resp.low.exp)
  standata$prior_e0_sig <- abs(stats::median(resp.low.exp)) * 2

  # Gamma
  standata$prior_gamma_mu  <- 0
  standata$prior_gamma_sig <- 5

  # Sigma
  standata$prior_sigma_mu  <- 0
  standata$prior_sigma_sig <- stats::sd(standata$response)


  return(standata)
}
