#' Extract posterior draws of key parameters
#'
#' @export
#' @param object A `stanemax` class object
#' @return A tibble containing posterior draws of key parameters.
#' If covariate(s) are included in the model, posterior draws for different combinations
#' of covariates are supplied in a long format - e.g. if there are posterior draws of 100 samples
#' and 4 levels of the covariates, the returned tibble will have the length of 400
#'

extract_param <- function(object){
  # object <- fit3
  # object <- fit4

  # Obtain relevant posteriors
  posterior.draws.raw <- extract_param_fit(object$stanfit)

  # Create a wide data-frame defining covariate levels

  param.cov <- object$prminput$param.cov
  cov.levels <- object$prminput$cov.levels

  if(is.null(param.cov)){
    posterior.draws.raw.2 <- posterior.draws.raw
  } else {
    posterior.draws.raw.2 <-
      append_cov_for_extract_param(posterior.draws.raw, param.cov, cov.levels)
  }

  # Merge them to generate a return object
  posterior.draws.raw.2 %>%
    dplyr::select(-covemax, -cove0, -covec50) %>%
    dplyr::relocate(emax, e0, ec50, gamma, sigma, .after = dplyr::last_col())

}



append_cov_for_extract_param <- function(posterior.draws.raw, param.cov, cov.levels){

  ## Get a list of covariate levels
  cov.name.levels.list <- list()

  for(k in names(param.cov)){
    cov.name.levels.list[[param.cov[[k]]]] <-
      cov.levels[[k]]
  }
  ## Start a data frame to add indeces

  for(k in 1:length(cov.name.levels.list)){
    if (k == 1){
      prm.cov.df <- dplyr::as_tibble(cov.name.levels.list[1])
    } else {
      prm.cov.df <-
        tidyr::expand_grid(prm.cov.df, dplyr::as_tibble(cov.name.levels.list[k]))
    }
  }

  ## Associate covariate levels to indeces
  for(k in names(param.cov)){
    prm.index.to.level <-
      dplyr::tibble(level = cov.levels[[k]]) %>%
      dplyr::mutate(index = dplyr::row_number())
    names(prm.index.to.level) <-
      c(param.cov[[k]], paste0("cov", k))

    prm.cov.df <-
      dplyr::full_join(prm.cov.df, prm.index.to.level,
                       by = param.cov[[k]])
  }

  posterior.draws.raw.2 <-
    posterior.draws.raw %>%
    dplyr::inner_join(prm.cov.df, by = paste0("cov", names(param.cov)))
}

