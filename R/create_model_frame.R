
# Create data frame with relevant columns
# (renamed for our purposes, not in the original names)
create_model_frame <- function(formula, data, param.cov = NULL, cov.levels, is.model.fit = TRUE){

  if(is.model.fit) {
    check_formula(formula, data)
  } else {
    check_covs_in_data(data, param.cov)
    formula <- formula[-2]
  }

  df.cov <- create_df_covs(data, param.cov, cov.levels)

  formula.cov <- update(formula, ~ . + covemax + covec50 + cove0)
  modelframe <- stats::lm(formula.cov, bind_cols(data, df.cov), method = "model.frame")

  if(is.model.fit) {
    df.model <-
      modelframe %>%
      as.data.frame() %>%
      rename(response = 1, exposure = 2)
  } else {
    df.model <-
      modelframe %>%
      as.data.frame() %>%
      rename(exposure = 1)
  }

  return(df.model)
}



create_df_covs <- function(data, param.cov = NULL, cov.levels){

  df.cov <- data.frame(emax = factor(rep(1, nrow(data))),
                       ec50 = factor(rep(1, nrow(data))),
                       e0   = factor(rep(1, nrow(data))))

  for(k in names(param.cov)){
    df.cov[[k]] <- factor(data[[param.cov[[k]]]], levels = cov.levels[[k]])
  }

  names(df.cov) <- paste0("cov", names(df.cov))

  return(df.cov)
}


# Get levels to convert covariates into factor
covs_get_levels <- function(data, param.cov = NULL){

  check_covs_in_data(data, param.cov)

  cov.levels <- list(emax = NULL,
                     ec50 = NULL,
                     e0   = NULL)

  for(k in names(cov.levels)){
    if(!is.null(param.cov[[k]])){
      cov.levels[[k]] = levels(factor(data[[param.cov[[k]]]]))
    }
  }

  return(cov.levels)
}



# Check cov are not missing in data
check_covs_in_data <- function(data, param.cov = NULL){

  for(k in param.cov) {
    if(is.null(data[[k]])) stop("Covariate specified with `param.cov` does not exist in the dataset")
  }

}

check_formula <- function(formula, data){

  modelframe <- stats::lm(formula, data, method = "model.frame")

  mt <- attr(modelframe, "terms")
  Y <- stats::model.response(modelframe)
  X <- stats::model.matrix(mt, modelframe)

  if(NCOL(Y) != 1) stop("Only one response variable is allowed")
  if(NCOL(X) != 2) stop("Only one exposure variable is allowed")
}

