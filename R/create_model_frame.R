# Create data frame with relevant columns
# (renamed for our purposes, not in the original names)
create_model_frame <- function(formula, data, param.cov = NULL, cov.levels, is.model.fit = TRUE) {
  if (is.model.fit) {
    check_formula(formula, data)
  } else {
    check_covs_in_data(data, param.cov)
    formula <- formula[-2]
  }

  df.cov <- create_df_covs(data, param.cov, cov.levels)
  cov.groups <- create_cov_groups(df.cov, param.cov)

  formula.cov <- stats::update(formula, ~ . + covemax + covec50 + cove0)
  modelframe <- stats::lm(formula.cov, dplyr::bind_cols(data, df.cov), method = "model.frame")

  if (is.model.fit) {
    df.model <-
      modelframe %>%
      as.data.frame() %>%
      dplyr::rename(response = 1, exposure = 2) %>%
      dplyr::left_join(cov.groups, by = c("covemax", "covec50", "cove0"))
  } else {
    df.model <-
      modelframe %>%
      as.data.frame() %>%
      dplyr::rename(exposure = 1) %>%
      dplyr::left_join(cov.groups, by = c("covemax", "covec50", "cove0"))
  }

  return(df.model)
}



create_df_covs <- function(data, param.cov = NULL, cov.levels) {
  df.cov <- data.frame(
    emax = factor(rep(1, nrow(data))),
    ec50 = factor(rep(1, nrow(data))),
    e0 = factor(rep(1, nrow(data)))
  )

  for (k in names(param.cov)) {
    df.cov[[k]] <- factor(data[[param.cov[[k]]]], levels = cov.levels[[k]])
  }

  names(df.cov) <- paste0("cov", names(df.cov))

  return(df.cov)
}


create_cov_groups <- function(df.cov, param.cov) {
  cov.groups <-
    df.cov %>%
    dplyr::mutate(
      covemaxstr = paste0("Emax:", covemax),
      covec50str = paste0("EC50:", covec50),
      cove0str = paste0("E0:", cove0)
    ) %>%
    dplyr::select(covemax, covec50, cove0, covemaxstr, covec50str, cove0str) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      Covariates = "",
      Covariates = purrr::map2_chr(Covariates, covemaxstr, ~ ifelse(is.null(param.cov$emax), .x, paste(.x, .y))),
      Covariates = purrr::map2_chr(Covariates, covec50str, ~ ifelse(is.null(param.cov$ec50), .x, paste(.x, .y))),
      Covariates = purrr::map2_chr(Covariates, cove0str, ~ ifelse(is.null(param.cov$e0), .x, paste(.x, .y))),
      Covariates = trimws(Covariates)
    ) %>%
    dplyr::select(-(covemaxstr:cove0str))

  return(cov.groups)
}


# Get levels to convert covariates into factor
covs_get_levels <- function(data, param.cov = NULL) {
  check_covs_in_data(data, param.cov)

  cov.levels <- list(
    emax = NULL,
    ec50 = NULL,
    e0 = NULL
  )

  for (k in names(cov.levels)) {
    if (!is.null(param.cov[[k]])) {
      cov.levels[[k]] <- levels(factor(data[[param.cov[[k]]]]))
    }
  }

  return(cov.levels)
}



# Check cov are not missing in data
check_covs_in_data <- function(data, param.cov = NULL) {
  for (k in param.cov) {
    if (is.null(data[[k]])) stop("Covariate specified with `param.cov` does not exist in the dataset")
  }
}

check_formula <- function(formula, data) {
  modelframe <- stats::lm(formula, data, method = "model.frame")

  mt <- attr(modelframe, "terms")
  Y <- stats::model.response(modelframe)
  X <- stats::model.matrix(mt, modelframe)

  if (NCOL(Y) != 1) stop("Only one response variable is allowed")
  if (NCOL(X) != 2) stop("Only one exposure variable is allowed")
}
