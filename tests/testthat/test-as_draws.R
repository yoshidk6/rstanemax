set.seed(123)

mods <- list()
pars <- list()
pars_wo_index <- list()

mods[[1]] <- suppressWarnings(stan_emax(
  formula = response ~ exposure,
  data = exposure.response.sample,
  chains = 1, iter = 50, refresh = 0,
  show_messages = FALSE
))
pars[[1]] <- c("ec50", "sigma", "gamma", "e0", "emax")
pars_wo_index[[1]] <- c("ec50", "sigma", "gamma", "e0", "emax")

mods[[2]] <- suppressWarnings(stan_emax(
  formula = resp ~ conc,
  data = exposure.response.sample.with.cov,
  param.cov = list(emax = "cov1", e0 = "cov2"),
  chains = 1, iter = 50, refresh = 0,
  show_messages = FALSE
))
pars[[2]] <- c(
  "ec50", "sigma", "gamma", "e0[B0]", "e0[B2]",
  "e0[B3]", "emax[A0]", "emax[A1]"
)
pars_wo_index[[2]] <- c("ec50", "sigma", "gamma", "e0", "emax")


mods[[3]] <- suppressWarnings(stan_emax_binary(
  formula = y ~ conc,
  data = exposure.response.sample.binary,
  param.cov = list(emax = "sex"),
  chains = 1, iter = 50, refresh = 0,
  show_messages = FALSE
))
pars[[3]] <- c(
  "ec50", "gamma", "e0", "emax[female]", "emax[male]"
)
pars_wo_index[[3]] <- c("ec50", "gamma", "e0", "emax")

test_that("as_draws coercion works", {
  for (m in mods) {
    expect_s3_class(posterior::as_draws_list(m), "draws_list")
    expect_s3_class(posterior::as_draws_array(m), "draws_array")
    expect_s3_class(posterior::as_draws_df(m), "draws_df")
    expect_s3_class(posterior::as_draws_matrix(m), "draws_matrix")
    expect_s3_class(posterior::as_draws_rvars(m), "draws_rvars")
  }
})

test_that("as_draws preserves covariate labels", {
  for (i in seq_along(mods)) {
    expect_equal(
      object = posterior::variables(
        x = posterior::as_draws_list(mods[[i]]),
        with_indices = TRUE
      ),
      expected = pars[[i]]
    )

    expect_equal(
      object = posterior::variables(
        x = posterior::as_draws_array(mods[[i]]),
        with_indices = TRUE
      ),
      expected = pars[[i]]
    )

    expect_equal(
      object = posterior::variables(
        x = posterior::as_draws_df(mods[[i]]),
        with_indices = TRUE
      ),
      expected = pars[[i]]
    )

    expect_equal(
      object = posterior::variables(
        x = posterior::as_draws_matrix(mods[[i]]),
        with_indices = TRUE
      ),
      expected = pars[[i]]
    )

    expect_equal(
      object = posterior::variables(
        x = posterior::as_draws_rvars(mods[[i]]),
        with_indices = TRUE
      ),
      expected = pars[[i]]
    )

    expect_equal(
      object = posterior::variables(
        x = posterior::as_draws_rvars(mods[[i]]),
        with_indices = FALSE
      ),
      expected = pars_wo_index[[i]]
    )
  }
})
