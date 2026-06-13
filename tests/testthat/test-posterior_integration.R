mod <- stan_emax(
  formula = resp ~ conc,
  data = exposure.response.sample.with.cov,
  param.cov = list(emax = "cov2", ec50 = "cov3"),
  chains = 2,
  iter = 1000,
  refresh = 0
)

test_that("variables() works", {
  expect_equal(
    object = posterior::variables(posterior::as_draws(mod)),
    expected = c(
      "ec50[C1]", "ec50[C0]", "sigma", "gamma", "e0", "emax[B0]",
      "emax[B2]", "emax[B3]"
    )
  )
})

test_that("as_draws_df() contains expected parameters", {
  draws <- posterior::as_draws_df(mod)
  draw_names <- posterior::variables(draws)
  expect_true(all(c("emax[B0]", "emax[B2]", "emax[B3]") %in% draw_names))
  expect_true(all(c("ec50[C1]", "ec50[C0]") %in% draw_names))
  expect_true(all(c("sigma", "gamma", "e0") %in% draw_names))
  expect_s3_class(draws, "draws_df")
  expect_equal(posterior::ndraws(draws), 1000L)
})

test_that("posterior_epred() produces correct tidy structure", {
  for (ndraws in 1L:3L) {
    mat <- withr::with_seed(
      seed = 42L,
      code = posterior_epred(mod, ndraws = ndraws)
    )
    expect_equal(dim(mat), c(ndraws, nrow(exposure.response.sample.with.cov)))
    expect_true(all(is.finite(mat)))
  }
})
