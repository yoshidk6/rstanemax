
mod <- stan_emax(
  formula = resp ~ conc,
  data = exposure.response.sample.with.cov,
  param.cov = list(emax = "cov2", ec50 = "cov3"),
  chains = 2,
  iter = 1000,
  refresh = 0
)

test_that("get_variables() works", {
  expect_equal(
    object = tidybayes::get_variables(mod),
    expected = c("ec50[C1]", "ec50[C0]", "sigma", "gamma", "e0", "emax[B0]",
                 "emax[B2]", "emax[B3]")
  )
})

test_that("spread_draws() works", {
  expect_no_error(tidybayes::spread_draws(mod, emax[condition]))
  out <- tidybayes::spread_draws(mod, emax[condition])
  expect_s3_class(out, "data.frame")
  expect_named(out, c("condition", "emax", ".chain", ".iteration", ".draw"))
  expect_equal(nrow(out), 3000L)
})





