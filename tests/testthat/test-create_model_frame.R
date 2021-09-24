library(dplyr)

test.data <- exposure.response.sample.with.cov

df.model <- create_model_frame(resp ~ conc, test.data, cov.levels = covs_get_levels(test.data))


context("test-create_model_frame")


test_that("check formula elements", {
  expect_error(stan_emax(cbind(dose, resp) ~ conc, test.data),
               "Only one response")
  expect_error(stan_emax(resp ~ dose + conc, test.data),
               "Only one exposure")
})



test_that("covariate settings", {
  expect_error(check_param_cov(param.cov = list(emax2 = "test")))

  cov.levels <- covs_get_levels(test.data, param.cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"))
  expect_equal(cov.levels$emax, c("B0", "B2", "B3"))
  expect_equal(cov.levels$ec50, c("C1", "C0"))
  expect_equal(cov.levels$e0,   c("A0", "A1"))

})

