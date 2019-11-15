library(dplyr)

set.seed(0)

## Generate data ##

nsample.dose   <- 20
clearance      <- 0.5
logsd.exposure <- 0.7

dose.conc.cov <-
  tibble(dose = rep(c(0, 100, 1000), each = nsample.dose)) %>%
  mutate(conc = dose / clearance * exp(rnorm(dose, 0, logsd.exposure)),
         cov1 = rep_len(c(0, 1, 1), length.out = nrow(.)),#rbinom(dose, 1, 0.5),
         cov2 = rep_len(c(0, 2, 2, 3), length.out = nrow(.)),
         cov3 = factor(rep_len(c(0, 1, 1, 0, 0), length.out = nrow(.)), levels = c(1, 0)))

e0   <- 10
emax <- 90
ec50 <- 100
sd.response <- 10

test.data <-
  dose.conc.cov %>%
  mutate(cov3num = as.numeric(as.character(cov3)),
         resp =
           (e0 + cov1 * 10) + (emax - cov2 * 5) * conc / ((ec50 + cov3num * 50) + conc) +
           rnorm(conc, 0, sd.response))



context("test-create_model_frame")

test_that("covariate settings", {
  expect_error(check_param_cov(param.cov = list(emax2 = "test")))

  cov.levels <- covs_get_levels(test.data, param.cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"))
  expect_equal(cov.levels$emax, c("2", "0", "3"))
  expect_equal(cov.levels$ec50, c("1", "0"))
  expect_equal(cov.levels$e0,   c("1", "0"))

})
