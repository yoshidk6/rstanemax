library(dplyr)

test.data <- exposure.response.sample.with.cov

set.seed(123)

num.iter <- 500

fit1 <-
  suppressWarnings(stan_emax(formula = resp ~ conc, data = exposure.response.sample.with.cov,
                             chains = 1, iter = num.iter, seed = 12345))
# Case with covariates
fit2 <-
  suppressWarnings(stan_emax(formula = resp ~ conc, data = exposure.response.sample.with.cov,
                             param.cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"),
                             chains = 1, iter = num.iter, seed = 12345))
# Case with overlapping covraiates
fit3 <-
  suppressWarnings(stan_emax(formula = resp ~ conc, data = exposure.response.sample.with.cov,
                             param.cov = list(emax = "cov2", ec50 = "cov2"),
                             chains = 1, iter = num.iter, seed = 12345))


extract_param(fit2)
extract_param(fit3)

test_that("check size of extract_param() output", {
  expect_equal(dim(extract_param(fit1)), c(num.iter/2, 6))
  expect_equal(dim(extract_param(fit2)), c(num.iter/2* 3*2*2, 6+3))
  expect_equal(dim(extract_param(fit3)), c(num.iter/2* 3, 6+1))
})
