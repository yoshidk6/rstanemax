library(dplyr)

set.seed(123)

test.data <- exposure.response.sample.with.cov

test.fit <- stan_emax(
  formula = resp ~ conc,
  data = test.data,
  chains = 2,
  iter = 100,
  refresh = 0
) |>
  suppressWarnings()

set.seed(123)

test.fit.bin <- stan_emax_binary(
  y ~ conc,
  data = exposure.response.sample.binary,
  chains = 1, iter = 100, seed = 12345, refresh = 0
) |>
  suppressWarnings()

test_that("confirm log lik calculations", {
  log_lik_original <- rstan::extract(extract_stanfit(test.fit), pars = "log_lik")$log_lik
  dimnames(log_lik_original) <- NULL
  log_lik_computed1 <- log_lik(test.fit)
  log_lik_computed2 <- log_lik(test.fit, newdata = test.data)

  expect_equal(log_lik_original, log_lik_computed1)
  expect_equal(log_lik_original, log_lik_computed2)

  log_lik_original <- rstan::extract(extract_stanfit(test.fit.bin), pars = "log_lik")$log_lik
  dimnames(log_lik_original) <- NULL
  log_lik_computed1 <- log_lik(test.fit.bin)
  log_lik_computed2 <- log_lik(test.fit.bin, newdata = exposure.response.sample.binary)

  expect_equal(log_lik_original, log_lik_computed1)
  expect_equal(log_lik_original, log_lik_computed2)
})
