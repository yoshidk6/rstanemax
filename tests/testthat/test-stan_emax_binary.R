library(dplyr)

set.seed(123)

test.fit <- stan_emax_binary(
  y ~ conc,
  data = exposure.response.sample.binary,
  chains = 1, iter = 1000, seed = 12345, refresh = 0
)

test.fit2 <- stan_emax_binary(
  formula = y ~ conc, data = exposure.response.sample.binary,
  param.cov = list(emax = "sex"),
  chains = 1, iter = 1000, seed = 12345, refresh = 0
)


# Test ------------------------------------------------------------------------

test_that("emax model run", {
  coef.stan <- rstan::summary(test.fit$stanfit, pars = c("e0", "emax", "ec50"))$summary[, 6]
  expect_is(test.fit, "stanemaxbin")
  expect_equal(dim(test.fit$stanfit), c(500, 1, 714))
  expect_equal(coef.stan, c(`e0[1]` = -1.3, `emax[1]` = 2.9, `ec50[1]` = 190), tolerance = 0.1)

  coef.stan <- rstan::summary(test.fit2$stanfit, pars = c("e0", "emax", "ec50"))$summary[, 6]
  expect_is(test.fit, "stanemaxbin")
  expect_equal(dim(test.fit$stanfit), c(500, 1, 714))
  expect_equal(coef.stan, c(`e0[1]` = -1.2, `emax[1]` = 3.4, `emax[2]` = 2.1, `ec50[1]` = 190), tolerance = 0.1)
})


test_that("posterior prediction with original data", {
  set.seed(1234)
  test.pp.matrix <- posterior_predict(test.fit2)
  test.pp.df <- posterior_predict(test.fit2, returnType = "dataframe")

  expect_is(test.pp.matrix, "matrix")
  expect_is(test.pp.df, "data.frame")

  expect_equal(dim(test.pp.matrix), c(500, 101))
  expect_equal(nrow(test.pp.df), 50500)

  expect_equal(mean(test.pp.matrix[, 1]), 0.23, tolerance = 0.1)
  expect_equal(mean(test.pp.matrix[, 90]), 0.86, tolerance = 0.1)
})
