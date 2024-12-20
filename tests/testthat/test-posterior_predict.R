library(dplyr)

set.seed(123)

test.data <- exposure.response.sample.with.cov
test.data.short <- sample_n(test.data, 30)

test.fit <- stan_emax(
  formula = resp ~ conc,
  data = test.data,
  chains = 2,
  iter = 1000,
  refresh = 0
)

test.fit.2cov <- stan_emax(
  formula = resp ~ conc,
  data = test.data,
  param.cov = list(emax = "cov2", ec50 = "cov3"),
  chains = 2,
  iter = 1000,
  refresh = 0
)

context("test-posterior_predict")

test_that("returnType specification", {
  expect_error(
    suppressWarnings(posterior_predict(test.fit, returnType = "tabble")),
    "'arg' should be one of*"
  )
})


test_that("posterior prediction with original data", {
  test.pp.matrix <- posterior_predict(test.fit)
  test.pe.matrix <- posterior_epred(test.fit)
  test.pl.matrix <- posterior_linpred(test.fit)

  test.pp.df <- posterior_predict(test.fit, returnType = "dataframe")

  expect_is(test.pp.matrix, "matrix")
  expect_is(test.pe.matrix, "matrix")
  expect_is(test.pl.matrix, "matrix")

  expect_is(test.pp.df, "data.frame")

  expect_equal(dim(test.pp.matrix), c(1000, 60))
  expect_equal(dim(test.pe.matrix), c(1000, 60))
  expect_equal(dim(test.pl.matrix), c(1000, 60))

  expect_equal(nrow(test.pp.df), 60000)

  expect_equal(mean(test.pp.matrix[, 1]), 15, tolerance = 2, scale = 1)
  expect_equal(mean(test.pl.matrix[, 1]), 15.2, tolerance = 1, scale = 1) # expect higher tolerance for expectations
  expect_equal(mean(test.pe.matrix[, 1]), 15.2, tolerance = 1, scale = 1)

  expect_equal(mean(test.pp.matrix[, 30]), 83, tolerance = 15, scale = 1)
  expect_equal(mean(test.pl.matrix[, 30]), 70, tolerance = 5, scale = 1)
  expect_equal(mean(test.pe.matrix[, 30]), 70, tolerance = 5, scale = 1)
})


newdata.vec <- c(0, rstan::summary(test.fit$stanfit, pars = c("ec50"))$summary[, 6])
newdata.df <- data.frame(conc = newdata.vec)

test_that("posterior prediction with new data", {
  test.pp.nd.v <-
    posterior_predict.stanemax(test.fit, newdata = newdata.vec) %>%
    apply(2, FUN = median)
  test.pp.nd.df <-
    posterior_predict.stanemax(test.fit, newdata = newdata.df) %>%
    apply(2, FUN = median)

  expect_equal(test.pp.nd.v, c(15, 55), tolerance = 5)
  expect_equal(test.pp.nd.df, c(15, 55), tolerance = 5)
})




test_that("posterior prediction with new data with covariates", {
  expect_error(
    posterior_predict.stanemax(test.fit.2cov, newdata = newdata.vec),
    "Covariate specified with `param.cov` does not exist in the dataset"
  )
  expect_error(
    posterior_epred.stanemax(test.fit.2cov, newdata = newdata.vec),
    "Covariate specified with `param.cov` does not exist in the dataset"
  )
  expect_error(
    posterior_linpred.stanemax(test.fit.2cov, newdata = newdata.vec),
    "Covariate specified with `param.cov` does not exist in the dataset"
  )


  # Make sure parameter extraction works fine
  param.fit.with2cov <- extract_param_fit(test.fit.2cov$stanfit)
  param.extract.raw <- rstan::extract(test.fit.2cov$stanfit, pars = c("emax", "e0", "ec50"))
  expect_equal(
    filter(param.fit.with2cov, mcmcid == 1) %>% select(emax) %>% distinct() %>% .$emax,
    param.extract.raw$emax[1, ]
  )

  # Make sure posterior_predict works with covariates
  test.pp.tibble <- posterior_predict.stanemax(test.fit.2cov, newdata = test.data.short, returnType = "tibble")
  expect_equal(dim(test.pp.tibble), c(30000, 16))

  # Make sure data is not re-sorted
  expect_equal(
    filter(test.pp.tibble, mcmcid == 1) %>% .$exposure,
    test.data.short$conc
  )
})


test_that("posterior prediction of quantile", {
  test.pp.quantile <- posterior_predict_quantile(test.fit.2cov)

  expect_equal(dim(test.pp.quantile), c(60, 11))
  expect_equal(as.numeric(select(test.pp.quantile, starts_with("ci_"))[1, ]),
    c(11.4, 15.2, 19.2),
    tolerance = 0.1
  )
})


test_that("make sure at least plot() doesn't cause error", {
  g1 <- plot(test.fit)
  g2 <- plot(test.fit.2cov)
  expect_is(g1, "gg")
  expect_is(g2, "gg")
})
