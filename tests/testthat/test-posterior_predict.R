library(dplyr)

test.data <- exposure.response.sample.test

set.seed(123)

test.fit <- stan_emax(resp ~ conc, data = test.data,
                      chains = 2, iter = 1000, refresh = 0)


context("test-posterior_predict")

test_that("returnType specification", {
  expect_error(posterior_predict.stanemax(test.fit, returnType = "tabble"),
               "'arg' should be one of*")
})

test.pp.matrix <- posterior_predict.stanemax(test.fit)
test.pp.df     <- posterior_predict.stanemax(test.fit, returnType = "dataframe")

test_that("posterior prediction with original data", {
  expect_is(test.pp.matrix, "matrix")
  expect_is(test.pp.df, "data.frame")

  expect_equal(dim(test.pp.matrix), c(2000, 60))
  expect_equal(nrow(test.pp.df), 120000)

  expect_equal(mean(test.pp.matrix[,1]),  10,  tolerance = 2,  scale = 1)
  expect_equal(mean(test.pp.matrix[,30]), 100, tolerance = 15, scale = 1)
})

newdata.vec <- c(0, rstan::summary(test.fit$stanfit, pars = c("ec50"))$summary[,6])
newdata.df  <- data.frame(exposure = newdata.vec)
test.pp.nd.v <-
  posterior_predict.stanemax(test.fit, newdata = newdata.vec) %>%
  apply(2, FUN = median)
test.pp.nd.df <-
  posterior_predict.stanemax(test.fit, newdata = newdata.df) %>%
  apply(2, FUN = median)


test_that("posterior prediction with new data", {
  expect_equal(test.pp.nd.v,   c(10, 55),  tolerance = 5, scale = 1)
  expect_equal(test.pp.nd.df,  c(10, 55),  tolerance = 5, scale = 1)
})


