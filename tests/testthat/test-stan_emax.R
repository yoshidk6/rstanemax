library(dplyr)

set.seed(0)

## Generate data ##

nsample.dose   <- 10
clearance      <- 0.5
logsd.exposure <- 0.7

dose.exposure <-
  tibble(dose = rep(c(0, 100, 1000), each = nsample.dose)) %>%
  mutate(exposure = dose / clearance * exp(rnorm(dose, 0, logsd.exposure)))

e0   <- 10
emax <- 100
ec50 <- 100
sd.response <- 10

test.data <-
  dose.exposure %>%
  mutate(response =
           e0 + emax * exposure / (ec50 + exposure) +
           rnorm(exposure, 0, sd.response))

test.standata <-
  with(test.data,
  list(exposure = exposure,
       response = response,
       N = length(response)))

set.seed(123)

test.fit <- stan_emax_run(stanmodels$mod_emax_e0,
                          test.standata,
                          chains = 2, iter = 500,
                          refresh = 0)

##########
context("test-stan_emax.R")

test_that("check formula elements", {
  expect_error(stan_emax(cbind(dose, response) ~ exposure, test.data),
               "Only one response variable is allowed")
  expect_error(stan_emax(response ~ dose + exposure, test.data),
               "Only one exposure variable is allowed")
})

test_that("emax model run", {
  expect_is(test.fit, "stanemax")
  expect_equal(dim(test.fit$stanfit), c(250, 2, 66))
  expect_equal(rstan::summary(test.fit$stanfit, pars = c("emax"))$summary[[1]],
               expected = 100, tolerance = 0.05, scale = 100)
})

##########
context("posterior_predict.R")

test.pp.matrix <- posterior_predict.stanemax(test.fit)
test.pp.df     <- posterior_predict.stanemax(test.fit, returnType = "dataframe")

test_that("emax model run", {
  expect_is(test.pp.matrix, "matrix")
  expect_is(test.pp.matrix, "data.frame")

  expect_equal(dim(test.pp.matrix), c(500, 30))
  expect_equal(nrow(test.pp.df), 15000)

  expect_equal(mean(test.pp.matrix[,1]),  10,  tolerance = 2)
  expect_equal(mean(test.pp.matrix[,30]), 100, tolerance = 20)
})




