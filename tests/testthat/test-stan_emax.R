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
       N = length(response),
       gamma_fix_flg = 1,
       gamma_fix_value = 1,
       e0_fix_flg = 0,
       e0_fix_value = 0))

set.seed(123)

test.fit <- stan_emax_run(stanmodels$emax,
                          test.standata,
                          chains = 2, iter = 500,
                          refresh = 0)

##########
context("stan_emax.R")

test_that("check formula elements", {
  expect_error(stan_emax(cbind(dose, response) ~ exposure, test.data),
               "Only one response variable is allowed")
  expect_error(stan_emax(response ~ dose + exposure, test.data),
               "Only one exposure variable is allowed")
})

tdata1 <- create_standata(1, 1, gamma.fix = 2, e0.fix = 2)
tdata2 <- create_standata(1, 1, gamma.fix = NULL, e0.fix = NULL)

test_that("create standata", {
  expect_equal(tdata1$gamma_fix_flg, 0)
  expect_equal(tdata1$gamma_fix_value, 2)
  expect_equal(tdata2$gamma_fix_flg, 1)
  expect_is(tdata2$gamma_fix_value, "numeric")

  expect_equal(tdata1$e0_fix_flg, 0)
  expect_equal(tdata1$e0_fix_value, 2)
  expect_equal(tdata2$e0_fix_flg, 1)
  expect_is(tdata2$e0_fix_value, "numeric")
})

test_that("emax model run", {
  expect_is(test.fit, "stanemax")
  expect_equal(dim(test.fit$stanfit), c(250, 2, 67))
  expect_equal(rstan::summary(test.fit$stanfit, pars = c("emax"))$summary[[1]],
               expected = 100, tolerance = 0.05, scale = 100)
})

##########
context("posterior_predict.R")

test.pp.matrix <- posterior_predict.stanemax(test.fit)
test.pp.df     <- posterior_predict.stanemax(test.fit, returnType = "dataframe")

test_that("emax model run", {
  expect_is(test.pp.matrix, "matrix")
  expect_is(test.pp.df, "data.frame")

  expect_equal(dim(test.pp.matrix), c(500, 30))
  expect_equal(nrow(test.pp.df), 15000)

  expect_equal(mean(test.pp.matrix[,1]),  10,  tolerance = 2)
  expect_equal(mean(test.pp.matrix[,30]), 100, tolerance = 20)
})




