library(dplyr)

test.data <- exposure.response.sample.test

set.seed(123)

test.fit <- stan_emax(resp ~ conc, data = test.data,
                      chains = 2, iter = 1000, refresh = 0)

test.fit.nls <- nls(resp ~ e0 + emax * conc / (ec50 + conc),
                    data = test.data,
                    start = list(e0  = 10,
                                 emax= 100,
                                 ec50= 100))

test.fit.cov <- stan_emax(formula = resp ~ conc, data = test.data,
                          param.cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"),
                          chains = 2, iter = 1000,
                          refresh = 0)

## Run with non-numeric covariates
test.data.2 <-
  mutate(test.data,
         cov1 = ifelse(cov1 == 1, "x1", "x0"),
         cov3 = 5)
test.fit.cov2 <- stan_emax(formula = resp ~ conc, data = test.data.2,
                           param.cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"),
                           chains = 2, iter = 1000,
                           refresh = 0)

## Fix emax and e0
test.data.3 <-
  mutate(test.data,
         cov3 = ifelse(cov1 == 1, "x1", "x0"),
         cov1 = "aa",
         cov2 = 5)
test.fit.fix.emax <- stan_emax(formula = resp ~ conc, data = test.data.3,
                               emax.fix = 80, e0 = 0,
                               param.cov = list(emax = "cov2", ec50 = "cov3", e0 = "cov1"),
                               chains = 2, iter = 1000,
                               refresh = 0)


##########
context("stan_emax.R")

tdata1 <- create_standata(data.frame(), gamma.fix = 2, e0.fix = 2)
tdata2 <- create_standata(data.frame(), gamma.fix = NULL, e0.fix = NULL)

test_that("wrong input for create_standata", {
  expect_error(create_standata(data.frame(), gamma.fix = 0, e0.fix = 2),
               "gamma.fix must be NULL or positive numeric")
})

test_that("create standata", {
  expect_equal(tdata1$gamma_fix_flg, 1)
  expect_equal(tdata1$gamma_fix_value, 2)
  expect_equal(tdata2$gamma_fix_flg, 0)
  expect_is(tdata2$gamma_fix_value, "numeric")

  expect_equal(tdata1$e0_fix_flg, 1)
  expect_equal(tdata1$e0_fix_value, 2)
  expect_equal(tdata2$e0_fix_flg, 0)
  expect_is(tdata2$e0_fix_value, "numeric")
})


test_that("emax model run", {
  coef.stan <- rstan::summary(test.fit$stanfit, pars = c("e0","emax","ec50"))$summary[,6]
  coef.nls  <- coef(test.fit.nls)
  expect_is(test.fit, "stanemax")
  expect_equal(dim(test.fit$stanfit), c(500, 2, 428))
  expect_equal(coef.stan/coef.nls, c(`e0[1]`=1, `emax[1]`=1, `ec50[1]`=1), tolerance = 0.1)
})


test_that("emax model run with covs", {
  coef.stan <- rstan::summary(test.fit.cov$stanfit, pars = c("e0","emax","ec50"))$summary[,6]
  expect_equal(length(coef.stan), 7)
})


test_that("emax model run with Emax and E0 fixed", {
  coef.stan <- rstan::summary(test.fit.fix.emax$stanfit, pars = c("e0","emax","ec50"))$summary[,6]
  expect_equal(coef.stan[1:2], c(`e0[1]`=0, `emax[1]`=80))
})

