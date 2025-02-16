library(dplyr)

set.seed(0)

## Generate data ##

nsample.dose <- 10
clearance <- 0.5
logsd.exposure <- 0.7

dose.exposure <-
  tibble(dose = rep(c(0, 100, 1000), each = nsample.dose)) %>%
  mutate(exposure = dose / clearance * exp(rnorm(dose, 0, logsd.exposure)))

e0 <- 10
emax <- 90
ec50 <- 100
sd.response <- 10

test.data <-
  dose.exposure %>%
  mutate(response = e0 + emax * exposure / (ec50 + exposure) + rnorm(exposure, 0, sd.response))


df.model <- create_model_frame(response ~ exposure, test.data, cov.levels = covs_get_levels(test.data))
test.standata.noprior <-
  create_standata(df.model,
    gamma.fix = 1, e0.fix = NULL
  )

##########
context("test-set_prior.R")

t.sdata.autoprior <- set_prior_auto(test.standata.noprior, endpoint_type = "continuous")

test_that("auto prior set", {
  expect_equal(t.sdata.autoprior$prior_ec50_mu, 156.0748, tolerance = 0.1)
  expect_equal(t.sdata.autoprior$prior_ec50_sig, 312.1496, tolerance = 0.1)
  expect_equal(t.sdata.autoprior$prior_emax_mu, 116.9879, tolerance = 0.1)
  expect_equal(t.sdata.autoprior$prior_emax_sig, 116.9879, tolerance = 0.1)
  expect_equal(t.sdata.autoprior$prior_e0_mu, 7.424736, tolerance = 0.1)
  expect_equal(t.sdata.autoprior$prior_e0_sig, 14.84947, tolerance = 0.1)
  expect_equal(t.sdata.autoprior$prior_gamma_mu, 0, tolerance = 0.1)
  expect_equal(t.sdata.autoprior$prior_gamma_sig, 5, tolerance = 0.1)
  expect_equal(t.sdata.autoprior$prior_sigma_mu, 0, tolerance = 0.1)
  expect_equal(t.sdata.autoprior$prior_sigma_sig, 36.84818, tolerance = 0.1)
})

t.sdata.manuprior <-
  set_prior(test.standata.noprior,
    priors = list(
      ec50 = c(1, 2),
      emax = c(3, 4),
      e0 = c(5, 6),
      gamma = c(7, 8),
      sigma = c(9, 10)
    )
  )

test_that("manual prior set", {
  expect_equal(t.sdata.manuprior$prior_ec50_mu, 1)
  expect_equal(t.sdata.manuprior$prior_ec50_sig, 2)
  expect_equal(t.sdata.manuprior$prior_emax_mu, 3)
  expect_equal(t.sdata.manuprior$prior_emax_sig, 4)
  expect_equal(t.sdata.manuprior$prior_e0_mu, 5)
  expect_equal(t.sdata.manuprior$prior_e0_sig, 6)
  expect_equal(t.sdata.manuprior$prior_gamma_mu, 7)
  expect_equal(t.sdata.manuprior$prior_gamma_sig, 8)
  expect_equal(t.sdata.manuprior$prior_sigma_mu, 9)
  expect_equal(t.sdata.manuprior$prior_sigma_sig, 10, tolerance = 0.1)

  expect_error(
    set_prior(test.standata.noprior, priors = list(ec50 = c(1))),
    "Priors need to be defined with length 2 numeric vectors"
  )
  expect_error(
    set_prior(test.standata.noprior, priors = list(emax = c(1, 2, 3))),
    "Priors need to be defined with length 2 numeric vectors"
  )
  expect_error(
    set_prior(test.standata.noprior, priors = list(e0 = "test")),
    "Priors need to be defined with length 2 numeric vectors"
  )
})
