library(dplyr)

# Data for package example

set.seed(0)

nsample.dose   <- 10
clearance      <- 0.5
logsd.exposure <- 0.3

dose.exposure <-
  tibble(dose = rep(c(0, 10, 30, 100, 300, 1000), each = nsample.dose)) %>%
  mutate(exposure = dose / clearance * exp(rnorm(dose, 0, logsd.exposure)))

# ggplot2::qplot(dose, exposure, data = dose.exposure)

e0   <- 10
emax <- 90
ec50 <- 100
sd.response <- 20

exposure.response.sample <-
  dose.exposure %>%
  mutate(response =
           e0 + emax * exposure / (ec50 + exposure) +
           rnorm(exposure, 0, sd.response))

# ggplot2::qplot(exposure, response, data = exposure.response.sample) + ggplot2::scale_x_log10()

save(exposure.response.sample, file = "data/exposure.response.sample.rda")


# Data for test

set.seed(0)

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

exposure.response.sample.test <-
  dose.conc.cov %>%
  mutate(cov3num = as.numeric(as.character(cov3)),
         resp =
           (e0 + cov1 * 10) + (emax - cov2 * 5) * conc / ((ec50 + cov3num * 50) + conc) +
           rnorm(conc, 0, sd.response))


save(exposure.response.sample.test, file = "data/exposure.response.sample.test.rda")
