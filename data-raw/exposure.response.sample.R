
library(dplyr)

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

