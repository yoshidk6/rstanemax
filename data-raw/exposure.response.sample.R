library(dplyr)

# Data for package example --------------------------------------------

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


# Data for test --------------------------------------------

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

exposure.response.sample.with.cov <-
  dose.conc.cov %>%
  mutate(cov3num = as.numeric(as.character(cov3)),
         resp =
           (e0 + cov1 * 10) + (emax - cov2 * 5) * conc / ((ec50 + cov3num * 50) + conc) +
           rnorm(conc, 0, sd.response)) %>%
  mutate(cov1 = paste0("A", cov1),
         cov2 = paste0("B", cov2),
         cov3 = paste0("C", cov3),
         cov3 = factor(cov3, levels = c("C1", "C0")))


save(exposure.response.sample.with.cov, file = "data/exposure.response.sample.with.cov.rda")


# Data for test binary --------------------------------------------

set.seed(12345)
inv_logit = function(p){
  return(exp(p)/(1+exp(p)))
}

emax.function <- function(e0, emax, ec50, x){
  y = e0 + ((emax * x) / (ec50+x))
  prob = inv_logit(y)
  return(prob)
}

emax.function.covar <- function(e0, emax, ec50, covageemax, covsexemax, x, age, sex){
  y = e0 + (( (emax + covageemax*age + covsexemax*sex ) * x) / (ec50+x))
  prob = inv_logit(y)
  return(prob)
}

n <- 200
conc <- as.numeric(seq(0.2, 900, length.out = n))
sim.sex <- rbinom(n, 1, prob = c(0.5,0.5))
sim.age <- rnorm(n, 50)
sim.gender <- ifelse(sim.sex == 1, "female", "male")


exposure.response.sample.binary <-
  tibble(conc =conc,
    age = sim.age,
    sex = sim.sex,
    gender = sim.gender) %>%
  mutate(response = as.numeric(rbinom(n,1, emax.function(e0 = -2, emax = 3,ec50 = 400, conc))),
    response.covars = as.numeric(rbinom(n,1, emax.function.covar(-2, 2, 400, 0.002, -0.01, conc, age, sex))))

usethis::use_data(exposure.response.sample.binary, overwrite = TRUE)
