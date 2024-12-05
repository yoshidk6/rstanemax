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

conc <- 10^(seq(log10(1), log10(10000), length.out = 101))

fun_emax <- function(x, e0, emax, ec50) e0 + ((emax * x) / (ec50+x))
fun_emax_cov <- function(x, e0, emax, ec50, sex_num) {
  emax <- emax + 1 * sex_num
  e0 + ((emax * x) / (ec50+x))
}

exposure.response.sample.binary <-
  tibble(conc = conc) %>%
  mutate(sex_num = rep(c(0, 1), length.out = length(conc)),
         sex = ifelse(sex_num == 1, "female", "male")) %>%
  mutate(y_logit = fun_emax(conc, e0 = -1.5, emax = 3, ec50 = 100),
         y_prob = boot::inv.logit(y_logit),
         y = rbinom(length(conc), 1, y_prob),
         y_cov_logit = fun_emax_cov(conc, e0 = -1.5, emax = 2, ec50 = 100, sex_num = sex_num),
         y_cov_prob = boot::inv.logit(y_cov_logit),
         y_cov = rbinom(length(conc), 1, y_cov_prob))


library(ggplot2)
ggplot(exposure.response.sample.binary,
       aes(conc, y_prob)) +
  geom_line(color = "tomato") +
  geom_jitter(aes(y = y), width = 0, height = 0.05, alpha = 0.5) +
  scale_x_log10() +
  coord_cartesian(ylim = c(-0.05, 1.05)) +
  xgxr::xgx_stat_ci(bins = 4, conf_level = 0.95, distribution = "binomial",
                    geom = c("point"), shape = 0, size = 4) +
  xgxr::xgx_stat_ci(bins = 4, conf_level = 0.95, distribution = "binomial",
                    geom = c("errorbar"), linewidth = 0.5)

ggplot(exposure.response.sample.binary,
       aes(conc, y_cov_prob)) +
  geom_line(aes(color = sex)) +
  geom_jitter(aes(y = y), width = 0, height = 0.05, alpha = 0.5) +
  scale_x_log10() +
  coord_cartesian(ylim = c(-0.05, 1.05)) +
  xgxr::xgx_stat_ci(bins = 4, conf_level = 0.95, distribution = "binomial",
                    geom = c("point"), shape = 0, size = 4) +
  xgxr::xgx_stat_ci(bins = 4, conf_level = 0.95, distribution = "binomial",
                    geom = c("errorbar"), linewidth = 0.5) +
  facet_wrap(~sex)

usethis::use_data(exposure.response.sample.binary, overwrite = TRUE)
