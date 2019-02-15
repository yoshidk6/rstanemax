context("test-stan_emax.R")

data <- dplyr::tibble(a = c(4,6,7),
                      b = c(8,11,15),
                      c = c(23,45,76))

test_that("check formula elements", {
  expect_equal(2 * 2, 4)
  expect_error(stan_emax(cbind(b, c) ~ a, data),
               "Only one response variable is allowed")
  expect_error(stan_emax(b ~ a + c, data),
               "Only one exposure variable is allowed")
})
