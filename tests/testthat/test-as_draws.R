
set.seed(123)

fit <- suppressWarnings(stan_emax(
  formula = response ~ exposure,
  data = exposure.response.sample,
  chains = 2,
  iter = 500,
  show_messages = FALSE
))

test_that("as_draws works", {

  expect_s3_class(posterior::as_draws_list(fit), "draws_list")
  expect_s3_class(posterior::as_draws_array(fit), "draws_array")
  expect_s3_class(posterior::as_draws_df(fit), "draws_df")
  expect_s3_class(posterior::as_draws_matrix(fit), "draws_matrix")
  expect_s3_class(posterior::as_draws_rvars(fit), "draws_rvars")

})
