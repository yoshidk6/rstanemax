mod <- stan_emax(
  formula = resp ~ conc,
  data = exposure.response.sample.with.cov,
  param.cov = list(emax = "cov2", ec50 = "cov3"),
  chains = 2,
  iter = 1000,
  refresh = 0
)

test_that("get_variables() works", {
  expect_equal(
    object = tidybayes::get_variables(mod),
    expected = c(
      "ec50[C1]", "ec50[C0]", "sigma", "gamma", "e0", "emax[B0]",
      "emax[B2]", "emax[B3]"
    )
  )
})

test_that("spread_draws() works", {
  expect_no_error(tidybayes::spread_draws(mod, emax[condition]))
  out <- tidybayes::spread_draws(mod, emax[condition])
  expect_s3_class(out, "data.frame")
  expect_named(out, c("condition", "emax", ".chain", ".iteration", ".draw"))
  expect_equal(nrow(out), 3000L)
})

test_that("add_epred_draws() works", {
  for (ndraws in 1L:3L) {
    for (seed in 1L:3L) {
      # construct expected result manually
      draw_rows <- expand.grid(
        .draw = 1L:ndraws,
        .row = seq_len(nrow(exposure.response.sample.with.cov))
      )
      d1 <- exposure.response.sample.with.cov |>
        dplyr::mutate(.row = dplyr::row_number()) |>
        dplyr::left_join(
          tibble::tibble(
            .row = draw_rows$.row,
            .chain = NA_integer_,
            .iteration = NA_integer_,
            .draw = draw_rows$.draw,
            .epred = as.vector(
              withr::with_seed(
                seed = seed,
                code = posterior_epred(mod, ndraws = ndraws)
              )
            )
          ),
          by = ".row"
        )

      # construct with tidybayes and drop grouping
      d2 <- tidybayes::add_epred_draws(
        newdata = exposure.response.sample.with.cov,
        object = mod,
        ndraws = ndraws,
        seed = seed
      ) |>
        dplyr::ungroup()

      expect_equal(d2, d1)
    }
  }
})
