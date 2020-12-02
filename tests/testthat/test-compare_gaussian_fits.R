## Tests of compare_gaussian_fits()

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

test_that("compare_gaussian_fits() fails when nonsense is supplied", {
  expect_error(compare_gaussian_fits("steve"))
  expect_error(compare_gaussian_fits(c("a", "b", "c")))
  expect_error(compare_gaussian_fits())
  expect_error(compare_gaussian_fits(samp_dat))
  expect_error(compare_gaussian_fits(data.frame(rnorm(100))))
})

gauss_fit_ue <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  orientation_strategy = "unconstrained")
gauss_fit_uel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  orientation_strategy = "unconstrained")
gauss_fit_cir <-
  fit_gaussian_2D(samp_dat,
                  method = "circular")
models_list <-
  list(
    unconstrained_elliptical = gauss_fit_ue,
    unconstrained_elliptical_log = gauss_fit_uel,
    circular = gauss_fit_cir
  )

test_that("compare_gaussian_fits() arrives at the correct answer", {
  expect_equal(compare_gaussian_fits(models_list)$preferred_model,
               "unconstrained_elliptical")
  expect_equal(compare_gaussian_fits(models_list)$comparison_table[1,3],
               156.2272, tolerance = 1e-2)
  expect_equal(compare_gaussian_fits(models_list,
                                     comparison_method = "AIC")$preferred_model,
               "unconstrained_elliptical")
  expect_equal(compare_gaussian_fits(models_list,
                                     comparison_method = "rss")$preferred_model,
               "unconstrained_elliptical")
})
