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
                  constrain_orientation = "unconstrained")
gauss_fit_uel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = "unconstrained")
gauss_fit_cir <-
  fit_gaussian_2D(samp_dat,
                  method = "circular")
models_list <-
  list(
    unconstrained_elliptical = gauss_fit_ue,
    unconstrained_elliptical_log = gauss_fit_uel,
    circular = gauss_fit_cir
  )

models_list_un <-
  list(
    gauss_fit_ue,
    gauss_fit_uel,
    gauss_fit_cir
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
  expect_equal(compare_gaussian_fits(models_list_un)$preferred_model,
               "model_1")
  expect_equal(compare_gaussian_fits(models_list_un)$comparison_table[1,3],
               156.2272, tolerance = 1e-2)
  expect_equal(compare_gaussian_fits(models_list_un,
                                     comparison_method = "AIC")$preferred_model,
               "model_1")
  expect_equal(compare_gaussian_fits(models_list_un,
                                     comparison_method = "rss")$preferred_model,
               "model_1")
})

gauss_fit_ue_bad <- gauss_fit_ue
attr(gauss_fit_ue_bad, "gaussplotR") <- "no"

bad_list1 <-
  list(
    gauss_fit_ue_bad,
    gauss_fit_uel,
    gauss_fit_cir
  )

test_that("compare_gaussian_fits() fails when bad fit objects are given", {
  expect_error(compare_gaussian_fits(bad_list1))
  expect_error(compare_gaussian_fits(models_list, comparison_method = 2))
  expect_error(compare_gaussian_fits(models_list, comparison_method = "2"))
})
