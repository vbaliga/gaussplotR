## Tests of autofit_gaussian_2D()

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

bad_data1 <- cbind(samp_dat[,c(1,3)], yvalz = rnorm(nrow(samp_dat)))
bad_data2 <- cbind(samp_dat[,c(1,2)], Z_values = rnorm(nrow(samp_dat)))

test_that("autofit_gaussian_2D() fails when nonsense is supplied", {
  expect_error(autofit_gaussian_2D("steve"))
  expect_error(autofit_gaussian_2D(c("a", "b", "c")))
  expect_error(autofit_gaussian_2D())
  expect_error(autofit_gaussian_2D(bad_data1))
  expect_error(autofit_gaussian_2D(bad_data2))
  expect_error(autofit_gaussian_2D(samp_dat[,1:2]))
  expect_error(autofit_gaussian_2D(data.frame(rnorm(100))))
  expect_error(autofit_gaussian_2D(samp_dat,
                                   comparison_method = 2))
  expect_error(autofit_gaussian_2D(samp_dat,
                                   comparison_method = "yes"))
  expect_error(autofit_gaussian_2D(samp_dat,
                                   comparison_method = c(2, 5)))
  expect_error(autofit_gaussian_2D(samp_dat,
                                   maxiter = "2"))
  expect_error(autofit_gaussian_2D(samp_dat,
                                   simplify = "yes"))
})

gauss_auto_rmse <-
  autofit_gaussian_2D(samp_dat,
                      comparison_method = "rmse")
gauss_auto_rss <-
  autofit_gaussian_2D(samp_dat,
                      comparison_method = "rss")
gauss_auto_aic <-
  autofit_gaussian_2D(samp_dat,
                      comparison_method = "AIC")

gauss_auto_rmse_unsimp <-
  autofit_gaussian_2D(samp_dat,
                      comparison_method = "rmse",
                      simplify = FALSE)
gauss_auto_rss_unsimp <-
  autofit_gaussian_2D(samp_dat,
                      comparison_method = "rss",
                      simplify = FALSE)
gauss_auto_aic_unsimp <-
  autofit_gaussian_2D(samp_dat,
                      comparison_method = "AIC",
                      simplify = FALSE)


test_that("autofit_gaussian_2D() arrives at the correct answer", {
  expect_equal(
    gauss_auto_rmse$fit_method,
    c(
      method = "elliptical",
      amplitude = "unconstrained",
      orientation = "unconstrained"
    )
  )
  expect_equal(
    gauss_auto_rss$fit_method,
    c(
      method = "elliptical",
      amplitude = "unconstrained",
      orientation = "unconstrained"
    )
  )
  expect_equal(
    gauss_auto_aic$fit_method,
    c(
      method = "elliptical",
      amplitude = "unconstrained",
      orientation = "unconstrained"
    )
  )
  expect_equal(gauss_auto_rmse$model_error_stats[1, 2],
               2.083181, tolerance = 1e-3)
  expect_equal(gauss_auto_rss$model_error_stats[1, 1],
               156.2272, tolerance = 1e-3)
  expect_equal(gauss_auto_aic$model_error_stats[1, 4],
               171.0041, tolerance = 1e-3)
})
