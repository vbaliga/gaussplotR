## Tests of fit_gaussian_2D

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

test_that("fit_gaussian_2D() fails when nonsense is supplied", {
  expect_error(fit_gaussian_2D("steve"))
  expect_error(fit_gaussian_2D(c("a", "b", "c")))
  expect_error(fit_gaussian_2D())
  expect_error(fit_gaussian_2D(data.frame(rnorm(100))))
})

test_that("fit_gaussian_2D() fails when maxiter is invalid", {
  expect_error(fit_gaussian_2D(samp_dat, maxiter = "a"))
})


gauss_fit_ue <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  orientation_strategy = "unconstrained")

gauss_fit_ce <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  orientation_strategy = 0)

gauss_fit_uel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  orientation_strategy = "unconstrained")

gauss_fit_cel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  orientation_strategy = -1)

gauss_fit_cir <-
  fit_gaussian_2D(samp_dat,
                  method = "circular")

test_that("each output has four components", {
  expect_equal(length(gauss_fit_ue), 4)
  expect_equal(length(gauss_fit_ce), 4)
  expect_equal(length(gauss_fit_uel), 4)
  expect_equal(length(gauss_fit_cel), 4)
  expect_equal(length(gauss_fit_cir), 4)
})

test_that("fit_methods are correctly stated", {
  expect_equal(gauss_fit_ue$fit_method, "elliptical_unconstr")
  expect_equal(gauss_fit_ce$fit_method, "elliptical_constr")
  expect_equal(gauss_fit_uel$fit_method, "elliptical_log_unconstr")
  expect_equal(gauss_fit_cel$fit_method, "elliptical_log_constr")
  expect_equal(gauss_fit_cir$fit_method, "circular")
})

test_that("Amplitudes are what we expect", {
  expect_equal(gauss_fit_ue$coefs$Amp, 32.25132, tolerance = 1e-5)
  expect_equal(gauss_fit_ce$coefs$Amp, 24.76887, tolerance = 1e-5)
  expect_equal(gauss_fit_uel$coefs$Amp, 25.72529, tolerance = 1e-5)
  expect_equal(gauss_fit_cel$coefs$Amp, 25.72529, tolerance = 1e-5)
  expect_equal(gauss_fit_cir$coefs$Amp, 23.18005, tolerance = 1e-5)
})

test_that("RSSes are what we expect", {
  expect_equal(gauss_fit_ue$model_error_stats$rss, 156.2272, tolerance = 1e-5)
  expect_equal(gauss_fit_ce$model_error_stats$rss, 885.831, tolerance = 1e-5)
  expect_equal(gauss_fit_uel$model_error_stats$rss, 214.3652, tolerance = 1e-5)
  expect_equal(gauss_fit_cel$model_error_stats$rss, 906.8782, tolerance = 1e-5)
  expect_equal(gauss_fit_cir$model_error_stats$rss, 899.613, tolerance = 1e-5)
})
