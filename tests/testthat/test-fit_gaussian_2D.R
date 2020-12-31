## Tests of fit_gaussian_2D

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

bad_data1 <- cbind(samp_dat[,c(1,3)], yvalz = rnorm(nrow(samp_dat)))
bad_data2 <- cbind(samp_dat[,c(1,2)], Z_values = rnorm(nrow(samp_dat)))

test_that("fit_gaussian_2D() fails when nonsense is supplied", {
  expect_error(fit_gaussian_2D("steve"))
  expect_error(fit_gaussian_2D(c("a", "b", "c")))
  expect_error(fit_gaussian_2D())
  expect_error(fit_gaussian_2D(bad_data1))
  expect_error(fit_gaussian_2D(bad_data2))
  expect_error(fit_gaussian_2D(samp_dat, method = "steve"))
  expect_error(fit_gaussian_2D(samp_dat, method = 4))
  expect_error(fit_gaussian_2D(data.frame(rnorm(100))))
})

test_that("fit_gaussian_2D() fails when maxiter is invalid", {
  expect_error(fit_gaussian_2D(samp_dat, maxiter = "a"))
})

test_that("fit_gaussian_2D() sends message about constrain_orientation", {
  expect_message(fit_gaussian_2D(samp_dat,
                               constrain_orientation = "a"))
})

test_that("fit_gaussian_2D() fails when user_init is invalid", {
  expect_error(fit_gaussian_2D(samp_dat, user_init = "a"))
})

test_that("fit_gaussian_2D() fails when verbose is invalid", {
  expect_error(fit_gaussian_2D(samp_dat, verbose = "a"))
})


gauss_fit_ue <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_amplitude = TRUE,
                  constrain_orientation = "unconstrained",
                  print_initial_params = TRUE)

gauss_fit_ce <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_amplitude = TRUE,
                  constrain_orientation = 0,
                  print_initial_params = TRUE)

gauss_fit_uel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_amplitude = TRUE,
                  constrain_orientation = "unconstrained",
                  print_initial_params = TRUE)

gauss_fit_cel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_amplitude = TRUE,
                  constrain_orientation = -1,
                  print_initial_params = TRUE)

gauss_fit_cir <-
  fit_gaussian_2D(samp_dat,
                  constrain_amplitude = TRUE,
                  user_init = c(25.72529, -2.5, 1.7, 1.3, 1.6),
                  method = "circular",
                  print_initial_params = TRUE)

test_that("each output has four components", {
  expect_equal(length(gauss_fit_ue), 4)
  expect_equal(length(gauss_fit_ce), 4)
  expect_equal(length(gauss_fit_uel), 4)
  expect_equal(length(gauss_fit_cel), 4)
  expect_equal(length(gauss_fit_cir), 4)
})

test_that("fit_methods are correctly stated", {
  expect_equal(
    gauss_fit_ue$fit_method,
    c(
      method = "elliptical",
      amplitude = "constrained",
      orientation = "unconstrained"
    )
  )
  expect_equal(
    gauss_fit_ce$fit_method,
    c(
      method = "elliptical",
      amplitude = "constrained",
      orientation = "constrained"
    )
  )
  expect_equal(
    gauss_fit_uel$fit_method,
    c(
      method = "elliptical_log",
      amplitude = "constrained",
      orientation = "unconstrained"
    )
  )
  expect_equal(
    gauss_fit_cel$fit_method,
    c(
      method = "elliptical_log",
      amplitude = "constrained",
      orientation = "constrained"
    )
  )
  expect_equal(
    gauss_fit_cir$fit_method,
    c(
      method = "circular",
      amplitude = "constrained",
      orientation = NA
    )
  )
})

test_that("Amplitudes are what we expect", {
  expect_equal(gauss_fit_ue$coefs$Amp, 25.72529, tolerance = 1e-5)
  expect_equal(gauss_fit_ce$coefs$Amp, 25.72529, tolerance = 1e-5)
  expect_equal(gauss_fit_uel$coefs$Amp, 25.72529, tolerance = 1e-5)
  expect_equal(gauss_fit_cel$coefs$Amp, 25.72529, tolerance = 1e-5)
  expect_equal(gauss_fit_cir$coefs$Amp, 25.72529, tolerance = 1e-5)
})

test_that("RSSes are what we expect", {
  expect_equal(gauss_fit_ue$model_error_stats$rss, 196.8072, tolerance = 1e-5)
  expect_equal(gauss_fit_ce$model_error_stats$rss, 885.8916, tolerance = 1e-5)
  expect_equal(gauss_fit_uel$model_error_stats$rss, 214.3652, tolerance = 1e-5)
  expect_equal(gauss_fit_cel$model_error_stats$rss, 906.8782, tolerance = 1e-5)
  expect_equal(gauss_fit_cir$model_error_stats$rss, 906.8782, tolerance = 1e-5)
})


gauss_fit_ue_ua <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_amplitude = FALSE,
                  constrain_orientation = "unconstrained")

gauss_fit_ce_ua <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_amplitude = FALSE,
                  constrain_orientation = 0)

gauss_fit_uel_ua <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_amplitude = FALSE,
                  constrain_orientation = "unconstrained")

gauss_fit_cel_ua <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_amplitude = FALSE,
                  constrain_orientation = -1)

gauss_fit_cir_ua <-
  fit_gaussian_2D(samp_dat,
                  constrain_amplitude = FALSE,
                  method = "circular")

test_that("each output has four components", {
  expect_equal(length(gauss_fit_ue_ua), 4)
  expect_equal(length(gauss_fit_ce_ua), 4)
  expect_equal(length(gauss_fit_uel_ua), 4)
  expect_equal(length(gauss_fit_cel_ua), 4)
  expect_equal(length(gauss_fit_cir_ua), 4)
})

test_that("fit_methods are correctly stated", {
  expect_equal(
    gauss_fit_ue_ua$fit_method,
    c(
      method = "elliptical",
      amplitude = "unconstrained",
      orientation = "unconstrained"
    )
  )
  expect_equal(
    gauss_fit_ce_ua$fit_method,
    c(
      method = "elliptical",
      amplitude = "unconstrained",
      orientation = "constrained"
    )
  )
  expect_equal(
    gauss_fit_uel_ua$fit_method,
    c(
      method = "elliptical_log",
      amplitude = "unconstrained",
      orientation = "unconstrained"
    )
  )
  expect_equal(
    gauss_fit_cel_ua$fit_method,
    c(
      method = "elliptical_log",
      amplitude = "unconstrained",
      orientation = "constrained"
    )
  )
  expect_equal(
    gauss_fit_cir_ua$fit_method,
    c(
      method = "circular",
      amplitude = "unconstrained",
      orientation = NA
    )
  )
})

test_that("Amplitudes are what we expect", {
  expect_equal(gauss_fit_ue_ua$coefs$Amp, 32.25132, tolerance = 1e-5)
  expect_equal(gauss_fit_ce_ua$coefs$Amp, 24.76887, tolerance = 1e-5)
  expect_equal(gauss_fit_uel_ua$coefs$Amp, 32.23457, tolerance = 1e-5)
  expect_equal(gauss_fit_cel_ua$coefs$Amp, 23.18006, tolerance = 1e-5)
  expect_equal(gauss_fit_cir_ua$coefs$Amp, 23.18005, tolerance = 1e-5)
})

test_that("RSSes are what we expect", {
  expect_equal(gauss_fit_ue_ua$model_error_stats$rss,
               156.2272, tolerance = 1e-5)
  expect_equal(gauss_fit_ce_ua$model_error_stats$rss,
               885.831, tolerance = 1e-5)
  expect_equal(gauss_fit_uel_ua$model_error_stats$rss,
               169.8812, tolerance = 1e-5)
  expect_equal(gauss_fit_cel_ua$model_error_stats$rss,
               899.613, tolerance = 1e-5)
  expect_equal(gauss_fit_cir_ua$model_error_stats$rss,
               899.613, tolerance = 1e-5)
})


## supply user-initialized values
ui_e <- c(0.83, 32, 3.57, -3, 2, 1, 1)
ui_el <- c(25, -0.1, -3, 2, 2.2, 1.7)
ui_cir <- c(23, -2.5, 1.7, 1.3, 1.6)

gauss_fit_ue_ui <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_orientation = "unconstrained",
                  user_init = ui_e)
gauss_fit_uel_ui <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = "unconstrained",
                  user_init = ui_el)
gauss_fit_cir_ui <-
  fit_gaussian_2D(samp_dat,
                  method = "circular",
                  user_init = ui_cir)

test_that("User-init Amplitudes are what we expect", {
  expect_equal(gauss_fit_ue_ui$coefs$Amp, 32.25132, tolerance = 1e-5)
  expect_equal(gauss_fit_uel_ui$coefs$Amp, 32.23456, tolerance = 1e-5)
  expect_equal(gauss_fit_cir_ui$coefs$Amp, 23.18005, tolerance = 1e-5)
})

test_that("User-init RSSes are what we expect", {
  expect_equal(gauss_fit_ue_ui$model_error_stats$rss,
               156.2272,
               tolerance = 1e-5)
  expect_equal(gauss_fit_uel_ui$model_error_stats$rss,
               169.8812,
               tolerance = 1e-5)
  expect_equal(gauss_fit_cir_ui$model_error_stats$rss,
               899.613,
               tolerance = 1e-5)
})

