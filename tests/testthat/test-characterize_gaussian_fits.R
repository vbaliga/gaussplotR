## Tests of characterize_gaussian_fits

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

## Bad data

bad_data1 <- cbind(samp_dat[,c(1,3)], yvalz = rnorm(nrow(samp_dat)))
bad_data2 <- cbind(samp_dat[,c(1,2)], Z_values = rnorm(nrow(samp_dat)))

test_that("characterize_gaussian_fits() fails when nonsense is supplied", {
  expect_error(characterize_gaussian_fits("steve"))
  expect_error(characterize_gaussian_fits(c("a", "b", "c")))
  expect_error(characterize_gaussian_fits())
  expect_error(characterize_gaussian_fits(samp_dat[,1:2]))
  expect_error(characterize_gaussian_fits(bad_data1))
  expect_error(characterize_gaussian_fits(bad_data2))
  expect_error(characterize_gaussian_fits(samp_dat, method = "steve"))
  expect_error(characterize_gaussian_fits(samp_dat, method = 4))
  expect_error(characterize_gaussian_fits(data.frame(rnorm(100))))
  expect_error(characterize_gaussian_fits(samp_dat,
                                          comparison_method = 2))
  expect_error(characterize_gaussian_fits(samp_dat,
                                          comparison_method = "yes"))
  expect_error(characterize_gaussian_fits(samp_dat,
                                          comparison_method = c(2, 5)))
  expect_error(characterize_gaussian_fits(samp_dat,
                                          maxiter = "2"))
  expect_error(characterize_gaussian_fits(samp_dat,
                                          simplify = "yes"))
})

## Bad models
gauss_fit_ue <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_orientation = "unconstrained")
gauss_fit_uel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = "unconstrained")
gauss_fit_uel_CA <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = "unconstrained",
                  constrain_amplitude = TRUE)
gauss_fit_cir <-
  fit_gaussian_2D(samp_dat,
                  method = "circular")
bad_models_list1 <-
  list(
    unconstrained_elliptical = gauss_fit_ue,
    unconstrained_elliptical_log = gauss_fit_uel,
    circular = gauss_fit_cir
  )

bad_models_list2 <-
  list(
    unconstrained_elliptical_log = gauss_fit_uel,
    unconstrained_elliptical_log = gauss_fit_uel,
    unconstrained_elliptical_log = gauss_fit_uel
  )

bad_models_list3 <-
  list(
    unconstrained_elliptical_log = gauss_fit_uel
  )

bad_models_list4 <-
  list(
    unconstrained_elliptical_log = gauss_fit_uel,
    gauss_fit_uel_CA = gauss_fit_uel_CA
  )

bad_models_list5 <-
  list(
    unconstrained_elliptical_log = gauss_fit_uel,
    unconstrained_elliptical_log = gauss_fit_uel,
    gauss_fit_uel_CA = gauss_fit_uel_CA
  )

test_that("characterize_gaussian_fits() fails bad models are supplied", {
  expect_error(characterize_gaussian_fits(bad_models_list1))
  expect_error(characterize_gaussian_fits(bad_models_list2))
  expect_error(characterize_gaussian_fits(bad_models_list3))
  expect_error(characterize_gaussian_fits(bad_models_list4))
  expect_error(characterize_gaussian_fits(bad_models_list5))
})

## Good data
no_models <-
  characterize_gaussian_fits(data = samp_dat)

## Good models
gauss_fit_uel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = "unconstrained",
                  constrain_amplitude = FALSE)
gauss_fit_zer <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = 0,
                  constrain_amplitude = FALSE)
gauss_fit_ngo <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = -1,
                  constrain_amplitude = FALSE)

good_models <-
  list(
    gauss_fit_uel = gauss_fit_uel,
    gauss_fit_zer = gauss_fit_zer,
    gauss_fit_ngo = gauss_fit_ngo
  )

with_models <-
  characterize_gaussian_fits(good_models)
