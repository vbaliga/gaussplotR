## Tests of get_volume_gaussian_2D()

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

test_that("get_volume_gaussian_2D() fails when nonsense is supplied", {
  expect_error(get_volume_gaussian_2D("steve"))
  expect_error(get_volume_gaussian_2D(c("a", "b", "c")))
  expect_error(get_volume_gaussian_2D())
  expect_error(get_volume_gaussian_2D(5, "a"))
  expect_error(get_volume_gaussian_2D("a", "5"))
  expect_error(get_volume_gaussian_2D(X_sig = c(1:3), Y_sig = 5))
  expect_error(get_volume_gaussian_2D(samp_dat))
  expect_error(get_volume_gaussian_2D(samp_dat, maxiter = "a"))
})

gauss_fit_cir <-
  fit_gaussian_2D(samp_dat,
                  method = "circular")

## Compute volume
vol_ue <-
  get_volume_gaussian_2D(gauss_fit_cir$coefs$X_sig, gauss_fit_cir$coefs$Y_sig)

test_that("get_volume_gaussian_2D() computes the expected value", {
  expect_equal(vol_ue, 9.239037, tolerance = 1e-5)
})

