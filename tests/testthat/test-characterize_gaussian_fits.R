## Tests of characterize_gaussian_fits

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

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
