## Tests of rgl_gaussian_2D()

#library(rgl)
library(viridisLite)

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

bad_data1 <- cbind(samp_dat[,c(1,3)], yvalz = rnorm(nrow(samp_dat)))
bad_data2 <- cbind(samp_dat[,c(1,2)], Z_values = rnorm(nrow(samp_dat)))

test_that("rgl_gaussian_2D() fails when nonsense is supplied", {
  expect_error(rgl_gaussian_2D("steve"))
  expect_error(rgl_gaussian_2D(c("a", "b", "c")))
  expect_error(rgl_gaussian_2D())
  expect_error(rgl_gaussian_2D(samp_dat))
  expect_error(rgl_gaussian_2D(bad_data1))
  expect_error(rgl_gaussian_2D(bad_data2))
  expect_error(rgl_gaussian_2D(samp_dat, maxiter = "a"))
})

gauss_fit_ue <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_orientation = "unconstrained")

gauss_fit_ce <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_orientation = 0)

gauss_fit_uel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = "unconstrained")

gauss_fit_cel <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = -1)

gauss_fit_cir <-
  fit_gaussian_2D(samp_dat,
                  method = "circular")

## predict one data set
## Generate a grid of x- and y- values on which to predict
grid <-
  expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
              Y_values = seq(from = -1, to = 4, by = 0.1))

## Predict the values using predict_gaussian_2D
gauss_data <-
  predict_gaussian_2D(
    fit_object = gauss_fit_cel,
    X_values = grid$X_values,
    Y_values = grid$Y_values,
  )

#test that plotting works?
#pdf(file = NULL)
if (interactive()) { rgl_gaussian_2D(gauss_data)}
#dev.off()

test_that("rgl_gaussian_2D() fails when bad args are supplied", {
  expect_error(rgl_gaussian_2D(gauss_data,
                                  viridis_dir = "increasing"))
  expect_error(rgl_gaussian_2D(gauss_data,
                                  viridis_dir = "1"))
  expect_error(rgl_gaussian_2D(gauss_data,
                                  viridis_dir = 10))
})

