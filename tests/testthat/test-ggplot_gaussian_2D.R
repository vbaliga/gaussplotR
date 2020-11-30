## Tests of ggplot_gaussian_2D()

library(ggplot2)
library(metR)

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

test_that("ggplot_gaussian_2D() fails when nonsense is supplied", {
  expect_error(ggplot_gaussian_2D("steve"))
  expect_error(ggplot_gaussian_2D(c("a", "b", "c")))
  expect_error(ggplot_gaussian_2D())
  expect_warning(expect_error(ggplot_gaussian_2D(samp_dat)))
  expect_error(ggplot_gaussian_2D(samp_dat, maxiter = "a"))
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
pdf(file = NULL)
ggplot_gaussian_2D(gauss_data)
dev.off()
