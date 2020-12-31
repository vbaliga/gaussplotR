## Tests of predict_gaussian_2D()

data(gaussplot_sample_data)
samp_dat <-
  gaussplot_sample_data[,1:3]

fake_obj1 <-
  list(model = lm(samp_dat$response ~ samp_dat$X_values))

fake_obj2 <-
  list(coefs = coef(lm(samp_dat$response ~ samp_dat$X_values)))

fake_obj3 <-
  list(
    coefs = coef(lm(samp_dat$response ~ samp_dat$X_values)),
    model = lm(samp_dat$response ~ samp_dat$X_values),
    fit_method = "steve"
  )

test_that("predict_gaussian_2D() fails when nonsense is supplied", {
  expect_error(predict_gaussian_2D("steve"))
  expect_error(predict_gaussian_2D(c("a", "b", "c")))
  expect_error(predict_gaussian_2D())
  expect_error(predict_gaussian_2D(samp_dat))
  expect_error(predict_gaussian_2D(fake_obj1))
  expect_error(predict_gaussian_2D(fake_obj2))
  expect_error(predict_gaussian_2D(fake_obj3))
  expect_error(predict_gaussian_2D(data.frame(rnorm(100))))
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

## Generate a grid of x- and y- values on which to predict
grid <-
  expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
              Y_values = seq(from = -1, to = 4, by = 0.1))

bad_grid1 <-
  data.frame(
    X_values = rnorm(100),
    Y_values = "a"
  )

bad_grid2 <-
  data.frame(
    X_values = "a",
    Y_values = rnorm(100)
  )


## Predict the values using predict_gaussian_2D
gauss_data_ue <-
  predict_gaussian_2D(
    fit_object = gauss_fit_ue,
    X_values = grid$X_values,
    Y_values = grid$Y_values,
  )
gauss_data_ce <-
  predict_gaussian_2D(
    fit_object = gauss_fit_ce,
    X_values = grid$X_values,
    Y_values = grid$Y_values,
  )
gauss_data_uel <-
  predict_gaussian_2D(
    fit_object = gauss_fit_uel,
    X_values = grid$X_values,
    Y_values = grid$Y_values,
  )
gauss_data_cel <-
  predict_gaussian_2D(
    fit_object = gauss_fit_cel,
    X_values = grid$X_values,
    Y_values = grid$Y_values,
  )
gauss_data_cir <-
  predict_gaussian_2D(
    fit_object = gauss_fit_cir,
    X_values = grid$X_values,
    Y_values = grid$Y_values,
  )

test_that("predict_gaussian_2D() fails when bad grid is supplied", {
  expect_error(predict_gaussian_2D(gauss_fit_ue))
  expect_error(predict_gaussian_2D(gauss_fit_ue,
                                   X_values = bad_grid1$X_values,
                                   Y_values = bad_grid1$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_ue,
                                   X_values = bad_grid2$X_values,
                                   Y_values = bad_grid2$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_ue,
                                   X_values = grid$X_values,
                                   Y_values = 1:3))
})



test_that("each output has correct dimensions", {
  expect_equal(dim(gauss_data_ue),  c(2601, 3))
  expect_equal(dim(gauss_data_ce),  c(2601, 3))
  expect_equal(dim(gauss_data_uel), c(2601, 3))
  expect_equal(dim(gauss_data_cel), c(2601, 3))
  expect_equal(dim(gauss_data_cir), c(2601, 3))
})

test_that("column names are correct", {
  expect_equal(colnames(gauss_data_ue),
               c("X_values", "Y_values", "predicted_values"))
  expect_equal(colnames(gauss_data_ce),
               c("X_values", "Y_values", "predicted_values"))
  expect_equal(colnames(gauss_data_uel),
               c("X_values", "Y_values", "predicted_values"))
  expect_equal(colnames(gauss_data_cel),
               c("X_values", "Y_values", "predicted_values"))
  expect_equal(colnames(gauss_data_cir),
               c("X_values", "Y_values", "predicted_values"))
})

test_that("predicted_values are what we expect", {
  expect_equal(gauss_data_ue$predicted_values[1],  4.946876, tolerance = 1e-5)
  expect_equal(gauss_data_ce$predicted_values[1],  1.509221, tolerance = 1e-5)
  expect_equal(gauss_data_uel$predicted_values[1], 5.033768, tolerance = 1e-5)
  expect_equal(gauss_data_cel$predicted_values[1], 0.9435862, tolerance = 1e-5)
  expect_equal(gauss_data_cir$predicted_values[1], 0.943607, tolerance = 1e-5)
})

gauss_fit_ue_bad1 <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_orientation = "unconstrained")
names(gauss_fit_ue_bad1$coefs) <- c("a", 5, 6, "b", "2", 6, "last")
gauss_fit_ue_bad2 <- gauss_fit_ue_bad1
gauss_fit_ue_bad2$coefs <- c(4, 2, 4)

gauss_fit_ce_bad1 <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical",
                  constrain_orientation = 0)
names(gauss_fit_ce_bad1$coefs) <- c("a", 5, 6, "b", "2", 6, "last")
gauss_fit_ce_bad2 <- gauss_fit_ce_bad1
gauss_fit_ce_bad2$coefs <- c(4, 2, 4)

gauss_fit_uel_bad1 <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = "unconstrained")
names(gauss_fit_uel_bad1$coefs) <- c("a", 5, 6, "b", "2", "last")
gauss_fit_uel_bad2 <- gauss_fit_uel_bad1
gauss_fit_uel_bad2$coefs <- c(4, 2, 4)

gauss_fit_cel_bad1 <-
  fit_gaussian_2D(samp_dat,
                  method = "elliptical_log",
                  constrain_orientation = -1)
names(gauss_fit_cel_bad1$coefs) <- c("a", 5, 6, "b", "2", "last")
gauss_fit_cel_bad2 <- gauss_fit_cel_bad1
gauss_fit_cel_bad2$coefs <- c(4, 2, 4)

gauss_fit_cir_bad1 <-
  fit_gaussian_2D(samp_dat,
                  method = "circular")
names(gauss_fit_cir_bad1$coefs) <- c("a", 5, 6, "b", "last")
gauss_fit_cir_bad2 <- gauss_fit_cir_bad1
gauss_fit_cir_bad2$coefs <- c(4, 2, 4)

test_that("predict_gaussian_2D() fails when models are misspecified", {
  expect_error(predict_gaussian_2D(gauss_fit_ue_bad1,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_ue_bad2,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_ce_bad1,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_ce_bad2,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_uel_bad1,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_uel_bad2,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_cel_bad1,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_cel_bad2,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_cir_bad1,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))
  expect_error(predict_gaussian_2D(gauss_fit_cir_bad2,
                                   X_values = grid$X_values,
                                   Y_values = grid$Y_values))

})
