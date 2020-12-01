## Part of the gaussplotR package
## Last updated: 2020-11-30 VBB

############################## autofit_gaussian_2D #############################

autofit_gaussian_2D <- function(data,
                                comparison_method = "rmse",
                                maxiter = 1000) {

  ## Fit each of the main types of models
  gauss_fit_ue <-
    fit_gaussian_2D(data,
                    method = "elliptical",
                    orientation_strategy = "unconstrained",
                    maxiter = maxiter)

  gauss_fit_uel <-
    fit_gaussian_2D(data,
                    method = "elliptical_log",
                    orientation_strategy = "unconstrained",
                    maxiter = maxiter)

  gauss_fit_cir <-
    fit_gaussian_2D(data,
                    method = "circular",
                    maxiter = maxiter)

  ## Make a list of all the model outputs
  fit_objects_list <-
    list(
      elliptical_unconstr = gauss_fit_ue,
      elliptical_log_unconstr = gauss_fit_uel,
      circular = gauss_fit_cir
      )

  preferred <- compare_gaussian_fits(fit_objects_list,
                                     comparison_method = comparison_method)

  if (preferred$preferred_model == "circular") {
    return(gauss_fit_cir)
  } else if (preferred$preferred_model == "elliptical_log_unconstr") {
    return(gauss_fit_uel)
  } else {
    return(gauss_fit_ue)
  }

}
