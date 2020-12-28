## Part of the gaussplotR package
## Last updated: 2020-12-26 VBB

############################## autofit_gaussian_2D #############################

#' Automatically determine the best-fitting 2D Gaussian for a data set
#'
#' @param data A data.frame that contains the raw data (generally rectilinearly
#'   gridded data, but this is not a strict requirement). Columns must be named
#'   \code{"X_values"}, \code{"Y_values"} and \code{"response"}.
#' @param comparison_method One of "rmse", "rss", or "AIC"; what metric should
#' be used to determine the "best-fitting" Gaussian?
#' @param maxiter Default 1000. A positive integer specifying the maximum number
#'   of iterations allowed. See \code{stats::nls.control()} for more details.
#' @param simplify TRUE or FALSE. If TRUE, return only the coefficients, model,
#'   model_error_stats, and fit_method for the best-fitting model. If FALSE, a
#'   model comparison table is also included in the returned list as
#'   \code{$model_comparison}. This table is obtained via
#'   \code{compare_gaussian_fits()}.
#'
#' @details This function runs \code{fit_gaussian_2D()} three times: once for
#'   each of the "main" types of models: 1) elliptical, unconstrained; 2)
#'   elliptical, log; 3) circular. In all three cases, amplitudes and
#'   orientations are unconstrained. The function \code{compare_gaussian_fits()}
#'   is then used to determine which of these three models is the best-fitting,
#'   using the \code{comparison_method} argument to make the decision.
#'
#' @return
#' If \code{simplify = TRUE}, a list with the components:
##' \itemize{
##'  \item{"coefs"} {A data.frame of fitted model parameters.}
##'  \item{"model"} {The model object, fitted by \code{stats::nls()}.}
##'  \item{"model_error_stats"} {A data.frame detailing the rss, rmse, deviance,
##'  and AIC of the fitted model.}
##'  \item{"fit_method"} {A character vector that indicates which method and
##'  orientation strategy was used by this function.}
##' }
##'
##' If \code{simplify = FALSE}, a model comparison table is also included
##' in the returned list as \code{$model_comparison}. This table is obtained
##' via \code{compare_gaussian_fits()}.
#'
#' @author Vikram B. Baliga
#'
#' @export
#'
#' @examples
#' if (interactive()) {
  # library(gaussplotR)
  #
  # ## Load the sample data set
  # data(gaussplot_sample_data)
  #
  # ## The raw data we'd like to use are in columns 1:3
  # samp_dat <-
  #   gaussplot_sample_data[,1:3]
  #
  # ## Automatically determine the best model to use
  # gauss_auto <-
  #   autofit_gaussian_2D(samp_dat)
#' }

autofit_gaussian_2D <- function(data,
                                comparison_method = "rmse",
                                maxiter = 1000,
                                simplify = TRUE) {

  #### Argument checks ####

  ## data names
  if (!any(names(data) == "X_values")) {
    stop("'X_values' column not found")
  }
  if (!any(names(data) == "Y_values")) {
    stop("'Y_values' column not found")
  }
  if (!any(names(data) == "response")) {
    stop("'response' column not found")
  }

  ## comparison_method
  if (!is.character(comparison_method)) {
    stop(
      "'comparison_method' must be one of: 'rmse', 'rss', 'AIC'"
    )
  }
  comparison_method_choices <- c("rmse", "rss", "AIC")
  if (!comparison_method %in% comparison_method_choices) {
    stop(
      "'comparison_method' must be one of: 'rmse', 'rss', 'AIC'"
    )
  }

  if (length(comparison_method) > 1) {
    message(
"More than 1 entry found for 'comparison_method'.
Only the first will be used."
)
  }

  ## maxiter
  if (!is.numeric(maxiter)) {
    stop("'maxiter' must be a numeric")
  }

  ## simplify
  if (!is.logical(simplify)) {
    stop("'simplify' must be TRUE or FALSE")
  }

  ## Fit each of the main types of models
  gauss_fit_ue <-
    fit_gaussian_2D(
      data,
      method = "elliptical",
      constrain_orientation = "unconstrained",
      constrain_amplitude = FALSE,
      maxiter = maxiter
    )

  gauss_fit_uel <-
    fit_gaussian_2D(
      data,
      method = "elliptical_log",
      constrain_orientation = "unconstrained",
      constrain_amplitude = FALSE,
      maxiter = maxiter
    )

  gauss_fit_cir <-
    fit_gaussian_2D(
      data,
      method = "circular",
      constrain_amplitude = FALSE,
      maxiter = maxiter
    )


  ## Make a list of all the model outputs
  fit_objects_list <-
    list(
      elliptical_unconstr = gauss_fit_ue,
      elliptical_log_unconstr = gauss_fit_uel,
      circular = gauss_fit_cir
      )

  preferred <-
    compare_gaussian_fits(
      fit_objects_list,
      comparison_method = comparison_method
      )

  if (simplify == TRUE) {
    if (preferred$preferred_model == "circular") {
      return(gauss_fit_cir)
    } else if (preferred$preferred_model == "elliptical_log_unconstr") {
      return(gauss_fit_uel)
    } else {
      return(gauss_fit_ue)
    }
  } else {
    if (preferred$preferred_model == "circular") {
      output <-
        list(
          model_comparison = preferred,
          gaussplotR_fit = gauss_fit_cir
        )
      return(output)
    } else if (preferred$preferred_model == "elliptical_log_unconstr") {
      output <-
        list(
          model_comparison = preferred,
          gaussplotR_fit = gauss_fit_uel
        )
      return(output)
    } else {
      output <-
        list(
          model_comparison = preferred,
          gaussplotR_fit = gauss_fit_ue
        )
      return(output)
    }
  }

}
