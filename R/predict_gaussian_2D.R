## Part of the gaussplotR package
## Last updated: 2020-11-27 VBB

############################# predict_gaussian_2D ##############################

#' Predict values from a fitted 2D gaussian
#'
#' @param fit_params vector of parameters for a 2D Gaussian that have been
#'   fitted to a data set. See \code{gaussplotR::fit_gaussian_2D()} for details.
#'   The resultant object from that function can be used here.
#' @param X_values vector of numeric values for the x-axis
#' @param Y_values vector of numeric values for the y-axis
#' @param ... Additional arguments
#'
#'
#' @section Warning:
#' This function assumes Gaussian parameters have been fitted beforehand. No
#' fitting of parameters is done within this function; they all must be
#' supplied via the object created by \code{gaussplotR::fit_gaussian_2D()}.
#'
#' @return A data.frame with the supplied \code{X_values} and \code{Y_values}
#'   along with the predicted values of the 2D gaussian
#'   (\code{predicted_values})
#'
#' @author Vikram B. Baliga
#'
#' @export
#'
#' @inherit fit_gaussian_2D examples



predict_gaussian_2D <- function(fit_params,
                                X_values,
                                Y_values,
                                ...) {

  ## Create NULL vector to fill with predicted values
  preds <- NULL

  ## Elliptical, unconstrained ##
  if (attr(fit_params, "fit_method") == "elliptical_unconstr") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        fit_params$A_o + fit_params$Amp * exp(
          -(
            (((X_values[i] -
                 fit_params$X_peak)*cos(fit_params$theta) -
                (Y_values[i] -
                   fit_params$Y_peak)*sin(fit_params$theta))/fit_params$a)^2 +
              (((X_values[i] -
                   fit_params$X_peak)*sin(fit_params$theta) -
                  (Y_values[i] -
                     fit_params$Y_peak)*cos(fit_params$theta))/fit_params$b)^2
          ) / 2
        )
    }
  }

  ## Elliptical, constrained ##
  if (attr(fit_params, "fit_method") == "elliptical_constr") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        fit_params$A_o + fit_params$Amp * exp(
          -(
            (((X_values[i] -
                 fit_params$X_peak)*cos(fit_params$theta) -
                (Y_values[i] -
                   fit_params$Y_peak)*sin(fit_params$theta))/fit_params$a)^2 +
              (((X_values[i] -
                   fit_params$X_peak)*sin(fit_params$theta) -
                  (Y_values[i] -
                     fit_params$Y_peak)*cos(fit_params$theta))/fit_params$b)^2
          ) / 2
        )
    }
  }

  ## Elliptical_log, unconstrained ##
  if (attr(fit_params, "fit_method") == "elliptical_log_unconstr") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        fit_params$Amp * exp(-((X_values[i] -
                       fit_params$X_peak) ^ 2) / (fit_params$X_sig ^ 2)) *
        exp(-(Y_values[i] -
                ((fit_params$Q + 1) * (X_values[i] - fit_params$X_peak) +
                   fit_params$Y_peak)) ^ 2 / (fit_params$Y_sig ^ 2))
    }
  }

  ## Elliptical_log, constrained ##
  if (attr(fit_params, "fit_method") == "elliptical_log_constr") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        fit_params$Amp * exp(-((X_values[i] -
                       fit_params$X_peak) ^ 2) / (fit_params$X_sig ^ 2)) *
        exp(-(Y_values[i] -
                ((fit_params$Q + 1) * (X_values[i] - fit_params$X_peak) +
                   fit_params$Y_peak)) ^ 2 / (fit_params$Y_sig ^ 2))
    }
  }

  ## Circular ##
  if (attr(fit_params, "fit_method") == "circular") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        fit_params$Amp * exp(
          -(
            (((X_values[i] - fit_params$X_peak)^2) / (2 * fit_params$X_sig^2) +
               ((Y_values[i] - fit_params$Y_peak)^2)/(2 * fit_params$Y_sig^2))
          )
        )
    }
  }

  ## Create a data.frame
  result <-
    data.frame(
      X_values = X_values,
      Y_values = Y_values,
      predicted_values = preds
    )

  ## Export
  return(result)
}
