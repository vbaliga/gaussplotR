## Part of the gaussplotR package
## Last updated: 2020-12-02 VBB

############################# predict_gaussian_2D ##############################

#' Predict values from a fitted 2D gaussian
#'
#' @param fit_object The output of \code{gaussplotR::fit_gaussian_2D()}.
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



predict_gaussian_2D <- function(fit_object,
                                X_values,
                                Y_values,
                                ...) {

  #### Argument checks ####

  ## fit_object
  if (is.null(attr(fit_object, "gaussplotR"))) {
    stop(
"This does not seem to be an object output from fit_gaussian_2D().
Please run fit_gaussian_2D() on your data prior to using this function."
)
  }
  if (!attr(fit_object, "gaussplotR") == "gaussplotR_fit") {
    stop(
"This does not seem to be an object output from fit_gaussian_2D().
Please run fit_gaussian_2D() on your data prior to using this function."
)
  }

  ## x and y values
  if (!is.numeric(X_values)) {
    stop("'X_values' must be numeric")
  }
  if (!is.numeric(Y_values)) {
    stop("'Y_values' must be numeric")
  }


  ## extract the fitted parameters and make a simple object
  coefz <- fit_object$coefs

  ## Create NULL vector to fill with predicted values
  preds <- NULL

  ## Elliptical, unconstrained ##
  if (fit_object$fit_method == "elliptical_unconstr") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        coefz$A_o + coefz$Amp * exp(
          -(
            (((X_values[i] -
                 coefz$X_peak)*cos(coefz$theta) -
                (Y_values[i] -
                   coefz$Y_peak)*sin(coefz$theta))/coefz$a)^2 +
              (((X_values[i] -
                   coefz$X_peak)*sin(coefz$theta) -
                  (Y_values[i] -
                     coefz$Y_peak)*cos(coefz$theta))/coefz$b)^2
          ) / 2
        )
    }
  }

  ## Elliptical, constrained ##
  if (fit_object$fit_method == "elliptical_constr") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        coefz$A_o + coefz$Amp * exp(
          -(
            (((X_values[i] -
                 coefz$X_peak)*cos(coefz$theta) -
                (Y_values[i] -
                   coefz$Y_peak)*sin(coefz$theta))/coefz$a)^2 +
              (((X_values[i] -
                   coefz$X_peak)*sin(coefz$theta) -
                  (Y_values[i] -
                     coefz$Y_peak)*cos(coefz$theta))/coefz$b)^2
          ) / 2
        )
    }
  }

  ## Elliptical_log, unconstrained ##
  if (fit_object$fit_method == "elliptical_log_unconstr") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        coefz$Amp * exp(-((X_values[i] -
                             coefz$X_peak) ^ 2) / (coefz$X_sig ^ 2)) *
        exp(-(Y_values[i] -
                ((coefz$Q + 1) * (X_values[i] - coefz$X_peak) +
                   coefz$Y_peak)) ^ 2 / (coefz$Y_sig ^ 2))
    }
  }

  ## Elliptical_log, constrained ##
  if (fit_object$fit_method == "elliptical_log_constr") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        coefz$Amp * exp(-((X_values[i] -
                             coefz$X_peak) ^ 2) / (coefz$X_sig ^ 2)) *
        exp(-(Y_values[i] -
                ((coefz$Q + 1) * (X_values[i] - coefz$X_peak) +
                   coefz$Y_peak)) ^ 2 / (coefz$Y_sig ^ 2))
    }
  }

  ## Circular ##
  if (fit_object$fit_method == "circular") {
    for (i in seq_along(X_values)) {
      preds[i] <-
        coefz$Amp * exp(
          -(
            (((X_values[i] - coefz$X_peak)^2) / (2 * coefz$X_sig^2) +
               ((Y_values[i] - coefz$Y_peak)^2)/(2 * coefz$Y_sig^2))
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
