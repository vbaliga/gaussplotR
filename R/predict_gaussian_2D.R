## Part of the gaussplotR package
## Last updated: 2020-12-02 VBB

############################# predict_gaussian_2D ##############################

#' Predict values from a fitted 2D gaussian
#'
#' @param fit_object Either the output of \code{gaussplotR::fit_gaussian_2D()}
#' or a list that contains coefficients and fit methods (see Details).
#' @param X_values vector of numeric values for the x-axis
#' @param Y_values vector of numeric values for the y-axis
#' @param ... Additional arguments
#'
#' @details
#' This function assumes Gaussian parameters have been fitted beforehand. No
#' fitting of parameters is done within this function; these can be
#' supplied via the object created by \code{gaussplotR::fit_gaussian_2D()}.
#'
#' If \code{fit_object} is not an object created by
#' \code{gaussplotR::fit_gaussian_2D()}, \code{predict_gaussian_2D()} attempts
#' to parse \code{fit_object} as a list of two items. The coefficients of the
#' fit must be supplied as a one-row, named data.frame within
#' \code{fit_object$coefs}, and details of the methods for fitting the Gaussian
#' must be contained as a character vector in \code{fit_object$fit_method}. This
#' character vector in \code{fit_object$fit_method} must be a named vector that
#' provides information about the method, amplitude constraint choice, and
#' orientation constraint choice, using the names \code{method},
#' \code{amplitude}, and \code{orientation}. \code{method} must be one of:
#' \code{"elliptical"}, \code{"elliptical_log"}, or \code{"circular"}.
#' \code{amplitude} and \code{orientation} must each be either
#' \code{"unconstrained"} or \code{"constrained"}. For example, \code{c(method =
#' "elliptical", amplitude = "unconstrained", orientation = "unconstrained")}.
#' One exception to this is when \code{method = "circular"}, in which case
#' \code{orientation} must be \code{NA}, e.g.: \code{c(method = "circular",
#' amplitude = "unconstrained", orientation = NA)}.
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

  #### argument checks ####

  ## checks if fit_object is not from fit_gaussian_2D()
  if (is.null(attr(fit_object, "gaussplotR"))) {

    if (is.null(fit_object$coefs)) {
      stop(
"Coefficients not found. Please ensure they are stored within $coefs"
      )
    }
    if (is.null(fit_object$fit_method)) {
      stop(
"Fit method not found. Please ensure this information is stored in $fit_method"
      )
    }

  }

  ## x and y values
  if (!is.numeric(X_values)) {
    stop("'X_values' must be numeric")
  }
  if (!is.numeric(Y_values)) {
    stop("'Y_values' must be numeric")
  }
  if (length(X_values) != length(Y_values)) {
    stop("Lengths of X_values and Y_values must match")
  }


  ## extract the fitted parameters and make a simple object
  coefz <- fit_object$coefs

  ## Create NULL vector to fill with predicted values
  preds <- NULL

  #### elliptical, unconstrained #####
  if (unname(fit_object$fit_method)[1] == "elliptical") {
    if (unname(fit_object$fit_method)[3] == "unconstrained") {

    ## check that coefs match what is needed
    if (!length(fit_object$coefs) == 7) {
      stop(
"The number of coefficients supplied does not match expectation"
      )
    }

    obj_coef_names <- names(fit_object$coefs)
    expected_names <-
      c("A_o", "Amp", "theta", "X_peak", "Y_peak", "a", "b")
    expected_names2 <-
      c("A_o ", "Amp ", "theta ", "X_peak ", "Y_peak ", "a ", "b ")
    if (!identical(obj_coef_names, expected_names)) {
      stop(
"The names of coefficients do not match expectations and/or coefficients are
provided in the wrong order. $coef should contain a data.frame with the
following column names: ", expected_names2
      )
    }

    ## generate the prediction
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
  }


  #### elliptical, constrained ####
  if (unname(fit_object$fit_method)[1] == "elliptical") {
    if (unname(fit_object$fit_method)[3] == "constrained") {

    ## check that coefs match what is needed
    if (!length(fit_object$coefs) == 7) {
      stop(
        "The number of coefficients supplied does not match expectation"
      )
    }

    obj_coef_names <- names(fit_object$coefs)
    expected_names <-
      c("A_o", "Amp", "theta", "X_peak", "Y_peak", "a", "b")
    expected_names2 <-
      c("A_o ", "Amp ", "theta ", "X_peak ", "Y_peak ", "a ", "b ")
    if (!identical(obj_coef_names, expected_names)) {
      stop(
"The names of coefficients do not match expectations and/or coefficients are
provided in the wrong order. $coef should contain a data.frame with the
following column names: ", expected_names2
      )
    }

    ## generate the prediction
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
  }


  #### elliptical_log, unconstrained ####
  if (unname(fit_object$fit_method)[1] == "elliptical_log") {
    if (unname(fit_object$fit_method)[3] == "unconstrained") {

    ## check that coefs match what is needed
    if (!length(fit_object$coefs) == 6) {
      stop(
        "The number of coefficients supplied does not match expectation"
      )
    }

    obj_coef_names <- names(fit_object$coefs)
    expected_names <-
      c("Amp", "Q", "X_peak", "Y_peak", "X_sig", "Y_sig")
    expected_names2 <-
      c("Amp ", "Q ", "X_peak ", "Y_peak ", "X_sig ", "Y_sig ")
    if (!identical(obj_coef_names, expected_names)) {
      stop(
"The names of coefficients do not match expectations and/or coefficients are
provided in the wrong order. $coef should contain a data.frame with the
following column names: ", expected_names2
      )
    }

    ## generate the prediction
    for (i in seq_along(X_values)) {
      preds[i] <-
        coefz$Amp * exp(-((X_values[i] -
                             coefz$X_peak) ^ 2) / (coefz$X_sig ^ 2)) *
        exp(-(Y_values[i] -
                ((coefz$Q + 1) * (X_values[i] - coefz$X_peak) +
                   coefz$Y_peak)) ^ 2 / (coefz$Y_sig ^ 2))
    }
  }
  }


  #### elliptical_log, constrained ####
  if (unname(fit_object$fit_method)[1] == "elliptical_log") {
    if (unname(fit_object$fit_method)[3] == "constrained") {

    ## check that coefs match what is needed
    if (!length(fit_object$coefs) == 6) {
      stop(
        "The number of coefficients supplied does not match expectation"
      )
    }

    obj_coef_names <- names(fit_object$coefs)
    expected_names <-
      c("Amp", "Q", "X_peak", "Y_peak", "X_sig", "Y_sig")
    expected_names2 <-
      c("Amp ", "Q ", "X_peak ", "Y_peak ", "X_sig ", "Y_sig ")
    if (!identical(obj_coef_names, expected_names)) {
      stop(
"The names of coefficients do not match expectations and/or coefficients are
provided in the wrong order. $coef should contain a data.frame with the
following column names: ", expected_names2
      )
    }

    ## generate the prediction
    for (i in seq_along(X_values)) {
      preds[i] <-
        coefz$Amp * exp(-((X_values[i] -
                             coefz$X_peak) ^ 2) / (coefz$X_sig ^ 2)) *
        exp(-(Y_values[i] -
                ((coefz$Q + 1) * (X_values[i] - coefz$X_peak) +
                   coefz$Y_peak)) ^ 2 / (coefz$Y_sig ^ 2))
    }
    }
  }


  #### circular ####
  if (unname(fit_object$fit_method)[1] == "circular") {

    ## check that coefs match what is needed
    if (!length(fit_object$coefs) == 5) {
      stop(
        "The number of coefficients supplied does not match expectation"
      )
    }

    obj_coef_names <- names(fit_object$coefs)
    expected_names <-
      c("Amp", "X_peak", "Y_peak", "X_sig", "Y_sig")
    expected_names2 <-
      c("Amp ", "X_peak ", "Y_peak ", "X_sig ", "Y_sig ")
    if (!identical(obj_coef_names, expected_names)) {
      stop(
"The names of coefficients do not match expectations and/or coefficients are
provided in the wrong order. $coef should contain a data.frame with the
following column names: ", expected_names2
      )
    }

    ## generate the prediction
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
