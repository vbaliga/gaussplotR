## Part of the gaussplotR package
## Last updated: 2021-03-18 VBB

############################ print.gaussplotR_fit() ############################
# Print method

#' @noRd
print_gaussplotR_fit <- function(x, ...) {
  #NextMethod()
  message("Model coefficients")
  print(round(x$coefs, 2), row.names = FALSE)
  message("Model error stats")
  print(round(x$model_error_stats, 2), row.names = FALSE)
  message("Fitting methods")
  print(x$fit_method)
}

#' @method print gaussplotR_fit
#' @export
print.gaussplotR_fit <- function(x, ...) {
  print_gaussplotR_fit(x)
}


# Summary method
#' @method summary gaussplotR_fit
#' @export
summary.gaussplotR_fit <- function(object, ...) {
  #NextMethod()
  message("Model coefficients")
  print(round(object$coefs, 2), row.names = FALSE)
  message("Model error stats")
  print(round(object$model_error_stats, 2), row.names = FALSE)
  message("Fitting methods")
  print(object$fit_method)
}
