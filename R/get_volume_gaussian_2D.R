## Part of the gaussplotR package
## Last updated: 2020-11-30 VBB

########################## get_volume_gaussian_2D #############################

#' Compute volume under 2D-Gaussian
#'
#' @param X_sig numeric value(s) of the x-axis spread (sigma)
#' @param Y_sig numeric value(s) of the y-axis spread (sigma)
#'
#' @details Volume under the 2D-Gaussian is computed as:
#' \code{2 * pi * sqrt(abs(X_sig)) * sqrt(abs(Y_sig))}
#'
#' Numeric vectors can be supplied to \code{X_sig} and \code{Y_sig}. If vectors
#' of length greater than 1 are given, the function computes volume for each
#' sequential pair of \code{X_sig}, \code{Y_sig} values. The lengths of these
#' supplied vectors must be identical.
#'
#' @return Numeric value(s) indicating the computed volume(s)
#'
#' @export
#'
#' @author Vikram B. Baliga
#'
#' @examples
#' library(gaussplotR)
#'
#' get_volume_gaussian_2D(5, 3) #24.33467

get_volume_gaussian_2D <- function(X_sig,
                                   Y_sig) {

  ## Argument checks
  if (!is.numeric(X_sig)) {
    stop("X_sig must be numeric")
  }

  if (!is.numeric(Y_sig)) {
    stop("Y_sig must be numeric")
  }

  if (!length(X_sig) == length(Y_sig)) {
    stop("X_sig and Y_sig should be numeric vectors of equal length")
  }

  ## Compute volume
  volume <-
    2 * pi * sqrt(abs(X_sig)) * sqrt(abs(Y_sig))

  ## Export
  return(volume)
}
