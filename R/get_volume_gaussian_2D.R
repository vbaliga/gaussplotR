## Part of the gaussplotR package
## Last updated: 2020-11-27 VBB

########################## get_volume_gaussian_2D #############################

#' Compute volume under 2D gaussian
#'
#' @param X_sig numeric value of the x-axis spread (sigma)
#' @param Y_sig numeric value of the y-axis spread (sigma)
#'
#' @details Volume under the 2D gaussian is computed as:
#' \code{2 * pi * sqrt(abs(X_sig)) * sqrt(abs(Y_sig))}
#'
#' @return A numeric value indicating the computed volume
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

  if (!is.numeric(X_sig)) {
    stop("X_sig must be numeric")
  }

  if (!is.numeric(Y_sig)) {
    stop("Y_sig must be numeric")
  }

  ## Compute volume
  volume <- 2 * pi * sqrt(abs(X_sig)) * sqrt(abs(Y_sig))

  ## Export
  return(volume)
}
