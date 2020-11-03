## Part of the gaussplotR package
## Last updated: 2020-11-02 VBB

########################## compute_gaussian_volume #############################

#' Compute volume under 2D gaussian
#'
#' @param X_var numeric value of the x-axis variance
#' @param Y_var numeric value of the y-axis variance
#'
#' @details Volume under the 2D gaussian is computed as:
#' \code{2 * pi * sqrt(abs(X_var)) * sqrt(abs(Y_var))}
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
#' compute_gaussian_volume(5, 3) #24.33467

compute_gaussian_volume <- function(X_var,
                                    Y_var) {

  if (!is.numeric(X_var)) {
    stop("X_var must be numeric")
  }

  if (!is.numeric(Y_var)) {
    stop("Y_var must be numeric")
  }

  ## Compute volume
  volume <- 2 * pi * sqrt(abs(X_var)) * sqrt(abs(Y_var))

  ## Export
  return(volume)
}
