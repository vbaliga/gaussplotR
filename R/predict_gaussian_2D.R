## Part of the gaussplotR package
## Last updated: 2020-09-19 VBB

############################# predict_gaussian_2D ##############################

#' Predict values from a fitted 2D gaussian
#'
#' @param X_values vector of numeric values for the x-axis
#' @param Y_values vector of numeric values for the y-axis
#' @param A numeric value of the amplitude
#' @param X_peak numeric value of the x-location of the peak of the gaussian
#' @param X_var numeric value of the x-axis variance
#' @param Q numeric value for the orientation parameter
#' @param Y_peak numeric value of the y-location of the peak of the gaussian
#' @param Y_var numeric value of the y-axis variance
#'
#' @section Warning:
#' This function assumes raw data have been log2 transformed
#'
#' @return A numeric vector that provides the values of the 2D gaussian,
#' corresponding to the set of {\code{X_values},\code{Y_values}}
#'
#' @author Vikram B. Baliga
#'
#' @export

predict_gaussian_2D <- function(X_values,
                                Y_values,
                                A,
                                X_peak,
                                X_var,
                                Q,
                                Y_peak,
                                Y_var){
  result <- NULL
  for (i in 1:length(X_values)){
    result[i] <-
      A * exp(-((X_values[i] - log2(X_peak)) ^ 2) / (X_var ^ 2)) *
      exp(-(Y_values[i] - log2(2 ^ ((Q + 1) * (X_values[i] - log2(X_peak)) +
                                      log2(Y_peak)
      ))) ^ 2 / (Y_var ^ 2))
  }
  return(result)
}
