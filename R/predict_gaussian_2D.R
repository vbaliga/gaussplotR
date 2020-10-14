## Part of the gaussplotR package
## Last updated: 2020-10-12 VBB

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
#' This function assumes Gaussian parameters have been fitted beforehand. No
#' fitting of parameters is done within this function; they all must be
#' supplied.
#'
#' @return A data.frame with the supplied \code{X_values} and \code{Y_values}
#'   along with the predicted values of the 2D gaussian
#'   (\code{predicted_values})
#'
#' @author Vikram B. Baliga
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ## Load the sample data set
#'   data(gaussplot_sample_data)
#'
#'   ## Generate a grid of x- and y- values on which to predict
#'   grid <-
#'     expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
#'                 Y_values = seq(from = -1, to = 4, by = 0.1))
#'
#'   ## Predict the values using predict_gaussian_2D
#'   gauss_data <-
#'     predict_gaussian_2D(
#'       X_values = grid$X_values,
#'       Y_values = grid$Y_values,
#'       A = gaussplot_sample_data$A[1],
#'       X_peak = gaussplot_sample_data$X_peak[1],
#'       Y_peak = gaussplot_sample_data$Y_peak[1],
#'       Q = gaussplot_sample_data$Q[1],
#'       X_var = gaussplot_sample_data$X_var[1],
#'       Y_var = gaussplot_sample_data$Y_var[1]
#'     )
#'
#'   ## Plot via ggplot2 and metR
#'   library(ggplot2); library(metR)
#'   ggplot_gaussian_2D(gauss_data)
#'  }


predict_gaussian_2D <- function(X_values,
                                Y_values,
                                A,
                                X_peak,
                                X_var,
                                Q,
                                Y_peak,
                                Y_var) {
  preds <- NULL
  for (i in seq_along(X_values)){
    preds[i] <-
      A * exp(-((X_values[i] - X_peak) ^ 2) / (X_var ^ 2)) *
      exp(-(Y_values[i] - ((Q + 1) * (X_values[i] - X_peak) +
                             Y_peak
      )) ^ 2 / (Y_var ^ 2))
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
