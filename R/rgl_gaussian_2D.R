## Part of the gaussplotR package
## Last updated: 2020-12-26 VBB

############################### rgl_gaussian_2D ################################

#' Produce a 3D plot of the 2D-Gaussian via rgl
#'
#' @param gauss_data Data.frame with X_values, Y_values, and predicted_values,
#'   e.g. exported from \code{predict_gaussian_2D()}
#' @param normalize Default TRUE, should predicted_values be normalized on a 0
#'   to 1 scale?
#' @param viridis_dir See "direction" in scale_fill_viridis_c()
#' @param viridis_opt See "option" in scale_fill_viridis_c()
#' @param x_lab Arguments passed to xlab()
#' @param y_lab Arguments passed to ylab()
#' @param box Whether to draw a box; see \code{rgl::plot3d()}
#' @param aspect Whether to adjust the aspect ratio; see \code{rgl::plot3d()}
#' @param ... Other arguments supplied to \code{rgl::plot3d()}
#'
#' @return An rgl object (i.e. of the class 'rglHighlevel'). See
#'   \code{rgl::plot3d()} for details.
#'
#' @author Vikram B. Baliga
#'
#' @export
#'
#' @inherit fit_gaussian_2D examples

rgl_gaussian_2D <- function(gauss_data,
                            normalize = TRUE,
                            viridis_dir = 1,
                            viridis_opt = "B",
                            x_lab = "X values",
                            y_lab = "Y values",
                            box = FALSE,
                            aspect = TRUE,
                            ...
) {

  ## data names
  if (!any(names(gauss_data) == "X_values")) {
    stop("X_values column not found")
  }
  if (!any(names(gauss_data) == "Y_values")) {
    stop("Y_values column not found")
  }
  if (!any(names(gauss_data) == "predicted_values")) {
    stop("predicted_values column not found")
  }

  ## aesthetic arguments
  if (!is.numeric(viridis_dir)) {
    stop("viridis_dir must be 1 or -1")
  }

  if (normalize == TRUE){
    gauss_data$predicted_values <-
      gauss_data$predicted_values/max(gauss_data$predicted_values)
  }

  zval <- gauss_data$predicted_values
  all_cols <-
    viridisLite::viridis(
      n = length(unique(zval)),
      direction = viridis_dir,
      option = viridis_opt
    )
  zval_sort <-
    sort(zval)
  names(all_cols) <-
    unique(zval_sort)
  all_cols_sort <-
    all_cols[order(match(names(all_cols), zval))]

  rgl::plot3d(
    gauss_data,
    col = all_cols_sort,
    xlab = x_lab,
    ylab = y_lab,
    box = box,
    aspect = aspect,
    ...)

}
