## Part of the gaussplotR package
## Last updated: 2020-12-02 VBB

############################# ggplot_gaussian_2D ###############################

#' Plot a 2D Gaussian via ggplot
#'
#' @param gauss_data Data.frame with X_values, Y_values, and predicted_values,
#'   e.g. exported from \code{predict_gaussian_2D()}
#' @param normalize Default TRUE, should predicted_values be normalized on a 0
#'   to 1 scale?
#' @param contour_thickness Thickness of contour lines
#' @param contour_color Color of the contour lines
#' @param bins Number of bins for the contour plot
#' @param viridis_dir See "direction" in scale_fill_viridis_c()
#' @param viridis_opt See "option" in scale_fill_viridis_c()
#' @param x_lab Arguments passed to xlab()
#' @param y_lab Arguments passed to ylab()
#' @param axis.text Arguments passed to axis.text
#' @param axis.title Arguments passed to axis.title
#' @param axis.ticks Arguments passed to axis.ticks
#' @param plot.margin Arguments passed to plot.margin
#' @param ... Other arguments supplied to \code{ggplot2::theme()}
#'
#' @return A ggplot object that uses metR::geom_contour_fill() to display the
#' 2D Gaussian
#'
#' @author Vikram B. Baliga
#'
#' @export
#'
#' @inherit fit_gaussian_2D examples

ggplot_gaussian_2D <- function(gauss_data,
                               normalize = TRUE,
                               contour_thickness = 0.04,
                               contour_color = "black",
                               bins = 15,
                               viridis_dir = 1,
                               viridis_opt = "B",
                               x_lab = "X values",
                               y_lab = "Y values",
                               axis.text = element_text(size = 6),
                               axis.title = element_text(size = 7),
                               axis.ticks = element_line(size = 0.3),
                               plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
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

  ## ggplot arguments
  if (!is.numeric(contour_thickness)) {
    stop("contour_thickness must be a numeric")
  }
  if (!is.numeric(bins)) {
    stop("bins must be a numeric")
  }
  if (!is.numeric(viridis_dir)) {
    stop("viridis_dir must be 1 or -1")
  }

  if (normalize == TRUE){
    gauss_data$predicted_values <-
      gauss_data$predicted_values/max(gauss_data$predicted_values)
  }

  ggplot2::ggplot(gauss_data, aes(X_values, Y_values, z = predicted_values)) +
    metR::geom_contour_fill(#aes(fill = ..level..),
                            size = contour_thickness,
                            color = contour_color,
                            bins = bins) +
    scale_fill_viridis_c(direction = viridis_dir,
                         option = viridis_opt) +
    coord_fixed(
      xlim = range(gauss_data$X_values),
      ylim = range(gauss_data$Y_values),
      expand = FALSE
    ) +
    xlab(x_lab) +
    ylab(y_lab) +
    theme_classic() +
    theme(
      axis.text = axis.text,
      axis.title = axis.title,
      axis.ticks = axis.ticks,
      plot.margin = plot.margin,
      ...
    )
}
