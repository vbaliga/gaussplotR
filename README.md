
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gaussplotR

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build
status](https://github.com/vbaliga/gaussplotR/workflows/R-CMD-check/badge.svg)](https://github.com/vbaliga/gaussplotR/actions)
<!-- badges: end -->

`gaussplotR` provides functions to predict values from a two-dimensional
Gaussian function and produce plots of predicted data.

## Installation

You can install `gaussplotR` via:

``` r
# install.packages("devtools")
devtools::install_github("vbaliga/gaussplotR")
```

## Example

At this time, parameters of the Gaussian will need to be known in
advance. The function `predict_gaussian_2D()` can then be used to
predict values from the gaussian over a supplied grid of x- and y-values
(generated here via `expand.grid`).

``` r
library(gaussplotR)

## Load the sample data set
data(gaussplot_sample_data)

## Generate a grid of x- and y- values on which to predict
grid <-
  expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
              Y_values = seq(from = -1, to = 4, by = 0.1))

## Predict the values using predict_gaussian_2D
gauss_data <-
  predict_gaussian_2D(
    X_values = grid$X_values,
    Y_values = grid$Y_values,
    A = gaussplot_sample_data$A[1],
    X_peak = gaussplot_sample_data$X_peak[1],
    Y_peak = gaussplot_sample_data$Y_peak[1],
    Q = gaussplot_sample_data$Q[1],
    X_var = gaussplot_sample_data$X_var[1],
    Y_var = gaussplot_sample_data$Y_var[1]
  )

## Generate a plot
## Here's a simpler example via lattice::levelplot()
library(lattice)
levelplot(
  predicted_values ~ X_values * Y_values,
  data = gauss_data,
  col.regions = colorRampPalette(
    c("white", "blue")
    )(100),
  asp = 1
)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

## And here's an extended example using ggplot2
## metR::geom_contour_fill() is used to draw the contours
library(ggplot2); library(metR)
ggplot_gaussian_2D(gauss_data)
```

<img src="man/figures/README-example-2.png" width="100%" />

## Citation

TBD

## License

GPL (\>= 3) + file LICENSE

🐢
