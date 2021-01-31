---
title: 'gaussplotR: Fit, Predict and Plot 2D Gaussians in R'
tags:
  - R
  - 2D Gaussian
  - Gaussian fit
  - Gaussian orientation
  - nonlinear least squares
authors:
  - name: Vikram B. Baliga
    orcid: 0000-0002-9367-8974
    affiliation: 1
affiliations:
 - name: Department of Zoology, University of British Columbia, 
 Vancouver, British Colombia, Canada V6T 1Z4
   index: 1
date: 31 January 2021
bibliography: paper.bib

---

# Summary

`gaussplotR` is an R package that is designed to fit 2D Gaussian equations to
data, characterize fitted models, predict new data from fitted models, and
provide visualizations of fitted 2D Gaussian surfaces. Such tools enable
researchers to fit models to data that characterize a response (i.e. the height
of the Gaussian) for a given set of bivariate inputs (i.e. a set of "X" and "Y"
variables). In principle, tools within `gaussplotR` have broad applicability to
a variety of scientific disciplines.


# Overview and getting started
A series of vignettes that provides detailed guidance are available on
[gaussplotR's GitHub page](https://vbaliga.github.io/gaussplotR/).

The function `fit_gaussian_2D()` is the workhorse of `gaussplotR`. It uses
`stats::nls()` to find the best-fitting parameters of a 2D Gaussian fit to
supplied data based on one of three formula choices. Each of these formula
choices is designed for a specific use case. The most generic method (and the
default) is `method = "elliptical"`. This allows the fitted 2D Gaussian to take
an ellipsoid shape, and this will likely be the best option for most use cases.
A slightly-altered method to fit an ellipsoid Gaussian is available in 
`method = "elliptical_log"`. This method follows `@Priebe2003` and is geared 
towards use with log2-transformed data. A third option is `method = "circular"`.
This produces a very simple 2D Gaussian that is constrained to have to have a
roughly circular shape (i.e. spread in X- and Y- are roughly equal). Rather than
place the burden on the user to determine formula choice, the function
`autofit_gaussian_2D()` can be used to automatically figure out the best formula
choice and arrive at the best-fitting parameters.

In some cases, the researcher may be interested in characterizing the orientation
of the fitted Gaussian and comparing it to theoretical predictions. For example,
studies of visual neuroscience often describe the properties of individual
motion-sensitive neurons based on whether they are "speed-tuned" or whether they
show independence from the speed of visual stimuli. Assessing such properties
can be done via fitting a 2D Gaussian to the response rate of a neuron for a
grid of investigated spatial (X-axis) and temporal frequencies (Y-axis). Should
the orientation of the fitted Gaussian lie along the diagonal of the plot, the
neuron can be classified as "speed-tuned". The function
`characterize_gaussian_fits()` allows for such analysis within `gaussplotR`.
Following methods used in studies of visual neuroscience 
[@Levitt1994; @Priebe2003; @Winship2006], the orientation and partial 
correlations of Gaussian data are analyzed. Features include computation of
partial correlations between response variables and independent and
diagonally-tuned predictions, along with Z-difference scoring.

The `predict_gaussian_2D()` function can be used to predict values from the
fitted Gaussian over a supplied grid of X- and Y-values (usually generated via
`expand.grid()`). This is useful if the original data are relatively sparse and
interpolation of values is desired, e.g. to attain smoother-looking contours in
plots.

Plotting can then be achieved via `ggplot_gaussian_2D()`, but note that the 
`data.frame` created by `predict_gaussian_2D()` can be supplied to other 
plotting frameworks such as `lattice::levelplot()`. A 3D plot can also be 
produced via `rgl_gaussian_2D()`.

`gaussplotR` was designed for broad applicability; there are many disciplines
in which a 2D Gaussian surface would be a useful model for describing a response
to a bivariate set of inputs. Functions in `gaussplotR` are being used in an in-
prep article on spatiotemporal tuning of motion-sensitive neurons in the avian
brain.


# Acknowledgements

We thank Douglas R. Wylie, Douglas L. Altshuler, and Graham Smyth for help in 
working with 2D Gaussian data.

# References
