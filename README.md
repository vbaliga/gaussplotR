
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gaussplotR

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build
status](https://github.com/vbaliga/gaussplotR/workflows/R-CMD-check/badge.svg)](https://github.com/vbaliga/gaussplotR/actions)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4041073.svg)](https://doi.org/10.5281/zenodo.4041073)
[![DOI](https://img.shields.io/badge/10.6084/m9.figshare.12980717-blue.svg?style=flat&labelColor=gainsboro&logoWidth=40&logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAAFAAAAAZCAYAAACmRqkJAAAKi0lEQVR4Ae3ZaVBUV97H8evuE0EfH32MmkcfoyAuGjXKgkvMaFRAFuiloemWvRuEXlgEBREXBYJiXAQUFeKocUniQiKogAJhQWwWENDEjLNYvjFLzUzNkplEZb5kTme6nCRjKlOpSZlb9SmL2%2Ffcuv3re87%2FnKP0TYfOcslqPMbt63xBKuh09MTxgi7HKT1Sj1TvKp%2BMkZB6%2FXT8c4AjUYPyVdfb7Qs6HTIJ8EHe7Ul%2B152CphDabRQ0uMr7%2FRQgh%2B8qU6%2FBiPDVGv0jq0uGE94b0ZZ3j%2B25MTetoMsh%2FWD91OBqT9%2Fsehd5EqGV17nKMzTqOHvaRMMLEp7qACfinq%2FW1BBx5ZxB13x5X3Jr1v%2Fz9pUcaHU63PiicjrhvXfNRbY1Th49Q6Y1vu6zyqSjzX3aVIgf4OkKToxhgxpd5OMzV0bYE4CRN1Chu34pnTfwnV03FiTlfzDRXBHo6dfgIq8sX6ByV6vjthGc0UdrrPPVGFQBxlSjzJQWENVUZkebceiLpyM8IZSx7O7Zl4JivUNMZX5h8Rt4%2B2L0llKfgu6JKa%2BXvpB5bZ48%2Ba3F6lil2pDkE2rODzCsU0VUnNFHNZQqdS3lx3Utl%2FMILQcfYt5TEeC1GSprgAq0XlgYGLQyxJTlr0uK0DVX7E5s2ZtOgHvLw5fLK9xVmcqguEj%2F2LXbwsvPBkZZKl4j5NcIKinaUsLbejFWZ7m8Do2cmwnb4cFqArRwx3TEYzi%2Bz7DTD0uhxnj8cAEWWUZK%2BTcdhh4pmTWUsW01Y1uCUmNY7Rtqzo5svJSS0poVXtg6yVj7sn9qunek3j8xPVXXeMFoaDkev6lDF7ene7Y5r2taNAXmEBXaP69zevaOjuUeeZ0zhzJuPsM5CdYvOhZVqBMhBqIVDt8zwGdQjR4of9AA%2BXJjUFpww7GodnHAQca4srDAWCXjW3pETal%2BbfumuOLKqSm17vIQtWr1Uu3JYy6JbXuXFbRN1R8pm5byxtG5CcdOz9EUVc7I5IeQEWQ7wWVwzwrsRn%2BbAFeiCxNsKv5Y9P03BFgjAlT90AGOQy2T47fObl00ocFZHl%2B2UGXw0RjzNUWHTPFthckHWh18al8KsGuaFigVVzlKuY%2BG9z37qvuoGlelpsJVldrgrFjbOE%2BeWe8uW18W84qCqc4s7tmCIgzI75hs%2FaJKNFu7rF%2BIIIhr%2BmIQ%2Btn8LQkDMQOeWAYnDHgsQI3NNU7W9j4h5t72o%2FEyvLEQ%2F%2Bu7ymzbOxbCAeOxAgtghz6YgOVYiufEOUlqu0M37ho%2BYn%2FnpJT8bsejVSt90uqdFdlGmV7hF7cuWXetNCShLX%2BI3nKhN%2ByvCs%2Bs6GQpWB33fzKNQR%2BqWr022yvc94q7spBCY%2Bbzkou6ZfJNPf89ZN%2FdidYHnIsKfIzjCMIc7MAwSJiMPFxGMcKQixGwx07R%2FiEe4CNsxFCbAJvwifj8LkIgYRHa8Lm47jNY8AokmMS5NryPh%2FijOB%2BOX4h7foEuyPHlisMtylJpzu1YspkQ36YbLqnx8F1X4abaqmYs9DGmLlrk4CE9XlHlKZskxfpt%2FUJLzyhV23dG%2BITF72fqo9njEaokwIu8lSbG1N4wx273CrP%2B%2BjniQVZhGrzQjlEioFIRcjDM6MIdjBVtHogvl4W9qIX8sTfwU5SgU%2FzdhdGYLcJ9BzvRID6vgx2SxN8PUI9KnIEWH4n7FuIo%2FoRfYV5vMMV4wHRFs%2BvG%2FKl05ZrDVdP11T7eulK3oNQcz%2FAXcj3DpMePjO44KetDL2lDh%2FmV1S3nNoeWnJb7RSXmMJl%2BI0GmH13rKs8lvEdQwfoWKmCxdmGbAEdgAW5jFiQhBb8WXSYTPSjGCBHaMPR5LMANkOCM%2B%2FgD3MS5Z8W1ElzwW3HNJCSI9tcw2ub%2BO8T5LPTBQBy1nusNcB7ztximI1sIsSSzXb04v3vyusJmx63nMufHXlV6LvpEShDd9x%2FHFYWXVPuSX7%2FD7zmpcjuWRupbyvaHnj8Z7BNsUFCArm70iTRcd5bFEN7oxwJs%2FpoA%2FwfBaLJ2Z2EFbmEsNKL7fYYPUI9DIqj%2Fsgkw0CasW%2BL6RbBDFI7gTZSKzz6Gk02AJ23G3QF4xybYU8INce6s5CJNlTyXhYwKv%2FRWMiEeimquzIhrPpGzuSNCsbvLec2%2Brpmh2e0yu%2FxOp96wv6p8X0xeIZW5Bo2%2F6ucdvb%2FdMWVDm8lX11pRpD16OJ6VyZsrQ8yK%2BVFJ9h4UhwEHDj5JgGE23UkSfoZujMMzSESNCPBT9KAFjqi2rcIYZRPgYmzDQ9xDLSz4%2FGsCPIE%2BNkWrTJy%2FhRrRthpVyJJExbnmG2I%2B6x%2BT%2FHxYyQkzQfJGlufpWy6bYlvPUEgu%2BHlHJA5boo7rE3blnBR7r6mv%2BvCBMYEag%2Faqsyr1%2BIk5a%2Fd2z9zGBDpZ31qulCWk9443Hfg5BuJJAgxAG0ZBEmS4DZ7RKIliMVi0d8UvRUCeuPoNAf4Z%2FmgV13pAwiwR3iffFKBQJM5noB%2F6Y5h45v7Wwf0cDtD1DlMIeiugWmZOy5Cv3RgjX7%2FF4GdMXasOjgurmqdafqpojltml9IjvOJ8NMu9lNL5gQmXdMu0BTefz8loMyoJvivs3VMZvhpjqaig%2FZ8gwJGYIsIKRh%2FY4wh%2Bg%2FGQoxYbREgZ%2BB3uww1V3xKgN%2BrwCNtF4Pvx8NveQCEYX%2BAukhCIYuHZLy%2FyDjHbJQfo7PTK1dEBWqPBX2vS%2B2hNW1XquDURypiwXStCjVWuyrSKQC%2FdoUaHtOT2HENoyal4b40x7rK7ylip9NIV3Jy0P6fD24fl3Ra6uoe3PNqOH2Pw3x%2FC8K8CHIU%2BIpQ7OI8yNOJ9TMJO%2FAU9Nn6PjRiGmm%2FpwgsRLQpKjwjuU%2Fz1CQK0R4G4T4%2FwCHWYKlmcA6xr4SA2EzobXeUa9vh21LgpdKxK8hqd5RsaXWS7S9YvlhU2O7ya3ekXrm%2B9lK3KzFH6a4y5V92Ve5hkM4d02EShMestZekE2IxZX7MWdkAgBtmsi9U2lXEwliAOK%2BGLTowThWIZkrEVSSKYgegPOUxwtFmdaBGLsRgg2qeKtosQDh2GYzbisUIEaPvcQ8T5VGzCKowBk2I3mTVALe4wd4tumKcoaZirSKte4RtVrvXwLrw%2BJXV%2F18Ts3BtLEmOaS0yRtRdMfpGJhTKNMbDJWR5V7eEbUNDtcIQAd1PJMwnuJl6E9KQHY7AAHkzQoBkj8B%2B%2FpTWQ4Maezne1P3x1esLBuqmB%2BbccNhJMGetbM%2BGZIi1V%2FoRyOXB77sKVWuPmrd4RBvYQm9ihVue%2F7xDPGljB50MoJmO%2By36gCGsQovCyCGwOarD9R7PLLXZOJjKZvse%2FDQQSvffG7F1rWrZPiLKUX2DPr1hbfHAKb0kDBSeTed5MQj94Pn1xBMvA%2B2IDYTAkcXzXANPRjHq04ACeFeH9aAIcBC3LOq%2FY5pPDeYtO4yRTmzUhbx9LozCEea8ybaHoxDNmVtPltxSVzxhCm3Asg4Tvs683Aa5wwkD8qP9XbgQqUbb6Tp09U5Os3rWiV4jZv2OuvxPdvht70RfST8fjATZd7P33OYzxZ%2FdF7FwcgqPU0yMR2vMYDulpDfBvw%2BGCdBePpq8AAAAASUVORK5CYII%3D)](http://dx.doi.org/10.6084/m9.figshare.12980717)
[![CRAN
status](https://www.r-pkg.org/badges/version/gaussplotR)](https://CRAN.R-project.org/package=gaussplotR)
<!-- badges: end -->

`gaussplotR` provides functions to fit two-dimensional Gaussian
functions, predict values from such functions, and produce plots of
predicted data.

## Installation

You can install `gaussplotR` via:

`install.packages("gaussplotR")`

Or to get the latest (developmental) version through github, use:

`devtools::install_github("vbaliga/gaussplotR")`

## Example

The function `fit_gaussian_2D()` uses `stats::nls()` to find the
best-fit parameters of a 2D Gaussian fit to supplied data.

The `predict_gaussian_2D()` function can then be used to predict values
from the Gaussian over a supplied grid of x- and y-values (generated
here via `expand.grid`). This is useful if the original data is
relatively sparse and interpolation of values is desired.

Plotting can then be achieved via `ggplot_gaussian_2D()`, but note that
the `data.frame` created by `predict_gaussian_2D()` can be supplied to
other plotting frameworks such as `lattice::levelplot()`.

``` r
library(gaussplotR)

## Load the sample data set
data(gaussplot_sample_data)

## The relevant data are in the first three columns
samp_dat <- gaussplot_sample_data[,1:3]
head(samp_dat)
#>    X_values  Y_values   response
#> 1 -4.965784 -4.965784  1.4901502
#> 2 -4.965784 -3.000000  1.6190163
#> 3 -4.965784 -1.000000  1.4880618
#> 4 -4.965784  1.000000  4.8455212
#> 5 -4.965784  3.000000 -0.7443408
#> 6 -4.965784  4.000000 -0.4960800

## Fit a 2D Gaussian with no user-supplied constraints; this
## will generally lead to the best fit
params_unconstrained <-
  fit_gaussian_2D(samp_dat)

## If desired, the user can specify contraints. E.g. force the
## gaussian to be horizontally oriented.
## Note that this will generally lead to poor fits and should
## be performed with caution.
params_horizontal <-
  fit_gaussian_2D(samp_dat, Q_strategy = -1)

## Generate a grid of x- and y- values on which to predict
grid <-
  expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
              Y_values = seq(from = -1, to = 4, by = 0.1))

## Predict the values using predict_gaussian_2D
## First, the unconstrained
gauss_data_unconstrained <-
  predict_gaussian_2D(
    X_values = grid$X_values,
    Y_values = grid$Y_values,
    A = params_unconstrained[1],
    Q = params_unconstrained[2],
    X_peak = params_unconstrained[3],
    Y_peak = params_unconstrained[4],
    X_var = params_unconstrained[5],
    Y_var = params_unconstrained[6]
  )

## And now the horizontal
gauss_data_horizontal <-
  predict_gaussian_2D(
    X_values = grid$X_values,
    Y_values = grid$Y_values,
    A = params_horizontal[1],
    Q = params_horizontal[2],
    X_peak = params_horizontal[3],
    Y_peak = params_horizontal[4],
    X_var = params_horizontal[5],
    Y_var = params_horizontal[6]
  )

## Plots via ggplot2 and metR
library(ggplot2); library(metR)
## Unconstrained
ggplot_gaussian_2D(gauss_data_unconstrained)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
## Constrained to horizontal orientation
ggplot_gaussian_2D(gauss_data_horizontal)
```

<img src="man/figures/README-example-2.png" width="100%" />

``` r

## And another example via lattice::levelplot()
library(lattice)
lattice::levelplot(
  predicted_values ~ X_values * Y_values,
  data = gauss_data_unconstrained,
  col.regions = colorRampPalette(
    c("white", "blue")
    )(100),
  asp = 1
)
```

<img src="man/figures/README-example-3.png" width="100%" />

## Citation

Baliga, VB. 2020. gaussplotR: Predict and plot 2D gaussians in R.
figshare. Software. <https://doi.org/10.6084/m9.figshare.12980717>

## License

GPL (\>= 3) + file LICENSE

🐢
