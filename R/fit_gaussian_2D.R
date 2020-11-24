## Part of the gaussplotR package
## Last updated: 2020-11-23 VBB

############################# fit_gaussian_2D ##############################

#' Determine the best-fit parameters of a 2D Gaussian fit to a data set
#'
#' @param data A data.frame or tibble that contains the raw data. Columns must
#' have "X_values", "Y_values" and "response" as column names.
#' @param Q_strategy Should the "Q" parameter of the model be constrained? See
#' Details for more.
#' @param user_init Default NULL; if desired, the user can supply initial values
#' for the A, X_peak, Y_peak, X_var, and Y_var parameters. See Details for more.
#' @param verbose TRUE or FALSE; should the trace of the iteration be
#' printed? See the \code{trace} argument of \code{stats::nls()} for more
#' detail.
#' @param ... Additional arguments passed to \code{stats::nls()}
#'
#' @details \code{stats::nls()} is used to fit parameters for a 2D Gaussian to
#' the supplied data. Such parameters include A (amplitude),
#' Q (orientation parameter), X_peak (x-axis peak location), Y_peak (y-axis
#' peak location), X_var (spread along x-axis), and Y_var (spread along y-axis).
#'
#' The Q parameter determines the orientation of the 2D Gaussian. In most cases,
#' the user should use the default "unconstrained" setting for this argument.
#' Doing so will provide the best-fit 2D Gaussian (assuming that the solution
#' yielded by \code{stats::nls()} converges on the global optimum). By choosing
#' to supply numeric values to the \code{Q_strategy} argument, the user can
#' dictate the orientation of the 2D Gaussian. Setting \code{Q_strategy = 0}
#' will result in a diagonally-oriented Gaussian, whereas setting
#' \code{Q_strategy = -1} will result in horizontal orientation.
#'
#' The \code{user_init} argument can also be used to supply a
#' vector of initial values for the A, Q, X_peak, Y_peak, X_var, and Y_var
#' parameters. If the user chooses to make use of \code{user_init}, then a
#' vector containing all 6 parameters must be supplied in that particular order.
#' Additional arguments (e.g. \code{control}) can be supplied to
#' \code{stats::nls()} via \code{...}.
#'
#' @return A vector that provides the best-fitting values of A (amplitude),
#' Q (orientation parameter), X_peak (x-axis peak location), Y_peak (y-axis
#' peak location), X_var (spread along x-axis), and Y_var (spread along y-axis).
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
#'   ## The relevant data are in the first three columns
#'   samp_dat <- gaussplot_sample_data[,1:3]
#'
#'   ## Fit a 2D Gaussian with no user-supplied constraints; this
#'   ## will generally lead to the best fit
#'   params_unconstrained <-
#'     fit_gaussian_2D(samp_dat)
#'
#'   ## If desired, the user can specify contraints. E.g. force the
#'   ## gaussian to be horizontally oriented.
#'   ## Note that this will generally lead to poor fits and should
#'   ## be performed with caution
#'   params_horizontal <-
#'     fit_gaussian_2D(samp_dat, Q_strategy = -1)
#'
#'   ## Generate a grid of x- and y- values on which to predict
#'   grid <-
#'     expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
#'                 Y_values = seq(from = -1, to = 4, by = 0.1))
#'
#'   ## Predict the values using predict_gaussian_2D
#'   ## First, the unconstrained
#'   gauss_data_unconstrained <-
#'     predict_gaussian_2D(
#'       X_values = grid$X_values,
#'       Y_values = grid$Y_values,
#'       A = params_unconstrained[1],
#'       Q = params_unconstrained[2],
#'       X_peak = params_unconstrained[3],
#'       Y_peak = params_unconstrained[4],
#'       X_var = params_unconstrained[5],
#'       Y_var = params_unconstrained[6]
#'     )
#'
#'   ## And now the horizontal
#'   gauss_data_horizontal <-
#'     predict_gaussian_2D(
#'       X_values = grid$X_values,
#'       Y_values = grid$Y_values,
#'       A = params_horizontal[1],
#'       Q = params_horizontal[2],
#'       X_peak = params_horizontal[3],
#'       Y_peak = params_horizontal[4],
#'       X_var = params_horizontal[5],
#'       Y_var = params_horizontal[6]
#'     )
#'
#'   ## Plot via ggplot2 and metR
#'   library(ggplot2); library(metR)
#'   ggplot_gaussian_2D(gauss_data_unconstrained)
#'   ggplot_gaussian_2D(gauss_data_horizontal)
#'  }


fit_gaussian_2D <- function(data,
                            user_init = NULL,
                            Q_strategy = "unconstrained",
                            verbose = FALSE,
                            ...) {

  #### Guesstimate starting points for parameters ####
  Q_init <- -0.5
  max_row <- which.max(data[,"response"])
  Amp <- max(data[,"response"])
  X_peak_init <- data[max_row,"X_values"]
  Y_peak_init <- data[max_row,"Y_values"]
  X_var_init <- var(data[,"X_values"])
  Y_var_init <- var(data[,"Y_values"])

  #### Overwrite with user-supplied parameters, if supplied ####
  if (!is.null(user_init)){
    Amp         <- user_init[1]
    Q_init      <- user_init[2]
    X_peak_init <- user_init[3]
    Y_peak_init <- user_init[4]
    X_var_init  <- user_init[5]
    Y_var_init  <- user_init[6]

  }

  if (Q_strategy == "unconstrained") {
    result <-
      stats::nls(
        response ~  Amp * exp(-((X_values - X_peak) ^ 2) / (X_var ^ 2)) *
          exp(-(Y_values - ((Q + 1) * (X_values - X_peak) +
                              Y_peak
          )) ^ 2 / (Y_var ^ 2)),
        start =
          c(
            Q = Q_init,
            X_peak = X_peak_init,
            Y_peak = Y_peak_init,
            X_var = X_var_init,
            Y_var = Y_var_init
            ),
        data = data,
        trace = verbose,
        ...)

    ## Export
    fit_coefs <- coef(result)
    coef_result <-
      c(A = Amp, fit_coefs)
    return(coef_result)
  }

  if (is.numeric(Q_strategy)) {
    result <-
      stats::nls(
        response ~  Amp * exp(-((X_values - X_peak) ^ 2) / (X_var ^ 2)) *
          exp(-(Y_values - ((Q_strategy + 1) * (X_values - X_peak) +
                              Y_peak
          )) ^ 2 / (Y_var ^ 2)),
        start =
          c(
            X_peak = X_peak_init,
            Y_peak = Y_peak_init,
            X_var = X_var_init,
            Y_var = Y_var_init
            ),
        data = data,
        trace = verbose,
        ...)

    ## Export
    fit_coefs <- coef(result)
    coef_result <-
      c(A = Amp, Q = Q_strategy, fit_coefs)
    return(coef_result)
  }

}
