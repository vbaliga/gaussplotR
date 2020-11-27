## Part of the gaussplotR package
## Last updated: 2020-11-26 VBB

############################# fit_gaussian_2D ##############################

#' Determine the best-fit parameters of a 2D Gaussian fit to a data set
#'
#' @param data A data.frame that contains the raw data (generally rectilinearly
#'   gridded data, but this is not a strict requirement). Columns must be named
#'   \code{"X_values"}, \code{"Y_values"} and \code{"response"}.
#' @param method Choice of \code{"circular"}, \code{"elliptical"}, or
#'   \code{"elliptical_log"}. Determine which specific implementation of 2D
#'   Gaussian to use. See Details for more.
#' @param orientation_strategy If using \code{"elliptical"} or \code{method =
#'   "elliptical_log"}, should the orientation of the Gaussian be unconstrained
#'   (i.e. the best-fit orienation is returned) or should it be pre-set by the
#'   user? See Details for more.
#' @param user_init Default NULL; if desired, the user can supply initial values
#' for the parameters of the chosen model. See Details for more.
#' @param maxiter Default 1000. A positive integer specifying the maximum number
#'   of iterations allowed. See \code{stats::nls.control()} for more details.
#' @param verbose TRUE or FALSE; should the trace of the iteration be
#' printed? See the \code{trace} argument of \code{stats::nls()} for more
#' detail.
#' @param ... Additional arguments passed to \code{stats::nls()}
#'
#' @details \code{stats::nls()} is used to fit parameters for a 2D Gaussian to
#'   the supplied data. Each method uses (slightly) different sets of
#'   parameters. Note that for a small (but non-trivial) proportion of data
#'   sets, nonlinear least squares may fail due to singularities or other
#'   issues. Most often, this occurs because of the starting parameters that are
#'   fed in. By default, this function attempts to set default parameters by
#'   making an educated guess about the major aspects of the supplied data.
#'   Should this strategy fail, the user can make use of the \code{user_init}
#'   argument to supply an alternate set of starting values.
#'
#' The simplest method is \code{method = "circular"}. Here, the 2D Gaussian is
#' constrained to have a roughly circular shape (i.e. spread in X- and Y- are
#' roughly equal). If this method is used, the fitted parameters are: A
#' (amplitude), X_peak (x-axis peak location), Y_peak (y-axis peak location),
#' X_sd (spread along x-axis), and Y_sd (spread along y-axis).
#'
#' A more generic method (and the default) is \code{method = "elliptical"}. This
#' allows the fitted 2D Gaussian to take a more ellipsoid shape (but note that
#' \code{method = "circular"} can be considered a special case of this). If this
#' method is used, the fitted parameters are: A (amplitude), X_peak (x-axis peak
#' location), Y_peak (y-axis peak location), and theta (rotation, in radians,
#' from the x-axis, in the clockwise direction).
#'
#' A third method is \code{method = "elliptical_log"}. This is a further special
#' case in which log2-transformed data may be used. See Priebe et al. 2013 for
#' more details. Parameters from this model include A (amplitude), Q
#' (orientation parameter), X_peak (x-axis peak location), Y_peak (y-axis peak
#' location), X_sd (spread along x-axis), and Y_sd (spread along y-axis).
#'
#' Similarly, if using \code{method = "elliptical_log"}, the Q parameter
#' determines the orientation of the 2D Gaussian. As such, a constraint could be
#' applied to the Q parameter by the user to dictate the orientation of the
#' ellpise.  In most cases, the user should use the default "unconstrained"
#' setting for this argument. Doing so will provide the best-fit 2D Gaussian
#' (assuming that the solution yielded by \code{stats::nls()} converges on the
#' global optimum). By choosing to supply numeric values to the
#' \code{Q_strategy} argument, the user can dictate the orientation of the 2D
#' Gaussian. Setting \code{Q_strategy = 0} will result in a diagonally-oriented
#' Gaussian, whereas setting \code{Q_strategy = -1} will result in horizontal
#' orientation.
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
                            method = "elliptical",
                            orientation_strategy = "unconstrained",
                            user_init = NULL,
                            maxiter = 1000,
                            verbose = FALSE,
                            ...) {

  #### Guesstimate starting points for parameters ####
  A_o_init = 1
  Q_init <- -0.5
  theta_init <- pi
  max_row <- which.max(data[,"response"])
  Amp_init <- max(data[,"response"])
  X_peak_init <- data[max_row,"X_values"]
  Y_peak_init <- data[max_row,"Y_values"]
  X_sd_init <- sd(data[,"X_values"])
  Y_sd_init <- sd(data[,"Y_values"])
  a_init <- 1
  b_init <- 1


  if (method == "elliptical"){

    #### Overwrite with user-supplied parameters, if supplied ####
    if (!is.null(user_init)){
      A_o_init    <- user_init[1]
      Amp_init    <- user_init[2]
      theta_init  <- user_init[3]
      X_peak_init <- user_init[4]
      Y_peak_init <- user_init[5]
      a_init      <- user_init[6]
      b_init      <- user_init[7]
    }

    if (orientation_strategy == "unconstrained") {
      fit_generic <-
        stats::nls(
          response ~ A_o + Amp * exp(
            -(
              (((X_values - X_peak)*cos(theta) -
                  (Y_values - Y_peak)*sin(theta))/a)^2 +
                (((X_values - X_peak)*sin(theta) -
                    (Y_values - Y_peak)*cos(theta))/b)^2
            ) / 2
          )
          ,
          start =
            c(A_o    = A_o_init,
              Amp    = Amp_init,
              theta  = theta_init,
              X_peak = X_peak_init,
              Y_peak = Y_peak_init,
              a      = a_init,
              b      = b_init
            ),
          data = data,
          trace = verbose,
          control = list(maxiter = maxiter, ...)
        )
      res <- coef(fit_generic)
      attr(res, "fit_method") = "elliptical_unconstr"
      return(res)
    }

    if (is.numeric(orientation_strategy)) {
      fit_generic_const <-
        stats::nls(
          response ~ A_o + Amp * exp(
            -(
              (((X_values - X_peak)*cos(orientation_strategy) -
                  (Y_values - Y_peak)*sin(orientation_strategy))/a)^2 +
                (((X_values - X_peak)*sin(orientation_strategy) -
                    (Y_values - Y_peak)*cos(orientation_strategy))/b)^2
            ) / 2
          )
          ,
          start =
            c(A_o    = A_o_init,
              Amp    = Amp_init,
              X_peak = X_peak_init,
              Y_peak = Y_peak_init,
              a      = a_init,
              b      = b_init
            ),
          data = data,
          trace = verbose,
          control = list(maxiter = maxiter, ...)
        )
      res <- coef(fit_generic_const)
      attr(res, "fit_method") = "elliptical_constr"
      return(res)
    }

  }

  if (method == "elliptical_log"){
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
          maxiter = maxiter,
          ...)

      ## Export
      fit_coefs <- coef(result)
      coef_result <-
        c(A = Amp, fit_coefs)
      attr(coef_result, "fit_method") <- "elliptical_log_unconstr"
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
          maxiter = maxiter,
          ...)

      ## Export
      fit_coefs <- coef(result)
      coef_result <-
        c(A = Amp, Q = Q_strategy, fit_coefs)
      attr(coef_result, "fit_method") <- "elliptical_log_constr"
      return(coef_result)
    }
  }

  if (method == "circular"){

    #### Overwrite with user-supplied parameters, if supplied ####
    if (!is.null(user_init)){
      Amp_init    <- user_init[1]
      X_peak_init <- user_init[2]
      Y_peak_init <- user_init[3]
      X_sd        <- user_init[4]
      Y_sd        <- user_init[5]
    }

    fit_circ <-
      stats::nls(
        response ~ Amp * exp(
          -(
            (((X_values - X_peak)^2) / (2 * X_sd^2) +
               ((Y_values - Y_peak)^2)/(2 * Y_sd^2))
          )
        )
        ,
        start =
          c(Amp    = Amp_init,
            X_peak = X_peak_init,
            Y_peak = Y_peak_init,
            X_sd   = X_sd_init,
            Y_sd   = Y_sd_init
          ),
        data = data,
        trace = verbose,
        control = list(maxiter = maxiter, ...)
      )
    res <- coef(fit_circ)
    attr(res, "fit_method") = "circular"
    return(res)

  }
}
