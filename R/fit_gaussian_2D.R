## Part of the gaussplotR package
## Last updated: 2020-11-27 VBB

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
#'   (i.e. the best-fit orientation is returned) or should it be pre-set by the
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
#' roughly equal). If this method is used, the fitted parameters are: Amp
#' (amplitude), X_peak (x-axis peak location), Y_peak (y-axis peak location),
#' X_sig (spread along x-axis), and Y_sig (spread along y-axis).
#'
#' A more generic method (and the default) is \code{method = "elliptical"}. This
#' allows the fitted 2D Gaussian to take a more ellipsoid shape (but note that
#' \code{method = "circular"} can be considered a special case of this). If this
#' method is used, the fitted parameters are: A_o (a constant term), Amp
#' (amplitude), theta (rotation, in radians, from the x-axis in the clockwise
#' direction), X_peak (x-axis peak location), Y_peak (y-axis peak location), a
#' (width of Gaussian along x-axis), and b (width of Gaussian along y-axis).
#'
#' A third method is \code{method = "elliptical_log"}. This is a further special
#' case in which log2-transformed data may be used. See Priebe et al. 2003 for
#' more details. Parameters from this model include: Amp (amplitude), Q
#' (orientation parameter), X_peak (x-axis peak location), Y_peak (y-axis peak
#' location), X_sig (spread along x-axis), and Y_sig (spread along y-axis).
#'
#' If using either \code{method = "elliptical"} or \code{method =
#' "elliptical_log"}, the \code{"orientation_strategy"} argument can be
#' used to specify how the orientation is set. In most cases, the user should
#' use the default "unconstrained" setting for this argument. Doing so will
#' provide the best-fit 2D Gaussian (assuming that the solution yielded by
#' \code{stats::nls()} converges on the global optimum).
#'
#' Setting \code{orientation_strategy} to a numeric (e.g.
#' \code{orientation_strategy = pi/2}) will force the orientation of the
#' Gaussian to the specified value. Note that this is handled differently by
#' \code{method = "elliptical"} vs \code{method = "elliptical_log"}. In
#' \code{method = "elliptical"}, the theta parameter dictates the rotation, in
#' radians, from the x-axis in the clockwise direction. In contrast, the
#' \code{method = "elliptical_log"} procedure uses a Q parameter to determine
#' the orientation of the 2D Gaussian. Setting \code{Q_strategy = 0} will result
#' in a diagonally-oriented Gaussian, whereas setting \code{Q_strategy = -1}
#' will result in horizontal orientation. See Priebe et al. 2003 for more
#' details.
#'
#' The \code{user_init} argument can also be used to supply a
#' vector of initial values for the A, Q, X_peak, Y_peak, X_var, and Y_var
#' parameters. If the user chooses to make use of \code{user_init}, then a
#' vector containing all parameters must be supplied in a particular order.
#'
#' Additional arguments to the \code{control} argument in \code{stats::nls()}
#' can be supplied via \code{...}.
#'
#' @return A data.frame that provides the best-fitting values for the specified
#' method. Refer to the Details section for the names of parameters.
#'
#' @author Vikram B. Baliga
#'
#' @references Priebe NJ, Cassanello CR, Lisberger SG. The neural representation
#'   of speed in macaque area MT/V5. J Neurosci. 2003 Jul 2;23(13):5650-61. doi:
#'   10.1523/JNEUROSCI.23-13-05650.2003.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ## Load the sample data set
#'   data(gaussplot_sample_data)
#'
#'   ## The raw data we'd like to use are in columns 1:3
#'   samp_dat <-
#'     gaussplot_sample_data[,1:3]
#'
#'
#'   #### Example 1: Unconstrained elliptical ####
#'   gauss_fit <-
#'     fit_gaussian_2D(samp_dat)
#'   ## This fits an unconstrained elliptical by default
#'   attr(gauss_fit, "fit_method")
#'
#'   ## Generate a grid of x- and y- values on which to predict
#'   grid <-
#'     expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
#'                 Y_values = seq(from = -1, to = 4, by = 0.1))
#'
#'   ## Predict the values using predict_gaussian_2D
#'   gauss_data <-
#'     predict_gaussian_2D(
#'       fit_params = gauss_fit,
#'       X_values = grid$X_values,
#'       Y_values = grid$Y_values,
#'     )
#'
#'   ## Plot via ggplot2 and metR
#'   library(ggplot2); library(metR)
#'   ggplot_gaussian_2D(gauss_data)
#'
#'   #### Example 2: Constrained elliptical_log ####
#'   gauss_fit <-
#'     fit_gaussian_2D(
#'       samp_dat,
#'       method = "elliptical_log",
#'       orientation_strategy = -1
#'     )
#'   ## This fits a constrained elliptical, as in Priebe et al. 2003
#'   attr(gauss_fit, "fit_method")
#'
#'   ## Generate a grid of x- and y- values on which to predict
#'   grid <-
#'     expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
#'                 Y_values = seq(from = -1, to = 4, by = 0.1))
#'
#'   ## Predict the values using predict_gaussian_2D
#'   gauss_data <-
#'     predict_gaussian_2D(
#'       fit_params = gauss_fit,
#'       X_values = grid$X_values,
#'       Y_values = grid$Y_values,
#'     )
#'
#'   ## Plot via ggplot2 and metR
#'   ggplot_gaussian_2D(gauss_data)
#' }


fit_gaussian_2D <- function(data,
                            method = "elliptical",
                            orientation_strategy = "unconstrained",
                            user_init = NULL,
                            maxiter = 1000,
                            verbose = FALSE,
                            ...) {

  #### Guesstimate starting points for parameters ####
  A_o_init <- 1
  Q_init <- -0.5
  theta_init <- pi
  max_row <- which.max(data[,"response"])
  Amp_init <- max(data[,"response"])
  X_peak_init <- data[max_row,"X_values"]
  Y_peak_init <- data[max_row,"Y_values"]
  X_sig_init <- stats::sd(data[,"X_values"])
  Y_sig_init <- stats::sd(data[,"Y_values"])
  a_init <- stats::sd(data[,"X_values"])
  b_init <- stats::sd(data[,"Y_values"])

  #### elliptical ####
  if (method == "elliptical"){
    ## overwrite with user-supplied parameters, if supplied ##
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
      res <- as.data.frame(t(stats::coef(fit_generic)))
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
      fit_coefs <- stats::coef(fit_generic_const)
      coef_result <- c(
        fit_coefs[c(1,2)], theta = orientation_strategy, fit_coefs[c(3:6)]
      )
      res <- as.data.frame(t(coef_result))
      attr(res, "fit_method") = "elliptical_constr"
      return(res)
    }

  }

  #### elliptical_log ####
  if (method == "elliptical_log"){
    ## overwrite with user-supplied parameters, if supplied ##
    if (!is.null(user_init)){
      Amp         <- user_init[1]
      Q_init      <- user_init[2]
      X_peak_init <- user_init[3]
      Y_peak_init <- user_init[4]
      X_var_init  <- user_init[5]
      Y_var_init  <- user_init[6]
    }

    if (orientation_strategy == "unconstrained") {
      result <-
        stats::nls(
          response ~  Amp_init * exp(-((X_values - X_peak) ^ 2) / (X_sig ^ 2)) *
            exp(-(Y_values - ((Q + 1) * (X_values - X_peak) +
                                Y_peak
            )) ^ 2 / (Y_sig ^ 2)),
          start =
            c(
              Q = Q_init,
              X_peak = X_peak_init,
              Y_peak = Y_peak_init,
              X_sig = X_sig_init,
              Y_sig = Y_sig_init
            ),
          data = data,
          trace = verbose,
          control = list(maxiter = maxiter, ...)
          )

      ## Export
      fit_coefs <- stats::coef(result)
      coef_result <-
        c(Amp = Amp_init, fit_coefs)
      coef_result <- as.data.frame(t(coef_result))
      attr(coef_result, "fit_method") <- "elliptical_log_unconstr"
      return(coef_result)
    }

    if (is.numeric(orientation_strategy)) {
      result <-
        stats::nls(
          response ~  Amp_init * exp(-((X_values - X_peak) ^ 2) / (X_sig ^ 2)) *
            exp(-(Y_values - ((orientation_strategy + 1) * (X_values - X_peak) +
                                Y_peak
            )) ^ 2 / (Y_sig ^ 2)),
          start =
            c(X_peak = X_peak_init,
              Y_peak = Y_peak_init,
              X_sig = X_sig_init,
              Y_sig = Y_sig_init
            ),
          data = data,
          trace = verbose,
          control = list(maxiter = maxiter, ...)
          )

      ## Export
      fit_coefs <- stats::coef(result)
      coef_result <-
        c(Amp = Amp_init, Q = orientation_strategy, fit_coefs)
      coef_result <- as.data.frame(t(coef_result))
      attr(coef_result, "fit_method") <- "elliptical_log_constr"
      return(coef_result)
    }
  }

  #### circular ####
  if (method == "circular"){
    ## overwrite with user-supplied parameters, if supplied ##
    if (!is.null(user_init)){
      Amp_init    <- user_init[1]
      X_peak_init <- user_init[2]
      Y_peak_init <- user_init[3]
      X_sig        <- user_init[4]
      Y_sig        <- user_init[5]
    }

    fit_circ <-
      stats::nls(
        response ~ Amp * exp(
          -(
            (((X_values - X_peak)^2) / (2 * X_sig^2) +
               ((Y_values - Y_peak)^2)/(2 * Y_sig^2))
          )
        )
        ,
        start =
          c(Amp    = Amp_init,
            X_peak = X_peak_init,
            Y_peak = Y_peak_init,
            X_sig   = X_sig_init,
            Y_sig   = Y_sig_init
          ),
        data = data,
        trace = verbose,
        control = list(maxiter = maxiter, ...)
      )
    res <- as.data.frame(t(stats::coef(fit_circ)))
    attr(res, "fit_method") = "circular"
    return(res)

  }
}
