## Part of the gaussplotR package
## Last updated: 2021-03-18 VBB

############################# fit_gaussian_2D ##############################

#' Determine the best-fit parameters for a specific 2D-Gaussian model
#'
#' @param data A data.frame that contains the raw data (generally rectilinearly
#'   gridded data, but this is not a strict requirement). Columns must be named
#'   \code{"X_values"}, \code{"Y_values"} and \code{"response"}.
#' @param method Choice of \code{"elliptical"}, \code{"elliptical_log"}, or
#'   \code{"circular"}. Determine which specific implementation of 2D-Gaussian
#'   to use. See Details for more.
#' @param constrain_amplitude Default FALSE; should the amplitude of the
#'   Gaussian be set to the maximum value of the \code{"response"} variable
#'   (\code{TRUE}), or should the amplitude fitted by \code{stats::nls()}
#'   (\code{FALSE})?
#' @param constrain_orientation If using \code{"elliptical"} or \code{method =
#'   "elliptical_log"}, should the orientation of the Gaussian be unconstrained
#'   (i.e. the best-fit orientation is returned) or should it be pre-set by the
#'   user? See Details for more. Defaults to \code{"unconstrained"}.
#' @param user_init Default NULL; if desired, the user can supply initial values
#'   for the parameters of the chosen model. See Details for more.
#' @param maxiter Default 1000. A positive integer specifying the maximum number
#'   of iterations allowed. See \code{stats::nls.control()} for more details.
#' @param verbose TRUE or FALSE; should the trace of the iteration be printed?
#'   See the \code{trace} argument of \code{stats::nls()} for more detail.
#' @param print_initial_params TRUE or FALSE; should the set of initial
#'   parameters supplied to \code{stats::nls()} be printed to the console? Set
#'   to FALSE by default to avoid confusion with the fitted parameters attained
#'   after using \code{stats::nls()}.
#' @param ... Additional arguments passed to \code{stats::nls.control()}
#'
#' @details \code{stats::nls()} is used to fit parameters for a 2D-Gaussian to
#'   the supplied data. Each method uses (slightly) different sets of
#'   parameters. Note that for a small (but non-trivial) proportion of data
#'   sets, nonlinear least squares may fail due to singularities or other
#'   issues. Most often, this occurs because of the starting parameters that are
#'   fed in. By default, this function attempts to set default parameters by
#'   making an educated guess about the major aspects of the supplied data.
#'   Should this strategy fail, the user can make use of the \code{user_init}
#'   argument to supply an alternate set of starting values.
#'
#'   The simplest method is \code{method = "circular"}. Here, the 2D-Gaussian is
#'   constrained to have a roughly circular shape (i.e. spread in X- and Y- are
#'   roughly equal). If this method is used, the fitted parameters are: Amp
#'   (amplitude), X_peak (x-axis peak location), Y_peak (y-axis peak location),
#'   X_sig (spread along x-axis), and Y_sig (spread along y-axis).
#'
#'   A more generic method (and the default) is \code{method = "elliptical"}.
#'   This allows the fitted 2D-Gaussian to take a more ellipsoid shape (but note
#'   that \code{method = "circular"} can be considered a special case of this).
#'   If this method is used, the fitted parameters are: A_o (a constant term),
#'   Amp (amplitude), theta (rotation, in radians, from the x-axis in the
#'   clockwise direction), X_peak (x-axis peak location), Y_peak (y-axis peak
#'   location), a (width of Gaussian along x-axis), and b (width of Gaussian
#'   along y-axis).
#'
#'   A third method is \code{method = "elliptical_log"}. This is a further
#'   special case in which log2-transformed data may be used. See Priebe et al.
#'   2003 for more details. Parameters from this model include: Amp (amplitude),
#'   Q (orientation parameter), X_peak (x-axis peak location), Y_peak (y-axis
#'   peak location), X_sig (spread along x-axis), and Y_sig (spread along
#'   y-axis).
#'
#'   If using either \code{method = "elliptical"} or \code{method =
#'   "elliptical_log"}, the \code{"constrain_orientation"} argument can be used
#'   to specify how the orientation is set. In most cases, the user should use
#'   the default "unconstrained" setting for this argument. Doing so will
#'   provide the best-fit 2D-Gaussian (assuming that the solution yielded by
#'   \code{stats::nls()} converges on the global optimum).
#'
#'   Setting \code{constrain_orientation} to a numeric (e.g.
#'   \code{constrain_orientation = pi/2}) will force the orientation of the
#'   Gaussian to the specified value. Note that this is handled differently by
#'   \code{method = "elliptical"} vs \code{method = "elliptical_log"}. In
#'   \code{method = "elliptical"}, the theta parameter dictates the rotation, in
#'   radians, from the x-axis in the clockwise direction. In contrast, the
#'   \code{method = "elliptical_log"} procedure uses a Q parameter to determine
#'   the orientation of the 2D-Gaussian. Setting \code{constrain_orientation =
#'   0} will result in a diagonally-oriented Gaussian, whereas setting
#'   \code{constrain_orientation = -1} will result in horizontal orientation.
#'   See Priebe et al. 2003 for more details.
#'
#'   The \code{user_init} argument can also be used to supply a vector of
#'   initial values for the A, Q, X_peak, Y_peak, X_var, and Y_var parameters.
#'   If the user chooses to make use of \code{user_init}, then a vector
#'   containing all parameters must be supplied in a particular order.
#'
#'   Additional arguments to the \code{control} argument in \code{stats::nls()}
#'   can be supplied via \code{...}.
#'
#' @return A list with the components:
##' \itemize{
##'  \item{"coefs"} {A data.frame of fitted model parameters.}
##'  \item{"model"} {The model object, fitted by \code{stats::nls()}.}
##'  \item{"model_error_stats"} {A data.frame detailing the rss, rmse, deviance,
##'  and AIC of the fitted model.}
##'  \item{"fit_method"} {A character vector that indicates which method and
##'  orientation strategy was used by this function.}
##' }
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
#'   ## This fits an unconstrained elliptical by default
#'   gauss_fit <-
#'     fit_gaussian_2D(samp_dat)
#'
#'   ## Generate a grid of x- and y- values on which to predict
#'   grid <-
#'     expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
#'                 Y_values = seq(from = -1, to = 4, by = 0.1))
#'
#'   ## Predict the values using predict_gaussian_2D
#'   gauss_data <-
#'     predict_gaussian_2D(
#'       fit_object = gauss_fit,
#'       X_values = grid$X_values,
#'       Y_values = grid$Y_values,
#'     )
#'
#'   ## Plot via ggplot2 and metR
#'   library(ggplot2); library(metR)
#'   ggplot_gaussian_2D(gauss_data)
#'
#'   ## Produce a 3D plot via rgl
#'   rgl_gaussian_2D(gauss_data)
#'
#'
#'   #### Example 2: Constrained elliptical_log ####
#'   ## This fits a constrained elliptical, as in Priebe et al. 2003
#'   gauss_fit <-
#'     fit_gaussian_2D(
#'       samp_dat,
#'       method = "elliptical_log",
#'       constrain_orientation = -1
#'     )
#'
#'   ## Generate a grid of x- and y- values on which to predict
#'   grid <-
#'     expand.grid(X_values = seq(from = -5, to = 0, by = 0.1),
#'                 Y_values = seq(from = -1, to = 4, by = 0.1))
#'
#'   ## Predict the values using predict_gaussian_2D
#'   gauss_data <-
#'     predict_gaussian_2D(
#'       fit_object = gauss_fit,
#'       X_values = grid$X_values,
#'       Y_values = grid$Y_values,
#'     )
#'
#'   ## Plot via ggplot2 and metR
#'   ggplot_gaussian_2D(gauss_data)
#'
#'   ## Produce a 3D plot via rgl
#'   rgl_gaussian_2D(gauss_data)
#' }


fit_gaussian_2D <- function(data,
                            method = "elliptical",
                            constrain_amplitude = FALSE,
                            constrain_orientation = "unconstrained",
                            user_init = NULL,
                            maxiter = 1000,
                            verbose = FALSE,
                            print_initial_params = FALSE,
                            ...) {

  #### argument checks ####

  ## data names
  if (!any(names(data) == "X_values")) {
    stop("'X_values' column not found")
  }
  if (!any(names(data) == "Y_values")) {
    stop("'Y_values' column not found")
  }
  if (!any(names(data) == "response")) {
    stop("'response' column not found")
  }

  ## method
  if (!is.character(method)) {
    stop("'method' must be one of: 'elliptical', 'elliptical_log', 'circular'")
  }
  method_choices <- c("elliptical", "elliptical_log", "circular")
  if (!method %in% method_choices) {
    stop("'method' must be one of: 'elliptical', 'elliptical_log', 'circular'")
  }

  ## constrain_amplitude
  if (!is.logical(constrain_amplitude)) {
    stop("constrain_amplitude must be either TRUE or FALSE")
  }

  ## orientation
  if (is.character(constrain_orientation)) {
    if (!constrain_orientation == "unconstrained") {
      message(
        "'constrain_orientation' must either be 'unconstrained' or a
              numeric. Defaulting to 'unconstrained'"
      )
    }
  }

  ## user_init
  if (!is.null(user_init)) {
    if (!is.numeric(user_init)){
      stop("'user_init' must contain numeric values only")
    }
  }

  ## maxiter
  if (!is.numeric(maxiter)) {
    stop("'maxiter' must be a numeric")
  }

  ## verbose
  if (!is.logical(verbose)) {
    stop("'verbose' must be either TRUE or FALSE")
  }

  ## print_initial_params
  if (!is.logical(print_initial_params)) {
    stop("'print_initial_params' must be either TRUE or FALSE")
  }



  #### guesstimate starting points for parameters ####
  A_o_init <-
    min(data[, "response"])
  Q_init <-
    -0.5
  theta_init <-
    pi
  max_row <-
    which.max(data[, "response"])
  Amp_init <-
    max(data[, "response"])
  X_peak_init <-
    data[max_row, "X_values"]
  Y_peak_init <-
    data[max_row, "Y_values"]

  ## find 66%
  qtile_val <-
    stats::quantile(data[, "response"], 0.66)
  upperdat <-
    data[which(data[, "response"] > qtile_val), ]
  ud_xrange <-
    range(upperdat$X_values)
  ud_xr <-
    abs(ud_xrange[2] - ud_xrange[1])
  ud_yrange <-
    range(upperdat$Y_values)
  ud_yr <-
    abs(ud_yrange[2] - ud_yrange[1])

  ## Use these ranges to guesstimate initial spread values
  X_sig_init <-
    ud_xr / 2
  Y_sig_init <-
    ud_yr / 2
  a_init <-
    X_sig_init / 2
  b_init <-
    Y_sig_init / 2



  #### elliptical ####
  if (method == "elliptical") {

    ## overwrite with user-supplied parameters, if supplied ##
    if (!is.null(user_init)) {
      A_o_init    <-
        user_init[1]
      Amp_init    <-
        user_init[2]
      theta_init  <-
        user_init[3]
      X_peak_init <-
        user_init[4]
      Y_peak_init <-
        user_init[5]
      a_init      <-
        user_init[6]
      b_init      <-
        user_init[7]
    }

    #### _unconstrained orientation ####
    if (!is.numeric(constrain_orientation)) {

      ## print initial params, if desired
      if (print_initial_params == TRUE) {
        params <-
          c(A_o_init,
            Amp_init,
            theta_init,
            X_peak_init,
            Y_peak_init,
            a_init,
            b_init)
        names(params) <-
          c("A_o",
            "Amp",
            "theta" ,
            "X_peak",
            "Y_peak",
            "a",
            "b")
        message("Initial parameters:")
        print(params)
      }

      ## deal with amplitude constraint choice
      if (constrain_amplitude == FALSE) {

        #### __unconstrained amplitude ####

        ## fit the model
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
            control = list(maxiter = maxiter,
                           ...)
          )

        ## extract the coefficients
        res <- as.data.frame(t(stats::coef(fit_generic)))

        ## create a data.frame of model error stats
        m_e_s <-
          data.frame(
            rss = sum(stats::resid(fit_generic) ^ 2),
            rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_generic) ^
                                                 2)),
            deviance = stats::deviance(fit_generic),
            AIC = stats::AIC(fit_generic)
          )

        ## construct output and return
        output <-
          list(
            coefs = res,
            model = fit_generic,
            model_error_stats = m_e_s,
            fit_method =
              c(method = "elliptical",
                amplitude = "unconstrained",
                orientation = "unconstrained"
                )
          )
        attr(output, "gaussplotR") <- "gaussplotR_fit"
        class(output) <- c(class(output), "gaussplotR_fit")
        return(output)

      } else {

        #### __constrained amplitude ####

        ## fit the model
        fit_generic <-
          stats::nls(
            response ~ A_o + Amp_init * exp(
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

        ## extract coefficients and paste together
        fit_coefs <- stats::coef(fit_generic)
        coef_result <-
          c(fit_coefs[1],
            Amp = Amp_init,
            fit_coefs[c(2:6)])
        res <- as.data.frame(t(coef_result))

        ## create a data.frame of model error stats
        m_e_s <-
          data.frame(
            rss = sum(stats::resid(fit_generic) ^ 2),
            rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_generic) ^
                                                 2)),
            deviance = stats::deviance(fit_generic),
            AIC = stats::AIC(fit_generic)
          )

        ## construct output and return
        output <-
          list(
            coefs = res,
            model = fit_generic,
            model_error_stats = m_e_s,
            fit_method =
              c(method = "elliptical",
                amplitude = "constrained",
                orientation = "unconstrained"
              )
          )
        attr(output, "gaussplotR") <- "gaussplotR_fit"
        class(output) <- c(class(output), "gaussplotR_fit")
        return(output)

      }

    }

  #### _constrained orientation ####
  if (is.numeric(constrain_orientation)) {

    ## print initial params, if desired
    if (print_initial_params == TRUE){
      params <-
        c(A_o_init,
          Amp_init,
          constrain_orientation,
          X_peak_init,
          Y_peak_init,
          a_init,
          b_init)
      names(params) <-
        c("A_o",
          "Amp",
          "theta" ,
          "X_peak",
          "Y_peak",
          "a",
          "b")
      message("Initial parameters:"); print(params)
    }

    ## deal with amplitude constraint choice
    if (constrain_amplitude == FALSE) {

      #### __unconstrained amplitude ####

      ## fit the model
      fit_generic_const <-
        stats::nls(
          response ~ A_o + Amp * exp(
            -(
              (((X_values - X_peak)*cos(constrain_orientation) -
                  (Y_values - Y_peak)*sin(constrain_orientation))/a)^2 +
                (((X_values - X_peak)*sin(constrain_orientation) -
                    (Y_values - Y_peak)*cos(constrain_orientation))/b)^2
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
          control = list(maxiter = maxiter,
                         ...
          )
        )

      ## extract the coefficents and paste together
      fit_coefs <- stats::coef(fit_generic_const)
      coef_result <- c(
        fit_coefs[c(1,2)],
        theta = constrain_orientation,
        fit_coefs[c(3:6)]
      )
      res <- as.data.frame(t(coef_result))

      ## create a data.frame of model error stats
      m_e_s <-
        data.frame(
          rss = sum(stats::resid(fit_generic_const) ^ 2),
          rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_generic_const) ^
                                               2)),
          deviance = stats::deviance(fit_generic_const),
          AIC = stats::AIC(fit_generic_const)
        )

      ## construct the output and return
      output <-
        list(
          coefs = res,
          model = fit_generic_const,
          model_error_stats = m_e_s,
          fit_method =
            c(method = "elliptical",
              amplitude = "unconstrained",
              orientation = "constrained"
            )
        )
      attr(output, "gaussplotR") <- "gaussplotR_fit"
      class(output) <- c(class(output), "gaussplotR_fit")
      return(output)

    } else {

      #### __constrained amplitude ####

      ## fit the model
      fit_generic_const <-
        stats::nls(
          response ~ A_o + Amp_init * exp(
            -(
              (((X_values - X_peak)*cos(constrain_orientation) -
                  (Y_values - Y_peak)*sin(constrain_orientation))/a)^2 +
                (((X_values - X_peak)*sin(constrain_orientation) -
                    (Y_values - Y_peak)*cos(constrain_orientation))/b)^2
            ) / 2
          )
          ,
          start =
            c(A_o    = A_o_init,
              X_peak = X_peak_init,
              Y_peak = Y_peak_init,
              a      = a_init,
              b      = b_init
            ),
          data = data,
          trace = verbose,
          control = list(maxiter = maxiter,
                         ...
          )
        )

      ## extract the coefficients and paste together
      fit_coefs <- stats::coef(fit_generic_const)
      coef_result <- c(
        fit_coefs[1],
        Amp = Amp_init,
        theta = constrain_orientation,
        fit_coefs[c(2:5)]
      )
      res <- as.data.frame(t(coef_result))

      ## create a data.frame of model error stats
      m_e_s <-
        data.frame(
          rss = sum(stats::resid(fit_generic_const) ^ 2),
          rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_generic_const) ^
                                               2)),
          deviance = stats::deviance(fit_generic_const),
          AIC = stats::AIC(fit_generic_const)
        )

      ## construct the output and return
      output <-
        list(
          coefs = res,
          model = fit_generic_const,
          model_error_stats = m_e_s,
          fit_method =
            c(method = "elliptical",
              amplitude = "constrained",
              orientation = "constrained"
            )
        )
      attr(output, "gaussplotR") <- "gaussplotR_fit"
      class(output) <- c(class(output), "gaussplotR_fit")
      return(output)
      }

  }
  }


  #### elliptical_log ####
  if (method == "elliptical_log"){

    ## overwrite with user-supplied parameters, if supplied ##
    if (!is.null(user_init)){
      Amp_init    <- user_init[1]
      Q_init      <- user_init[2]
      X_peak_init <- user_init[3]
      Y_peak_init <- user_init[4]
      X_sig_init  <- user_init[5]
      Y_sig_init  <- user_init[6]
    }

    #### _unconstrained orientation ####
    if (constrain_orientation == "unconstrained") {

      ## print initial params, if desired
      if (print_initial_params == TRUE){
        params <-
          c(Amp_init,
            Q_init,
            X_peak_init,
            Y_peak_init,
            X_sig_init,
            Y_sig_init)
        names(params) <-
          c("Amp",
            "Q",
            "X_peak",
            "Y_peak",
            "X_sig",
            "Y_sig")
        message("Initial parameters:"); print(params)
      }

      ## deal with amplitude choice
      if (constrain_amplitude == FALSE) {

        #### __unconstrained amplitude ####

        ## fit the model
        fit_el <-
          stats::nls(
            response ~  Amp * exp(-((X_values - X_peak) ^ 2) / (X_sig ^ 2)) *
              exp(-(Y_values - ((Q + 1) * (X_values - X_peak) +
                                  Y_peak
              )) ^ 2 / (Y_sig ^ 2)),
            start =
              c(Amp    = Amp_init,
                Q      = Q_init,
                X_peak = X_peak_init,
                Y_peak = Y_peak_init,
                X_sig  = X_sig_init,
                Y_sig  = Y_sig_init
              ),
            data = data,
            trace = verbose,
            control = list(maxiter = maxiter,
                           ...
            )
          )

        ## extract the coefficents
        fit_coefs <- stats::coef(fit_el)
        res <- as.data.frame(t(fit_coefs))

        ## create a data.frame of model error stats
        m_e_s <-
          data.frame(
            rss = sum(stats::resid(fit_el) ^ 2),
            rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_el) ^
                                                 2)),
            deviance = stats::deviance(fit_el),
            AIC = stats::AIC(fit_el)
          )

        ## construct the output and return
        output <-
          list(
            coefs = res,
            model = fit_el,
            model_error_stats = m_e_s,
            fit_method =
              c(method = "elliptical_log",
                amplitude = "unconstrained",
                orientation = "unconstrained"
              )
          )
        attr(output, "gaussplotR") <- "gaussplotR_fit"
        class(output) <- c(class(output), "gaussplotR_fit")
        return(output)

      } else {

        #### __constrained amplitude ####

        ## fit the model
        fit_el <-
          stats::nls(
          response ~  Amp_init * exp(-((X_values - X_peak) ^ 2) / (X_sig ^ 2)) *
            exp(-(Y_values - ((Q + 1) * (X_values - X_peak) +
                                Y_peak
            )) ^ 2 / (Y_sig ^ 2)),
            start =
              c(Q      = Q_init,
                X_peak = X_peak_init,
                Y_peak = Y_peak_init,
                X_sig  = X_sig_init,
                Y_sig  = Y_sig_init
              ),
            data = data,
            trace = verbose,
            control = list(maxiter = maxiter,
                           ...
            )
          )

        ## extract the coefficients and paste together
        fit_coefs <- stats::coef(fit_el)
        coef_result <-
          c(Amp = Amp_init, fit_coefs)
        res <- as.data.frame(t(coef_result))

        ## create a data.frame of model error stats
        m_e_s <-
          data.frame(
            rss = sum(stats::resid(fit_el) ^ 2),
            rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_el) ^
                                                 2)),
            deviance = stats::deviance(fit_el),
            AIC = stats::AIC(fit_el)
          )

        ## construct the output and return
        output <-
          list(
            coefs = res,
            model = fit_el,
            model_error_stats = m_e_s,
            fit_method =
              c(method = "elliptical_log",
                amplitude = "constrained",
                orientation = "unconstrained"
              )
          )
        attr(output, "gaussplotR") <- "gaussplotR_fit"
        class(output) <- c(class(output), "gaussplotR_fit")
        return(output)

      }
    }

    #### _constrained orientation ####
    if (is.numeric(constrain_orientation)) {

      ## print initial params, if desired
      if (print_initial_params == TRUE){
        params <-
          c(Amp_init,
            constrain_orientation,
            X_peak_init,
            Y_peak_init,
            X_sig_init,
            Y_sig_init)
        names(params) <-
          c("Amp",
            "Q",
            "X_peak",
            "Y_peak",
            "X_sig",
            "Y_sig")
        message("Initial parameters:"); print(params)
      }

      ## deal with amplitude choice
      if (constrain_amplitude == FALSE) {

        #### __unconstrained amplitude ####

        ## fit the model
        fit_el <-
          stats::nls(
        response ~  Amp * exp(-((X_values - X_peak) ^ 2) / (X_sig ^ 2)) *
          exp(-(Y_values - ((constrain_orientation + 1) * (X_values - X_peak) +
                              Y_peak
          )) ^ 2 / (Y_sig ^ 2)),
            start =
              c(Amp    = Amp_init,
                X_peak = X_peak_init,
                Y_peak = Y_peak_init,
                X_sig  = X_sig_init,
                Y_sig  = Y_sig_init
              ),
            data = data,
            trace = verbose,
            control = list(maxiter = maxiter,
                           ...
            )
          )

        ## extract coefficients and paste together
        fit_coefs <- stats::coef(fit_el)
        coef_result <-
          c(fit_coefs[1],
            Q = constrain_orientation,
            fit_coefs[2:5])
        res <- as.data.frame(t(coef_result))

        ## create a data.frame of model error stats
        m_e_s <-
          data.frame(
            rss = sum(stats::resid(fit_el) ^ 2),
            rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_el) ^
                                                 2)),
            deviance = stats::deviance(fit_el),
            AIC = stats::AIC(fit_el)
          )

        ## construct the output and return
        output <-
          list(
            coefs = res,
            model = fit_el,
            model_error_stats = m_e_s,
            fit_method =
              c(method = "elliptical_log",
                amplitude = "unconstrained",
                orientation = "constrained"
              )
          )
        attr(output, "gaussplotR") <- "gaussplotR_fit"
        class(output) <- c(class(output), "gaussplotR_fit")
        return(output)

      } else {

        #### __constrained amplitude ####

        ## fit the model
        fit_el <-
          stats::nls(
        response ~  Amp_init * exp(-((X_values - X_peak) ^ 2) / (X_sig ^ 2)) *
          exp(-(Y_values - ((constrain_orientation + 1) * (X_values - X_peak) +
                              Y_peak
          )) ^ 2 / (Y_sig ^ 2)),
            start =
              c(X_peak = X_peak_init,
                Y_peak = Y_peak_init,
                X_sig  = X_sig_init,
                Y_sig  = Y_sig_init
              ),
            data = data,
            trace = verbose,
            control = list(maxiter = maxiter,
                           ...
            )
          )

        ## extract coefficients and paste together
        fit_coefs <- stats::coef(fit_el)
        coef_result <-
          c(Amp = Amp_init, Q = constrain_orientation, fit_coefs)
        res <- as.data.frame(t(coef_result))

        ## create a data.frame of model error stats
        m_e_s <-
          data.frame(
            rss = sum(stats::resid(fit_el) ^ 2),
            rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_el) ^
                                                 2)),
            deviance = stats::deviance(fit_el),
            AIC = stats::AIC(fit_el)
          )

        # construct the output and return
        output <-
          list(
            coefs = res,
            model = fit_el,
            model_error_stats = m_e_s,
            fit_method =
              c(method = "elliptical_log",
                amplitude = "constrained",
                orientation = "constrained"
              )
          )
        attr(output, "gaussplotR") <- "gaussplotR_fit"
        class(output) <- c(class(output), "gaussplotR_fit")
        return(output)

      }

    }
  }



  #### circular ####
  if (method == "circular") {

    ## overwrite with user-supplied parameters, if supplied ##
    if (!is.null(user_init)) {
      Amp_init    <- user_init[1]
      X_peak_init <- user_init[2]
      Y_peak_init <- user_init[3]
      X_sig_init  <- user_init[4]
      Y_sig_init  <- user_init[5]
    }

    ## print initial params, if desired
    if (print_initial_params == TRUE) {
      params <-
        c(Amp_init,
          X_peak_init,
          Y_peak_init,
          X_sig_init,
          Y_sig_init)
      names(params) <-
        c("Amp",
          "X_peak",
          "Y_peak",
          "X_sig",
          "Y_sig")
      message("Initial parameters:"); print(params)
    }

    ## deal with amplitude choice
    if (constrain_amplitude == FALSE) {

      #### __unconstrained amplitude ####

      ## fit the model
      fit_circ <-
        stats::nls(
          response ~ Amp * exp(-((((X_values - X_peak) ^ 2
          ) / (2 * X_sig ^ 2) +
            ((Y_values - Y_peak) ^ 2
            ) / (2 * Y_sig ^ 2))))
          ,
          start =
            c(Amp    = Amp_init,
              X_peak = X_peak_init,
              Y_peak = Y_peak_init,
              X_sig  = X_sig_init,
              Y_sig  = Y_sig_init
            ),
          data = data,
          trace = verbose,
          control =
            list(maxiter = maxiter,
                 ...)
        )

      ## extract the coefficients
      res <- as.data.frame(t(stats::coef(fit_circ)))

      ## create a data.frame of model error stats
      m_e_s <-
        data.frame(
          rss = sum(stats::resid(fit_circ) ^ 2),
          rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_circ) ^
                                               2)),
          deviance = stats::deviance(fit_circ),
          AIC = stats::AIC(fit_circ)
        )

      ## construct the output and return
      output <-
        list(
          coefs = res,
          model = fit_circ,
          model_error_stats = m_e_s,
          fit_method =
            c(method = "circular",
              amplitude = "unconstrained",
              orientation = NA
            )
        )
      attr(output, "gaussplotR") <- "gaussplotR_fit"
      class(output) <- c(class(output), "gaussplotR_fit")
      return(output)

    } else {

      #### __constrained amplitude ####

      ## fit the model
      fit_circ <-
        stats::nls(
          response ~ Amp_init * exp(-((((X_values - X_peak) ^ 2
          ) / (2 * X_sig ^ 2) +
            ((Y_values - Y_peak) ^ 2
            ) / (2 * Y_sig ^ 2))))
          ,
          start =
            c(X_peak = X_peak_init,
              Y_peak = Y_peak_init,
              X_sig  = X_sig_init,
              Y_sig  = Y_sig_init
            ),
          data = data,
          trace = verbose,
          control =
            list(maxiter = maxiter,
                 ...)
        )

      ## extract the coefficients and paste together
      fit_coefs <- stats::coef(fit_circ)
      coef_result <-
        c(Amp = Amp_init, fit_coefs)
      res <- as.data.frame(t(coef_result))

      ## create a data.frame of model error stats
      m_e_s <-
        data.frame(
          rss = sum(stats::resid(fit_circ) ^ 2),
          rmse = sqrt((1 / nrow(data)) * sum(stats::resid(fit_circ) ^
                                               2)),
          deviance = stats::deviance(fit_circ),
          AIC = stats::AIC(fit_circ)
        )

      ## construct the output and return
      output <-
        list(
          coefs = res,
          model = fit_circ,
          model_error_stats = m_e_s,
          fit_method =
            c(method = "circular",
              amplitude = "constrained",
              orientation = NA
            )
        )
      attr(output, "gaussplotR") <- "gaussplotR_fit"
      class(output) <- c(class(output), "gaussplotR_fit")
      return(output)

    }

  }

}
