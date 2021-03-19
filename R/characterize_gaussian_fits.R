## Part of the gaussplotR package
## Last updated: 2020-12-27 VBB

########################### characterize_gaussian_fits #########################

#' Characterize the orientation of fitted 2D-Gaussians
#'
#' The orientation and partial correlations of Gaussian data are analyzed
#' according to Levitt et al. 1994 and Priebe et al. 2003. Features include
#' computation of partial correlations between response variables and
#' independent and diagonally-tuned predictions, along with Z-difference
#' scoring.
#'
#' @param fit_objects_list A list of outputs from \code{fit_gaussian_2D()}. See
#'   Details for more. This is the preferred input object for this function.
#' @param data A data.frame that contains the raw data (generally rectilinearly
#'   gridded data, but this is not a strict requirement). Columns must be named
#'   \code{"X_values"}, \code{"Y_values"} and \code{"response"}. See
#'   \code{fit_gaussian_2D()} for details.
#' @param constrain_amplitude Default FALSE; should the amplitude of the
#'   Gaussian be set to the maximum value of the \code{"response"} variable
#'   (\code{TRUE}), or should the amplitude fitted by \code{stats::nls()}
#'   (\code{FALSE})? See \code{fit_gaussian_2D()} for details.
#' @param ... Additional arguments that can be passed to
#'   \code{fit_gaussian_2D()} if data are supplied.
#'
#' @details
#' This function accepts either a list of objects output from
#' \code{fit_gaussian_2D()} (preferred) or a data.frame that contains the raw
#' data.
#'
#' The supplied fit_objects_list must be a list that contains objects returned
#' by \code{fit_gaussian_2D()}. This list must contain exactly three models. All
#' three models must have been run using \code{method = "elliptical_log"}. The
#' models must be: 1) one in which orientation is unconstrained, 2) one in which
#' orientation is constrained to Q = 0 (i.e. a diagonally-oriented Gaussian),
#' and 3) one in which orientation is constrained to Q = -1 (i.e. a
#' horizontally-oriented Gaussian). See this function's Examples for guidance.
#'
#' Should raw data be provided instead of the fit_objects_list, the
#' \code{characterize_gaussian_fits()} runs \code{fit_gaussian_2D()} internally.
#' This is generally not recommended, as difficulties in fitting models via
#' \code{stats::nls()} are more easily troubleshot by the optional arguments in
#' \code{fit_gaussian_2D()}. Nevertheless, supplying raw data instead of a list
#' of fitted models is feasible, though your mileage may vary.
#'
#' @return
#' A list with the following:
#' \itemize{
##'  \item{"model_comparison"} {A model comparison output (i.e. what is produced
##'  by \code{compare_gaussian_fits()}), which indicates the relative preference
##'  of each of the three models.}
##'  \item{"Q_table"} {A data.frame that provides information on the value of Q
##'  from the best-fitting model, along with the 5-95% confidence intervals of
##'  this estimate.}
##'  \item{"r_i"} {A numeric, the correlation of the data with the independent
##'  (Q = -1) prediction.}
##'  \item{"r_s"} {A numeric, the correlation of the data with the diagonally-
##'  oriented (Q = 0) prediction.}
##'  \item{"r_is"} {A numeric, the correlation between the independent
##'  (Q = -1) prediction and the the diagonally-oriented (Q = 0) prediction.}
##'  \item{"R_indp"} {A numeric, partial correlation of the response variable
##'  with the independent (Q = -1) prediction.}
##'  \item{"R_diag"} {A numeric, partial correlation of the response variable
##'  with the diagonally-oriented (Q = 0) prediction.}
##'  \item{"ZF_indp"} {A numeric, the Fisher Z-transform of the R_indp
##'  coefficient. See Winship et al. 2006 for details.}
##'  \item{"ZF_diag"} {A numeric, the Fisher Z-transform of the R_diag
##'  coefficient. See Winship et al. 2006 for details.}
##'  \item{"Z_diff"} {A numeric, the Z-difference between ZF_indp and ZF_diag.
##'  See Winship et al. 2006 for details.}
##'
##' }
#'
#' @export
#'
#' @references
#' Levitt JB, Kiper DC, Movshon JA. Receptive fields and functional architecture
#' of macaque V2. J Neurophysiol. 1994 71:2517–2542.
#'
#' Priebe NJ, Cassanello CR, Lisberger SG. The neural representation of speed in
#' macaque area MT/V5. J Neurosci. 2003 Jul 2;23(13):5650-61. doi:
#' 10.1523/JNEUROSCI.23-13-05650.2003.
#'
#' Winship IR, Crowder N, Wylie DRW. Quantitative reassessment of speed tuning
#' in the accessory optic system and pretectum of pigeons. J Neurophysiol. 2006
#' 95(1):546-551. doi: 10.1152/jn.00921.2005
#'
#' @author Vikram B. Baliga
#'
#' @examples
#' if (interactive()) {
#'   library(gaussplotR)
#'
#'   ## Load the sample data set
#'   data(gaussplot_sample_data)
#'
#'   ## The raw data we'd like to use are in columns 1:3
#'   samp_dat <-
#'     gaussplot_sample_data[,1:3]
#'
#'   ## Fit the three required models
#'   gauss_fit_uncn <-
#'     fit_gaussian_2D(
#'       samp_dat,
#'       method = "elliptical_log",
#'       constrain_amplitude = FALSE,
#'       constrain_orientation = "unconstrained"
#'     )
#'
#'   gauss_fit_diag <-
#'     fit_gaussian_2D(
#'       samp_dat,
#'       method = "elliptical_log",
#'       constrain_amplitude = FALSE,
#'       constrain_orientation = 0
#'     )
#'
#'   gauss_fit_indp <-
#'     fit_gaussian_2D(
#'       samp_dat,
#'       method = "elliptical_log",
#'       constrain_amplitude = FALSE,
#'       constrain_orientation = -1
#'     )
#'
#'   ## Combine the outputs into a list
#'   models_list <-
#'     list(
#'       gauss_fit_uncn,
#'       gauss_fit_diag,
#'       gauss_fit_indp
#'     )
#'
#'   ## Now characterize
#'   out <-
#'     characterize_gaussian_fits(models_list)
#'   out
#'
#'   ## Alternatively, the raw data itself can be supplied.
#'   ## This is less preferred, as fitting of models may fail
#'   ## internally.
#'   out2 <-
#'     characterize_gaussian_fits(data = samp_dat)
#'
#'   ## This produces the same output, assuming models are fit without error
#'   identical(out, out2)
#' }

characterize_gaussian_fits <- function(fit_objects_list = NULL,
                                       data = NULL,
                                       constrain_amplitude = FALSE,
                                       ...) {

  #### Check if fit_objects_list has been populated ####
  if(is.null(fit_objects_list)) {

    ## If data hasn't been supplied either, stop and alert.
    if(is.null(data)) {
      stop(
"Neither 'fit_objects_list' nor 'data' has been supplied.
Please supply one of these input options."
      )
    }

    ## If raw data are present, attempt to fit each of the three necessary
    ## models
    gauss_fit_uncn <-
      fit_gaussian_2D(
        data,
        method = "elliptical_log",
        constrain_amplitude = constrain_amplitude,
        constrain_orientation = "unconstrained",
        ...
      )

    gauss_fit_diag <-
      fit_gaussian_2D(
        data,
        method = "elliptical_log",
        constrain_amplitude = constrain_amplitude,
        constrain_orientation = 0,
        ...
      )

    gauss_fit_indp <-
      fit_gaussian_2D(
        data,
        method = "elliptical_log",
        constrain_amplitude = constrain_amplitude,
        constrain_orientation = -1,
        ...
      )

    ## Create the fit_objects_list
    fit_objects_list <-
      list(
        gauss_fit_uncn,
        gauss_fit_diag,
        gauss_fit_indp
      )
  }

  #### Check contents of fit_objects_list ####
  ## fit_objects_list should not be longer than 3 models
  if (length(fit_objects_list) != 3) {
    stop(
"'fit_objects_list' must be a list with exactly three items.
Please see the Details section of this function's Help file."
    )
  }

  ## Check that fit_objects_list has one of each type of needed model
  mod1_type <- fit_objects_list[[1]]$fit_method
  mod2_type <- fit_objects_list[[2]]$fit_method
  mod3_type <- fit_objects_list[[3]]$fit_method
  all_mod_types <-
    data.frame(t(data.frame(mod1_type, mod2_type, mod3_type)))

  ## Check that they are all of method elliptical_log
  if (any(all_mod_types$method != "elliptical_log")) {
    stop(
"All models in 'fit_objects_list' must be run using the 'elliptical_log' method"
    )
  }

  ## Make sure amplitudes are all constrained or all unconstrained
  if (length(unique(all_mod_types$amplitude)) > 1) {
    stop(
"All models must treat amplitude identically (either constrained or not)."
    )
  }

  ## Figure out which model is which
  mod1_Q <- fit_objects_list[[1]]$coefs$Q
  mod2_Q <- fit_objects_list[[2]]$coefs$Q
  mod3_Q <- fit_objects_list[[3]]$coefs$Q
  all_mod_Qs <-
    data.frame(rbind(mod1_Q, mod2_Q, mod3_Q))
  colnames(all_mod_Qs) <- "Q"
  all_mod_Qs$mod_num <- 1:3

  ## Identity of the unconstrained model
  uncn_id <- which(all_mod_types$orientation=='unconstrained')
  if (length(uncn_id) == 0) {
    stop(
"No supplied models had orientation set to 'unconstrained'.
Please revise 'fit_objects_list'."
    )
  }

  constrained_rows <- which(all_mod_types$orientation=='constrained')
  if (length(constrained_rows) == 0) {
    stop(
"No supplied models had orientation constrained to either 0 or -1.
Please revise 'fit_objects_list'."
    )
  }

  ## Remove the uncn_id row from all_mod_Qs. This is because an unconstrained
  ## model may result in Q = 0 or -1, so we want to avoid confusion.
  subset_mod_Qs <- all_mod_Qs[-uncn_id,]

  ## Identities of the diagonal and indpendent models
  diag_id <- constrained_rows[which(subset_mod_Qs$Q == 0)]
  indp_id <- constrained_rows[which(subset_mod_Qs$Q == -1)]

  ## Check that diag_id and indp_id are not integer(0)
  if (length(diag_id) == 0) {
    stop(
"No supplied models had Q = 0.
Please revise 'fit_objects_list'."
    )
  }
  if (length(indp_id) == 0) {
    stop(
"No supplied models had Q = -1.
Please revise 'fit_objects_list'."
    )
  }

  ## Re-order and re-name fit objects list for sake of uniformity
  ## This way, no matter what the input is, everything is standarized
  ## from here on out
  all_models <-
    list(
      mod_uncn = fit_objects_list[[uncn_id]],
      mod_diag = fit_objects_list[[diag_id]],
      mod_indp = fit_objects_list[[indp_id]]
    )

  #### Which is the preferred model
  comp <- compare_gaussian_fits(all_models)
  pref_num <- which(names(all_models) == comp$preferred_model)
  pref_mod <- all_models[[pref_num]]

  #### Get the CI of Q ####
  Q_lower_CI <- as.numeric(stats::coef(pref_mod$model)[2] - 1.96 *
    (summary(pref_mod$model)$coefficients)[2, 2])
  Q_upper_CI <- as.numeric(stats::coef(pref_mod$model)[2] + 1.96 *
    (summary(pref_mod$model)$coefficients)[2, 2])

  Q_table <-
    c(Q = pref_mod$coefs$Q,
      Q_lower_CI = Q_lower_CI,
      Q_upper_CI = Q_upper_CI)

  #### Generate predictions from each model ####
  uncn_preds <- stats::predict(all_models$mod_uncn$model)
  diag_preds <- stats::predict(all_models$mod_diag$model)
  indp_preds <- stats::predict(all_models$mod_indp$model)

  ## get the original data
  ## if data were supplied, use that
  if (!is.null(data)){
    original <- data$response
  } else {
    ## otherwise, infer it from the models
    original <-
      all_models$mod_uncn$model$m$lhs()
  }

  all_preds <-
    data.frame(
      original = original,
      uncn_preds = uncn_preds,
      diag_preds = diag_preds,
      indp_preds = indp_preds
    )

  all_cors <- stats::cor(all_preds)

  #### Compute r statistics ####
  r_i <- all_cors[1,4]
  r_s <- all_cors[1,3]
  r_is <- all_cors[3,4]

  R_ind <-
    (r_i - (r_s) * (r_is)) / sqrt((1 - (r_s ^ 2)) * (1 - (r_is ^ 2)))

  R_spd <-
    (r_s - (r_i) * (r_is)) / sqrt((1 - (r_i ^ 2)) * (1 - (r_is ^ 2)))

  ## ZF scores
  ZF_ind <-
    0.5 * log((1 + R_ind) / (1 - R_ind))
  ZF_spd <-
    0.5 * log((1 + R_spd) / (1 - R_spd))

  ## Zdiff
  Z_diff <-
    (ZF_ind - ZF_spd) / ((1 / (length(original) - 3)) +
                           (1 / (length(original) - 3))) ^ 0.5


  #### Put it all together and return ####
  output <-
    list(
      model_comparison = comp,
      Q_table = Q_table,
      r_i = r_i,
      r_s = r_s,
      r_is = r_is,
      R_indp = R_ind,
      R_diag = R_spd,
      ZF_indp = ZF_ind,
      ZF_diag = ZF_spd,
      Z_diff = Z_diff
    )

  return(output)

}
