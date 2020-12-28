## Part of the gaussplotR package
## Last updated: 2020-12-26 VBB

############################# compare_gaussian_fits ############################

#' Compare fitted 2D Gaussians and determine the best-fitting model
#'
#' @param fit_objects_list A list of outputs from \code{fit_gaussian_2D()}. See
#' Details for more
#' @param comparison_method One of "rmse", "rss", or "AIC"; what metric should
#' be used to determine the "best-fitting" Gaussian?
#'
#' @details For the argument \code{fit_objects_list}, a list of fitted model
#' objects (output from \code{fit_gaussian_2D()}) can simply be combined via
#' \code{list()}. Naming the list is optional; should you supply names, the
#' output of \code{compare_gaussian_fits()} will refer to specific models by
#' these names.
#'
#' @return A list with the components:
##' \itemize{
##'  \item{"preferred_model"} {A character indicating the name of the preferred
##'  model (or if a named list was not provided, a model number is given in
##'  the order of the original supplied list). }
##'  \item{"comparison_table"} {A data.frame detailing the rss, rmse, deviance,
##'  and AIC of the fitted models. The data.frame is sorted by the
##'  comparison_method that was selected.}
##' }
#'
#' @author Vikram B. Baliga
#'
#' @export
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
#'   ## Fit a variety of different models
#'   gauss_fit_ue <-
#'     fit_gaussian_2D(samp_dat)
#'   gauss_fit_uel <-
#'     fit_gaussian_2D(samp_dat, method = "elliptical_log")
#'   gauss_fit_cir <-
#'     fit_gaussian_2D(samp_dat, method = "circular")
#'
#'   ## Combine the outputs into a list
#'   models_list <-
#'     list(
#'       unconstrained_elliptical = gauss_fit_ue,
#'       unconstrained_elliptical_log = gauss_fit_uel,
#'       circular = gauss_fit_cir
#'     )
#'
#'   ## Compare via rmse
#'   models_compared <-
#'     compare_gaussian_fits(
#'       fit_objects_list = models_list,
#'       comparison_method = "rmse" ## the default
#'     )
#' }


compare_gaussian_fits <- function(fit_objects_list,
                                  comparison_method = "rmse"
                                  ) {

  ## Argument checks
  if (!is.list(fit_objects_list)) {
    stop(
"fit_objects_list must be a list of outputs from fit_gaussian_2D()"
)
  }

  if (is.null(attr(fit_objects_list[[1]], "gaussplotR"))) {
    stop(
"This list does not seem to contain object outputs from fit_gaussian_2D().
Please run fit_gaussian_2D() on your data prior to using this function."
)
  }
  if (!attr(fit_objects_list[[1]], "gaussplotR") == "gaussplotR_fit") {
    stop(
"This list does not seem to contain object outputs from fit_gaussian_2D().
Please run fit_gaussian_2D() on your data prior to using this function."
      )
  }

  ## comparison_method
  if (!is.character(comparison_method)) {
    stop(
      "comparison_method must be one of: 'rmse', 'rss', 'AIC'"
    )
  }
  comparison_method_choices <- c("rmse", "rss", "AIC")
  if (!comparison_method %in% comparison_method_choices) {
    stop(
      "comparison_method must be one of: 'rmse', 'rss', 'AIC'"
    )
  }

  ## Allocate a matrix to fill
  table_mat <- matrix(nrow =
                        length(fit_objects_list),
                      ncol = 4)
  ## Extract all the model_error_stats from each object in the list
  for (i in seq_along(fit_objects_list)) {
    for (j in 1:4) {
      table_mat[i, j] <-
        fit_objects_list[[i]]$model_error_stats[1, j]
    }
  }

  ## Convert to data.frame and label columns
  table <-
    as.data.frame(table_mat)
  colnames(table) <-
    colnames(fit_objects_list[[1]]$model_error_stats)

  ## Label rows with the names of the list if possible
  ## or model_n if not
  if (is.null(names(fit_objects_list))) {
    rownames(table) <-
      paste0("model_",
             seq_along(fit_objects_list))
  } else {
    rownames(table) <-
      names(fit_objects_list)
  }

  ## Sort the table based on the method desired
  if (comparison_method == "rmse"){
    table <-
      table[base::order(table$rmse),]
    preferred_model <-
      rownames(table)[1]
  }

  if (comparison_method == "rss"){
    table <-
      table[base::order(table$rmse),]
    preferred_model <-
      rownames(table)[1]
  }

  if (comparison_method == "AIC"){
    table <-
      table[base::order(table$rmse),]
    preferred_model <-
      rownames(table)[1]
  }

  ## Export
  output <-
    list(
      preferred_model = preferred_model,
      comparison_table = table
    )
  return(output)

}

