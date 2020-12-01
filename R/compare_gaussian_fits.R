## Part of the gaussplotR package
## Last updated: 2020-11-30 VBB

############################# compare_gaussian_fits ############################


compare_gaussian_fits <- function(fit_objects_list,
                                  comparison_method = "rmse") {

  ## Allocate a matrix to fill
  table_mat <- matrix(nrow = length(fit_objects_list),
                      ncol = 4)
  ## Extract all the model_error_stats from each object in the list
  for (i in seq_along(fit_objects_list)) {
    for (j in 1:4) {
      table_mat[i, j] <- fit_objects_list[[i]]$model_error_stats[1, j]
    }
  }

  ## Convert to data.frame and label columns
  table <- as.data.frame(table_mat)
  colnames(table) <- colnames(fit_objects_list[[1]]$model_error_stats)

  ## Label rows with the names of the list if possible or model_n if not
  if (is.null(names(fit_objects_list))) {
    rownames(table) <-
      paste0("model_", seq_along(fit_objects_list))
  } else {
    rownames(table) <- names(fit_objects_list)
  }

  ## Sort the table based on the method desired
  if (comparison_method == "rmse"){
    table <- table[base::order(table$rmse),]
    preferred_model <- rownames(table)[1]
  }

  if (comparison_method == "rss"){
    table <- table[base::order(table$rmse),]
    preferred_model <- rownames(table)[1]
  }

  if (comparison_method == "AIC"){
    table <- table[base::order(table$rmse),]
    preferred_model <- rownames(table)[1]
  }

  ## Export
  output <-
    list(
      preferred_model = preferred_model,
      comparison_table = table
    )
  return(output)

}

