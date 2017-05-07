FMeasure <- function(precision, recall, beta = 1) {
  return((1 + beta^2) * ((precision * recall) / ((beta^2 * precision) + recall)))
}

getData <- function(data_location, datasets, folds = TRUE) {
  x <- c()
  sd <- c()

  recall <- c()
  precision <- c()
  F1 <- c()
  reduction <- c()

  sizeTrue <- function(x) {
    return(length(x[x == TRUE]))
  }

  # Loop all the data sets
  for (dataset in datasets) {
    dataset_location = paste(data_location, dataset, sep = "/")

    topic_data <- readRDS(dataset_location)

    recall_list <- c()
    precision_list <- c()
    F1_list <- c()
    reduction_list <- c()

    # Loop all the folds
    if (folds) {
      for (j in 1:length(topic_data)) {
        recall_list <- c(recall_list, topic_data[[j]]$recall)
        precision_list <- c(precision_list, topic_data[[j]]$precision)
        F1_list <- c(F1_list, FMeasure(topic_data[[j]]$precision, topic_data[[j]]$recall, 1))
        reduction_list <- c(reduction_list, (topic_data[[j]]$TP + topic_data[[j]]$FP) / (sizeTrue(topic_data[[j]]$base_positives) + sizeTrue(topic_data[[j]]$base_negatives)))
      }

      coeff_names <- topic_data[[1]]$names
    } else {
      recall_list <- c(recall_list, topic_data$recall)
      precision_list <- c(precision_list, topic_data$precision)
      F1_list <- c(F1_list, FMeasure(topic_data$precision, topic_data$recall, 3))
      reduction_list <- c(reduction_list, (topic_data$TP + topic_data$FP) / (sizeTrue(topic_data$base_positives) + sizeTrue(topic_data$base_negatives)))

      coeff_names <- topic_data$names
    }

    x <- c(x, length(coeff_names))
    sd <- c(sd, sd(F1_list))
    recall <- c(recall, mean(recall_list))
    precision <- c(precision, mean(precision_list))
    F1 <- c(F1, mean(F1_list))
    reduction <- c(reduction, 1 - mean(reduction_list))
  }

  saveRDS(data.frame(x = x, sd = sd, recall = recall, precision = precision, F1 = F1, reduction = reduction), paste(data_location, "F1_list.rds", sep = "/"))

  return(data.frame(x = x, sd = sd, recall = recall, precision = precision, F1 = F1, reduction = reduction))
}

plotThis <- function(data_location, file_version, x, y, sd, ...) {
  plot_frame <- data.frame(x = x, y = y, sd = sd)
  other_data <- list(...)

  suppressMessages(library("Hmisc"))

  if (rfa_store == TRUE) {
    png(paste(data_location, "/RF_analysis_plot_", file_version, ".png", sep = ""))
  }

  with(
    data = plot_frame,
    expr = errbar(x, y, y + sd, y - sd, add = F, pch = 1, cap = .015, xlab = '', ylab = '', ylim = c(0,1.1))
  )
  # title(main = "Plot")
  title(ylab = "F1 Score", line = 2.2, cex.lab = 1)
  title(xlab = "# Topics", line = 2.1, cex.lab = 1)

  for (item in other_data) {
    points(plot_frame$x, item)
  }
  # points(plot_frame$x, F1, col = 'red')
  # points(plot_frame$x, precision, col = 'blue')

  if (rfa_store == TRUE) {
    dev.off()
  }
}

prepareData <- function(data_location, datasets, folds = TRUE) {
  for (file in datasets) {
    data <- readRDS(paste(data_location, file, sep = "/"))

    if (folds) {
      for (i in 1:length(data)) {
        data[[i]]$names <- data[[i]]$rf$coefnames
        data[[i]]$rf <- NULL
      }
    } else {
      data$names <- data$rf$coefnames
      data$rf <- NULL
    }

    saveRDS(data, paste(data_location, "/small_", file, sep = ""))
  }
}

setupForestAnalysis <- function(data_location, file_version, folds = TRUE) {
  # DIRECTORY TESTING
  if (!dir.exists(data_location)) {
    stop("Data directory does not exist")
  }

  if (rf_force && dir.exists(data_location)) {
    print("Writing directory already exists")

    print("OVERWRITING")
    overwriteFiles("RF_analysis_plot", data_location, file_version)
  }

  # CHECK FOR EXISTING MINIFIED FILES
  existing_cf_minified <- getAllOfVersion("small_RF_CF", data_location, file_version)
  existing_sf_minified <- getAllOfVersion("small_RF_SF", data_location, file_version)

  # CHECK FOR EXISTING ANALYSIS
  existing_cf_fits <- getAllOfVersion("RF_CF", data_location, file_version)
  existing_sf_fits <- getAllOfVersion("RF_SF", data_location, file_version)
  existing_analysis <- getAllOfVersion("RF_analysis_plot", data_location, file_version)

  if (length(existing_cf_minified) + length(existing_sf_minified) != length(existing_cf_fits) + length(existing_sf_fits)) {
    overwriteFiles("small_RF_CF", data_location, file_version)
    overwriteFiles("small_RF_SF", data_location, file_version)

    prepareData(data_location, existing_cf_fits, folds)
    prepareData(data_location, existing_sf_fits, folds)

    cf_minified <- getAllOfVersion("small_RF_CF", data_location, file_version)
    sf_minified <- getAllOfVersion("small_RF_SF", data_location, file_version)

    datasets <- c(cf_minified, sf_minified)
  } else {
    datasets <- c(existing_cf_minified, existing_sf_minified)
  }

  if (length(existing_analysis) > 0) {
    overwriteFiles("RF_analysis_plot", data_location, file_version)
  }

  data <- getData(data_location, datasets, folds)

  plotThis(data_location, file_version, data$x, data$F1, data$sd)
}