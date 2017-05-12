# http://appliedpredictivemodeling.com/blog/2013/12/8/28rmc2lv96h8fw8700zm4nl50busep

# https://cran.r-project.org/web/packages/caret/caret.pdf
suppressMessages(library(caret))
suppressMessages(library(pROC))
suppressMessages(library(randomForest))
suppressMessages(library(doMC))
source("libraries/utils.R")

singleFoldForest <- function(data, includes, training_selection, datasets_location, file_version, set_num) {
  thetas <- data$posterior$theta

  # Create include/exclude list
  input <- formatInput(thetas, includes)

  # Determine train and test portion of data
  inTrain <- rep(0, length(thetas[,1]))
  inTrain[training_selection] <- 1

  # Split dataset into train and test set
  training <- input[inTrain == 1,]
  testing <- input[inTrain == 0,]

  # Train forest
  rf <- trainForest(training, set_num)

  # Get output metrics
  results <- getMetrics(rf, testing)

  return(results)
}

crossFoldForest <- function(data, includes, datasets_location, file_version) {
  # Create include/exclude list
  print("Formatting inputs")
  input <- formatInput(data, includes)

  # Create training folds
  print("Creating folds")
  train_folds <- createDataPartition(y = input$Class, times = rf_folds, list = TRUE)

  temp_file_list <- vector("list", length = length(train_folds))

  # Fit folds
  for (i in 1:length(train_folds)) {
    # Create test and train set for this fold
    # testing <- input[train_folds[[i]],]
    # training <- input[-train_folds[[i]],]

    # Train forest
    rf <- trainForest(input)

    # Store output metrics into list
    temp_file_list[[i]] <- tempfile(fileext = ".rds")

    saveRDS(getMetrics(rf, testing), temp_file_list[[i]])

    rm(rf)
  }

  return(temp_file_list)
}

trainForest <- function(training) {
  print("Training forest")

  # Count the number of includes in the training portion
  nmin <- sum(training$Class == "include")

  # Create control variable, set to cross validate
  ctrl <- trainControl(method = "cv",
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary,
                       verboseIter = TRUE)

  mtry <- getMtry(length(training[1,]) - 1)

  tunegrid <- expand.grid(.mtry = mtry)

  # Parallel RFs
  if (rf_parallel) {
    registerDoMC(cores = parallel_cores)
  }

  training_cleaned <- training[ , !(names(training) %in% c('PID', 'reviewID'))]

  rf <- train(Class ~ ., data = training_cleaned,
              method = "rf",
              ntree = rf_ntree,
              tuneGrid = tunegrid,
              metric = "ROC",
              trControl = ctrl,
              strata = training$Class,
              sampsize = rep(nmin, 2))

  return(rf)
}

setupForest <- function(input_matrix, includes, data_location, file_version, folds = FALSE, training_selection = FALSE) {
  # DIRECTORY TESTING
  if (!dir.exists(data_location)) {
    stop("Data directory does not exist")
  }

  file_pattern <- "RF_SF"

  if (folds == TRUE) {
    file_pattern <- "RF_CF"
  }

  if (rf_force && dir.exists(data_location)) {
    print("Writing directory already exists")

    print("OVERWRITING")
    overwriteFiles(paste(file_pattern, "_fit_k", dataset$numberOfTopics, sep = ""), data_location, file_version)
  }

  # RETRIEVE STORED MODEL FIT
  fit_location <- getAllOfVersion(paste(file_pattern, "_fit_k", dataset$numberOfTopics, sep = ""), data_location, file_version)

  if (length(fit_location) > 0) {
    fit_location <- paste(data_location, fit_location, sep = "/")

    print(paste("Reading forest fit from RDS: ", fit_location))

    fit_data <- readRDS(fit_location)
  }

  # FIT NEW MODEL
  if (!exists("fit_data")) {
    if (folds == TRUE) {
      print("Fitting a new CROSS FOLD random forest")

      file_list <- crossFoldForest(input_matrix, includes, data_location, file_version)

      fit_data <- vector("list", length = length(file_list))

      for (i in 1:length(file_list)) {
        fit_data[[i]] <- readRDS(file_list[[i]])

        unlink(file_list[[i]])
      }
    } else {
      print("Fitting a new SINGLE random forest")

      fit_data <- singleFoldForest(input_matrix, includes, training_selection, data_location, file_version, dataset$numberOfTopics)
    }

    if (rf_store) {
      file_location = paste(data_location, "/", file_pattern, "_fit_k", dataset$numberOfTopics, "_", file_version, ".rds", sep = "")

      print(paste("Writing forest fit to RDS: ", file_location, sep = ""))

      saveRDS(fit_data, file_location)
    }
  }

  return(fit_data)
}

formatInput <- function(input_matrix, includes) {
  data <- as.data.frame(as.matrix(input_matrix))

  # Create include/exclude factor
  y <- vector(length = length(data[,1]))
  y[] <- 'exclude'
  y[includes] <- 'include'

  y <- factor(y)

  # Create data frame out of thetas
  input <- data.frame(X = data)
  # Append the factor
  input$Class <- y

  return(input)
}

getMtry <- function(set_num) {
  if (set_num < 50) {
    mtry <- seq(1, set_num, 1)
  } else {
    mtry <- seq(10, set_num, 10)

    # Append set_num if not divisible by 10
    if (set_num %% 10 != 0) {
      mtry <- c(mtry, set_num)
    }
  }

  return(mtry)
}

getMetricsForFile <- function(project_name, file_version, set_num, fold_type = "SF") {
  project_folder <- getProjectFolder(project_name)
  pattern <- paste("RF_", fold_type, "_fit_k", set_num, sep = "")
  file <- getAllOfVersion(pattern, project_folder, file_version)

  return(readRDS(paste(project_folder, file, sep = "/")))
}

getMetrics <- function(rf, test_set) {
  # prob_test_set <- test_set[, !names(test_set) %in% c("PID", "reviewID")]
  #
  # rf_probs <- predict(rf, prob_test_set, type = "prob")
  #
  # ROC <- roc(response = test_set$Class,
  #            predictor = rf_probs[,1],
  #            levels = rev(levels(test_set$Class)))
  #
  # metadata <- data.frame(PID = test_set$PID, reviewID = test_set$reviewID)
  #
  # base_positives <- test_set$Class == 'include'
  # base_negatives <- test_set$Class == 'exclude'
  #
  # test_positives <- rf_probs$include >= 0.5
  # test_negatives <- rf_probs$exclude > 0.5
  #
  # TP <- sizeTrue(test_positives & base_positives)
  # TN <- sizeTrue(test_negatives & base_negatives)
  #
  # FP <- sizeTrue(test_positives & base_negatives)
  # FN <- sizeTrue(test_negatives & base_positives)
  #
  # recall <- TP / sizeTrue(base_positives) # most important, has to be close to 1
  #
  # accuracy <- (TP + TN) / (TP + TN + FP + FN) # not interesting
  # precision <- TP / (TP + FP) # not interesting
  #
  # sensitivity <- TP / sizeTrue(base_positives) # most important, has to be close to 1 == recall
  # specificity <- TN / sizeTrue(base_negatives) # not interesting
  #
  # F1 <- 2 * ((precision * recall) / (precision + recall)) # not interesting, will be terrible
  #
  # return(list(rf = rf, rf_probs = rf_probs, ROC = ROC, base_positives = base_positives, base_negatives = base_negatives,
  #            test_positives = test_positives, test_negatives = test_negatives, TP = TP, TN = TN, FP = FP, FN = FN, recall = recall,
  #            accuracy = accuracy, precision = precision, sensitivity = sensitivity, specificity = specificity, F1 = F1, metadata = metadata))

  return(list(rf = rf))
}