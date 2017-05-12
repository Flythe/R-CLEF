source("libraries/utils.R")

probabilitiesForest <- function(rf, dataset, corpus, project_name) {
  suppressMessages(library(tm))

  project_location <- getProjectFolder(project_name)
  file_version <- getLastVersion("clean_corpus", project_location)

  PIDs <- vector("list")
  reviewIDs <- vector("list")

  last_item <- 0

  for (i in 1:length(corpus$content)) {
    if (corpus$content[[i]]$meta$reviewid %in% test_reviewIDs && last_item == 0) {
      last_item <- i - 1
    }

    if (last_item != 0) {
      PIDs <- append(PIDs, list(corpus$content[[i]]$meta$pid))
      reviewIDs <- append(reviewIDs, list(corpus$content[[i]]$meta$reviewid))
    }
  }

  rm(corpus)

  thetas <- dataset$posterior$theta[-(1:last_item),]

  data <- as.data.frame(as.matrix(thetas))

  # Create data frame out of thetas
  input <- data.frame(X = data)

  result <- predict(rf[[1]], input, type = "prob")

  result[[1]]$PIDs <- unlist(PIDs)
  result[[1]]$reviewIDs <- unlist(reviewIDs)

  file_location = paste(project_location, "/", "PROBABILITIES_", file_version, ".rds", sep = "")
  saveRDS(result[[1]], file_location)

  return(result[[1]])
}