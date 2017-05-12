source("libraries/utils.R")
source("libraries/random-forest-builder.R")

executeForest <- function(dataset, project_name, corpus) {
  suppressMessages(library(tm))

  project_location <- getProjectFolder(project_name)
  file_version <- getLastVersion("clean_corpus", project_location)

  includes <- vector("list")

  last_item <- 0

  for (i in 1:length(corpus$content)) {
    if (corpus$content[[i]]$meta$included) {
      includes <- c(includes, i)
    }

    if (corpus$content[[i]]$meta$reviewid %in% test_reviewIDs && last_item == 0) {
      last_item <- i - 1
    }
  }

  rm(corpus)

  includes <- unlist(includes)

  thetas <- dataset$posterior$theta[1:last_item,]

  rm(dataset)

  result <- setupForest(thetas, includes, project_location, file_version, rf_fold, training_selection)

  return(result)
}