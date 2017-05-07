source("libraries/utils.R")
source("libraries/random-forest-builder.R")

execute <- function(project_name, selection_file, folds = TRUE, file_version = FALSE) {
  datasets_location <- getProjectFolder(project_name)

  if (file_version == FALSE) {
    file_version <- getLastVersion("LDA_fit", datasets_location)
  }

  datasets <- getAllOfVersion("LDA_fit", datasets_location, file_version)

  source(paste(datasets_location, selection_file, sep = "/"))

  runSet <- function(set_filename) {
    dataset <- readRDS(paste(datasets_location, "/", set_filename, sep = ""))

    results <- setupForest(dataset, includes, datasets_location, file_version, folds = FALSE, training_selection = training_selection)

    return(results)
  }

  if (rf_parallel) {
    library(parallel)

    results <- mclapply(datasets, function(set) runSet(set), mc.cores = parallel_cores, mc.silent = parallel_silent)
  } else {
    for (dataset in datasets) {
      runSet(dataset)
    }
  }

  return(results)
}