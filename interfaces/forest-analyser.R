source("libraries/utils.R")

source("libraries/random-forest-analyser.R")

execute <- function(project_name, folds) {
  datasets_location <- getProjectFolder(project_name)

  file_version <- getLastVersion("RF_.*_fit", datasets_location)

  return(setupForestAnalysis(datasets_location, file_version, folds))
}