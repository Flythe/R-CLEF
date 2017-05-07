#!/usr/bin/env Rscript
# https://stackoverflow.com/questions/18306362/run-r-script-from-command-line

# CALL
# ./cli-workflow.R ~/workspace/R test_corp articles_sysrev_test

source("cli-input.R")
source("libraries/utils.R")

if (length(args) < 3) {
  stop("Corpus name or CSV name not provided")
}

# CLEANING ----------------------------------------------

project_name <- args[2]
csv_name <- args[3]

# OVERWRITE CONFIG DEFAULTS
clean_force <- FALSE

source("libraries/preprocessing.R")

# Pre-process the corpus
clean_corpus <- setupPreprocessing(project_name, csv_name)

if (workflow_run_to == "cleaning") {
  stop()
}

# FITTING -----------------------------------------------

# Overwrite the defaults
#fit_ks <- seq(2, 4, 1)

project_location <- getProjectFolder(project_name)
file_version <- getLastVersion("clean_corpus", project_location)

source("interfaces/fit.R")

library(parallel)

if (fit_parallel) {
  datasets <- mclapply(
    fit_ks,
    function(k) setupFitting(clean_corpus, project_name, file_version, k, fit_divider/k, fit_beta, fit_burnin, fit_iter, fit_thin, fit_keep),
    mc.cores = parallel_cores,
    mc.silent = parallel_silent
  )
} else {
  datasets <- list()

  for (k in fit_ks) {
    datasets <- append(datasets, list(setupFitting(clean_corpus, project_name, file_version, k, fit_divider/k, fit_beta, fit_burnin, fit_iter, fit_thin, fit_keep)))
  }
}

if (workflow_run_to == "fitting") {
  stop()
}

# RANDOM FOREST -----------------------------------------

source("libraries/random-forest-builder.R")

selection_file <- "rf_selection.R"

# Load 'includes' variable
source(paste(project_location, selection_file, sep = "/"))

runSet <- function(dataset) {
  results <- setupForest(dataset, includes, project_location, file_version, rf_fold, training_selection)

  return(results)
}

#if (rf_parallel) {
#  results <- mclapply(datasets, function(set) runSet(set), mc.cores = parallel_cores, mc.silent = parallel_silent)
#} else {
results <- list()

for (dataset in datasets) {
  results <- append(results, list(runSet(dataset)))
}
#}

# RANDOM FOREST ANALYSER --------------------------------

source("libraries/random-forest-analyser.R")

results <- setupForestAnalysis(project_location, file_version, rf_fold)

# ANALYSIS -------------------------------

#TODO

#source("libraries/")