configLoaded <- TRUE

# GENERIC
corpus_folder <- "originals"
data_folder <- "data"
parallel_cores <- 2
parallel_silent <- FALSE
workflow_run_to <- "forest"

# CLEAN
clean_force <- FALSE
clean_store <- TRUE
clean_gram <- FALSE
clean_stem <- FALSE
clean_overwrite <- TRUE
clean_grams <- 1:2
clean_extra_stopwords <- c()

# FIT
fit_force <- FALSE
fit_k <- 25 # Number of topics
fit_ks <- c(75)
fit_iter <- 500 # Iterations
fit_burnin <- 200
fit_thin <- 200
fit_nstart <- 3
fit_keep <- 10
fit_divider <- 50
fit_alpha <- fit_k / fit_divider
fit_beta <- 0.01
fit_seed <- list(123234, 890, 112, 239234, 1947)
fit_store <- TRUE
fit_parallel <- FALSE

# RANDOM FOREST
rf_force <- TRUE
rf_store <- TRUE
rf_fold <- TRUE
rf_folds <- 1:2
rf_parallel <- TRUE

rfa_store <- TRUE