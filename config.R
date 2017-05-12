configLoaded <- TRUE

# GENERIC
corpus_folder <- "originals"
data_folder <- "data"
parallel_silent <- FALSE
workflow_run_to <- "forest"

# CLEAN
clean_force <- FALSE
clean_store <- TRUE
clean_gram <- FALSE
clean_stem <- FALSE
clean_overwrite <- TRUE
clean_grams <- 1:2
clean_extra_stopwords <- c()#c("big", "data", "big_data") #c("big", "data", "ieee", "discussion", "conclusion", "introduction", "methods", "psycinfo_database",
                          # "rights_reserved", "record_apa", "journal_abstract", "apa_rights", "psycinfo", "reserved_journal",
                          # "conclusionadvancement", "apa", "reserved", "rights_journal",
                          # "test", "case", "proven_probable", "perform", "result", "detect", "present", "reveal", "year",
                          # "show", "report", "disseminate", "technique", "major", "increase", "include", "challenge", "total",
                          # "special", "count", "significantly_higher")

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
rf_folds <- 1
rf_parallel <- TRUE
rf_ntree <- 800

rfa_store <- TRUE

test_reviewIDs <- c('CD007431','CD008803','CD008782','CD009647','CD009135','CD008760','CD010775','CD009519','CD009372','CD010276','CD009551','CD012019','CD008081','CD009185','CD010339','CD010653','CD010542','CD010896','CD010023','CD010772','CD011145','CD010705','CD010633','CD010173','CD009786','CD010386','CD010783','CD010860','CD009579','CD009925')

if (fit_parallel || rf_parallel) {
  suppressMessages(library(parallel))

  parallel_cores <- detectCores()
}
