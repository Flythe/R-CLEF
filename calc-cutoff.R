workspace <- "~/workspace/R"

sizeTrue <- function(x) {
  return(length(x[x == TRUE]))
}

dataset <- readRDS("data/clef/small_RF_CF_fit_k75_1.rds")

probs <- dataset[[1]]$rf_probs
probs$base_true <- dataset[[1]]$base_positives

ord_probs <- probs[order(-probs$include),]

cutoffs <- seq(0.01,1,0.01)

found <- c()

for (cutoff in cutoffs) {
  found <- c(found, sizeTrue(ord_probs[ord_probs$include > cutoff,]$base_true))
}

plot(cutoffs, found)