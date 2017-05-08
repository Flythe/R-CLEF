data <- readRDS("data/clef/RF_CF_fit_k75_1.rds")

subset <- vector("list", length = 6)

data <- Filter(Negate(is.null), data)

for (i in 1:length(data)) {
  subset[[i]] <- data[[i]][c("rf_probs", "metadata")]

  setlength <- length(subset[[i]]$metadata)
  half <- setlength / 2

  subset[[i]]$pids <- as.character(unlist(subset[[i]]$metadata[1:half]))
  subset[[i]]$reviewids <- as.character(unlist(subset[[i]]$metadata[(half + 1):setlength]))
}

remove(data)

for (i in 1:length(subset)) {
  N <- length(subset[[i]]$reviewids)

  subset[[i]]$submit <- data.frame(TOPIC_ID = character(N),
                                   INTERACTION = character(N),
                                   PID = character(N),
                                   RANK = character(N),
                                   SCORE = character(N),
                                   RUN_ID = character(N),
                                   stringsAsFactors = FALSE)

  subset[[i]]$submit$TOPIC_ID <- subset[[i]]$reviewids
  # subset[[i]]$submit$INTERACTION <- vector("list", length = length(subset[[i]]$rf_probs$include))
  subset[[i]]$submit$PID <- subset[[i]]$pids
  subset[[i]]$submit$SCORE <- subset[[i]]$rf_probs$include
  subset[[i]]$submit$RUN_ID <- rep.int(i, length(subset[[i]]$rf_probs$include))

  subset[[i]]$submit <- subset[[i]]$submit[order(-subset[[i]]$submit$SCORE),]

  subset[[i]]$submit$RANK <- seq(1:N)

  write.table(subset[[i]]$submit, paste("data/clef/run", i, sep = ""), quote = FALSE)
}