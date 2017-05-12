data <- readRDS("data/clef-run/PROBABILITIES_1.rds")

subset <- vector("list", length = length(test_reviewIDs))

# data <- Filter(Negate(is.null), data)

for (i in 1:length(test_reviewIDs)) {
  review_id = test_reviewIDs[[i]]

  subset[[i]] <- data[data$reviewIDs == review_id,]

  N <- length(subset[[i]]$reviewIDs)

  subset[[i]]$submit <- data.frame(TOPIC_ID = character(N),
                                   INTERACTION = character(N),
                                   PID = character(N),
                                   RANK = character(N),
                                   SCORE = character(N),
                                   RUN_ID = character(N),
                                   stringsAsFactors = FALSE)

  subset[[i]]$submit$TOPIC_ID <- subset[[i]]$reviewIDs
  subset[[i]]$submit$INTERACTION <- rep.int("NF", length(subset[[i]]$include))
  subset[[i]]$submit$PID <- subset[[i]]$PIDs
  subset[[i]]$submit$SCORE <- subset[[i]]$include
  subset[[i]]$submit$RUN_ID <- rep.int(i, length(subset[[i]]$include))

  subset[[i]]$submit <- subset[[i]]$submit[order(-subset[[i]]$submit$SCORE),]

  subset[[i]]$submit$RANK <- seq(1:N)

  write.table(subset[[i]]$submit, paste("data/clef-run/run", review_id, sep = ""), quote = FALSE, row.names = FALSE)
}