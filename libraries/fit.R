################
# RETURN VALUE #
################

# System date
# LDAData
# usedTerms
# termFrequency
# tokensPerDoc
# posterior
#   phi
#   theta
# control
#   alpha
#   delta
#   burnin
#   iter
#   keep
# numberOfTopics

LDASimulation <- function(corpus, project_name, file_version, k, alpha, beta, burnin, iter, thin, keep, multiple = FALSE) {
  # Convert tm corpus to document list, both LDA and topicmodels methods should use the TM to parse the original text
  # This line below can then convert for the LDA while topicmodels uses the TermDocumentMatrix
  # https://stackoverflow.com/questions/21148049/r-topic-modeling-lda-command-lexicalize-giving-unexpected-results
  LDADocuments <- as.list(data.frame(text = unlist(sapply(corpus, `[`, "content")), stringsAsFactors = F))[[1]]

  # Get terms by splitting the documents on whitespace
  usedTerms <- table(unlist(strsplit(LDADocuments, "[[:space:]]+")))

  # Lexicalize the documents
  LDADocuments <- lexicalize(LDADocuments, vocab = names(usedTerms))

  # Get the sum of terms per document
  tokensPerDoc <- sapply(LDADocuments, function(x) sum(x[2, ]))

  control = list(alpha = alpha, delta = beta, burnin = burnin, iter = iter, thin = thin)

  print("LDASimulation setup done.")

  # Run LDA
  LDAData <- lda.collapsed.gibbs.sampler(documents = LDADocuments,
                                         K = k,
                                         vocab = names(usedTerms),
                                         num.iterations = control$iter,
                                         alpha = control$alpha,
                                         eta = control$delta,
                                         burnin = control$burnin,
                                         compute.log.likelihood = TRUE)

  print("LDASimulation run done.")

  # Document to topic distribution estimate
  # Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
  theta <- t(apply(LDAData$document_sums + alpha, 2, function(x) x/sum(x)))
  # Topic to term distribution estimate
  # Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
  phi <- t(apply(t(LDAData$topics) + beta, 2, function(x) x/sum(x)))

  print("LDASimulation posteriors done.")

  runData = list(LDAData = LDAData,
                 usedTerms = names(usedTerms),
                 termFrequency = as.integer(usedTerms),
                 tokensPerDoc = tokensPerDoc,
                 phi = phi,
                 theta = theta,
                 control = control,
                 numberOfTopics = k)

  if (store == TRUE) {
    filename <- paste(data_folder, "/", project_name, "/LDA_fit_k", k, "_a", round(alpha, 2), "_b", round(beta, 2), "_", file_version, ".rds", sep = "")

    saveRDS(runData, filename)

    print("LDASimulation data stored.")
  }

  return(runData)
}

TmLDASimulation <- function(corpus, project_name, file_version, k, alpha, beta, burnin, iter, thin, keep, multiple = FALSE) {
  print("building ID list")

  pids <- vector("list", length(corpus))
  review_ids <- vector("list", length(corpus))

  for (i in 1:length(corpus$content)) {
    pids[i] <- as.numeric(corpus$content[[i]]$meta$pid)
    review_ids[i] <- as.character(corpus$content[[i]]$meta$reviewid)
  }

  print("building dtm")

  dtm <- DocumentTermMatrix(corpus)

  if (multiple) {
    control = list(alpha = alpha, delta = beta, iter = iter, keep = 1, nstart = 3, best = FALSE, seed = list(123234, 890, 112))
  } else {
    control = list(alpha = alpha, delta = beta, burnin = burnin, iter = iter, keep = keep, thin = thin)
  }

  print("TmLDASimulation setup done.")

  LDAData = LDA(dtm, k = k, method = "Gibbs", control = control)

  print("TmLDASimulation run done.")

  if (multiple) {
    return(LDAData)
  }

  # Solution to translate between topicmodels and LDAVis: http://www.r-bloggers.com/a-link-between-topicmodels-lda-and-ldavis/
  # Document to topic distribution estimate
  # Matrix where each column contains the probability distribution over topics for 1 document (1 cell is a probability of topic x for document y)
  theta <- posterior(LDAData)$topics %>% as.matrix
  # Topic to term distribution estimate
  # Matrix where each column contains the probability distribution over words for 1 topic (1 cell is a probability of word x for topic y)
  phi <- posterior(LDAData)$terms %>% as.matrix

  print("TmLDASimulation posteriors done.")

  usedTerms <- colnames(phi)

  # tokensPerDoc <- rowSums(as.matrix(dtm))

  print("TmLDASimulation tokensPerDoc done.")

  # Memory issue on cloud machine, do this locally
  #termFrequency <- colSums(as.matrix(dtm))

  print("TmLDASimulation termFrequency done.")

  runData = list(
    date = Sys.time(),
    LDAData = LDAData,
    usedTerms = usedTerms,
    dtm = dtm,
    pids = pids,
    review_ids = review_ids,
    #tokensPerDoc = tokensPerDoc,
    posterior = list(phi = phi, theta = theta),
    #termFrequency = termFrequency,
    control = control,
    numberOfTopics = k
  )

  if (fit_store == TRUE) {
    filename <- paste(data_folder, "/", project_name, "/LDA_fit_k", k, "_a", round(alpha, 2), "_b", round(beta, 2), "_", file_version, ".rds", sep = "")

    saveRDS(runData, filename)

    print("TmLDASimulation data stored.")
  }

  return(runData)
}

setupFitting <- function(corpus, project_name, file_version, k, alpha, beta, burnin, iter, thin, keep, multiple = FALSE) {
  suppressMessages(library(lda))
  suppressMessages(library(LDAvis))
  suppressMessages(library(topicmodels))
  suppressMessages(library(tm))
  suppressMessages(library(dplyr))
  suppressMessages(library(stringi))

  data_location <- paste(data_folder, project_name, sep = "/")

  # DIRECTORY TESTING
  if (!dir.exists(data_folder)) {
    stop("Data directory does not exist")
  }

  if (fit_force && dir.exists(data_location)) {
    print("Writing directory already exists")

    print("OVERWRITING")
    overwriteFiles(paste("LDA_fit_k", k, sep = ""), data_location, file_version)
  }

  # RETRIEVE STORED MODEL FIT
  fit_location <- getAllOfVersion(paste("LDA_fit_k", k, sep = ""), data_location, file_version)

  if (length(fit_location) > 0) {
    fit_location <- paste(data_location, fit_location, sep = "/")

    print(paste("Reading fit from RDS: ", fit_location))

    fit_data <- readRDS(fit_location)
  }

  # FIT NEW MODEL
  if (!exists("fit_data")) {
    fit_data <- TmLDASimulation(corpus, project_name, file_version, k, alpha, beta, burnin, iter, thin, keep, multiple)
  }

  return(fit_data)
}

visualise <- function(runData, outputFolder) {
  runData$termFrequency = colSums(as.matrix(runData$dtm))

  # Create JSON format data
  json.data <- createJSON(phi = runData$posterior$phi,
                          theta = runData$posterior$theta,
                          doc.length = runData$tokensPerDoc,
                          vocab = runData$usedTerms,
                          term.frequency = runData$termFrequency)

  # Remove the directory (throws error otherwise)
  unlink(outputFolder, recursive = TRUE)

  # Create browser scripts
  serVis(json.data, out.dir = outputFolder, open.browser = FALSE)
}