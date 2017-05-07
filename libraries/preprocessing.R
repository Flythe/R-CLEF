if (!exists("configLoaded")) source("config.R")

###############################
# TM_MAP compatible functions #
###############################

# Removes the whitespace from beginning and end of document content
trimWhitespace <- function(document) {
  PlainTextDocument(trimws(document))
}

removeReturns <- function(document) {
  PlainTextDocument(str_replace_all(document, "\n", " "))
}

# Stem completes document
stemComplete <- function(document, dict) {
  # Fix the stem completion (https://stackoverflow.com/questions/25206049/stemcompletion-is-not-working, answer by cdxsza)
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(document), " ")), dictionary = dict, type = "prevalent"), sep = "", collapse = " ")))
}

# Removes dashes between words
removeDash <- function(document) {
  PlainTextDocument(str_replace_all(document, "-", " "))
}

replaceUnderscore <- function(document) {
  PlainTextDocument(str_replace_all(document, "XZXZX", "_"))
}

# Removes words longer than 26 characters from the string
removeLong <- function(document) {
  PlainTextDocument(stripWhitespace(str_replace_all(document, "[:alpha:]{26,}", "")))
}

###############################

# Creates N-grams, counts term occurance, removes terms with low count
# Uses tm corpus as input
createNGrams <- function(original_corpus) {
  # Convert back to document list (function uses Corpus as input for interoperability)
  original_text <- as.list(data.frame(text = paste("", paste(unlist(sapply(original_corpus, `[`, "content")), "", sep = " "), sep = " "), stringsAsFactors = F))[[1]]

  # Create ngrams
  ngram <- dfm(original_text, ngrams = clean_grams, verbose = FALSE)

  # Get the term counts
  column_counts <- colSums(ngram)

  # Create list we want to remove
  sparseTerms <- column_counts < 15 | stri_length(names(column_counts)) <= 6
  # Remove unwanted terms
  cleaned_terms <- column_counts[!sparseTerms]
  cleaned_terms <- cleaned_terms[!names(cleaned_terms) %in% clean_extra_stopwords]

  # Create list of terms without dashes (for searching purposes)
  noDash <- gsub("_", " ", names(cleaned_terms))

#   library(openxlsx)
#
#   data <- read.xlsx('data/termine-data.xlsx', colNames = FALSE)
#
#   data[,2] <- as.numeric(data[,2])
#
#   cutoff <- 20
#
#   cut_data <- data[data[,2] > cutoff,1]
#
#   cut_data <- cut_data[order(-nchar(cut_data), cut_data)]

  # noDash <- cut_data
  # cleaned_terms <- gsub(" ", "XZXZX", cut_data)

  # Store the original text in a variable
  gram_ready_text <- original_text
  # For each term (or compound term) check the original text and merge any occurences by adding a _
  # Stupid paste construction to concat a space to the beginning and end of the search string to avoid merging compound terms
  for(i in 1:length(noDash)) {
    gram_ready_text <- gsub(paste("", paste(as.String(noDash[i]), "", sep = " "), sep = " "), paste("", paste(names(cleaned_terms[i]), "", sep = " "), sep = " "), gram_ready_text, ignore.case = TRUE)
    # gram_ready_text <- gsub(paste("", paste(noDash[i], "", sep = " "), sep = " "), paste("", paste(cleaned_terms[i], "", sep = " "), sep = " "), gram_ready_text, ignore.case = TRUE)
  }

  # Convert into corpus again
  gram_ready_text <- Corpus(VectorSource(gram_ready_text))

  result <- tm_map(gram_ready_text, content_transformer(trimWhitespace))

  # Returns the corpus with _ added to N-gram compound terms
  return(gram_ready_text)
}

# Stem the text and stem complete the text (with most prevalent term)
# Uses tm corpus as input
stemText <- function(original_corpus) {
  # Stem the document
  stemmed_corpus <- tm_map(original_corpus, content_transformer(stemDocument))
  # Make sure there is no extra whitespace because of processing steps
  stemmed_corpus <- tm_map(stemmed_corpus, content_transformer(stripWhitespace))

  # Apply the stem completion
  stem_completed_corpus <- tm_map(stemmed_corpus, content_transformer(stemComplete), original_corpus)

  # Return the corpus
  return(stem_completed_corpus)
}

# Remove stopwords
# Uses tm corpus as input
removeStopWords <- function(original_corpus, extra) {
  # Remove stopwords
  result <- tm_map(original_corpus, content_transformer(removeWords), c(stopwords("SMART"), extra))

  result <- tm_map(result, content_transformer(stripWhitespace))
  result <- tm_map(result, content_transformer(trimWhitespace))

  return(result)
}

# Remove words that are weirdly long (e.g. > 30 letters, while max. size in English is approx. 26)
# Uses tm corpus as input
removeOverlyLongWords <- function(original_corpus) {
  result <- tm_map(original_corpus, content_transformer(removeLong))

  result <- tm_map(result, content_transformer(stripWhitespace))
  result <- tm_map(result, content_transformer(trimWhitespace))

  return(result)
}

# Remove special characters and extra whitespace at beginning and end of document
# Uses tm corpus as input
removeSpecialCharacters <- function(original_corpus) {
  # Remove dash (-), punctuation, and numbers
  result <- tm_map(original_corpus, content_transformer(removeDash))
  result <- tm_map(result, content_transformer(removePunctuation))
  result <- tm_map(result, content_transformer(removeNumbers))

  result <- tm_map(result, content_transformer(replaceUnderscore))

  result <- tm_map(result, content_transformer(stripWhitespace))
  result <- tm_map(result, content_transformer(trimWhitespace))

  return(result)
}

removeReturnsFromCorpus <- function(original_corpus) {
  tm_map(original_corpus, content_transformer(removeReturns))
}

cleanMyText <- function(original_text, stem, gram) {
  # Lowercase everything
  print("Cleaning: to lower case.")
  result <- char_tolower(original_text)

  # Convert to corpus
  print("Cleaning: convert to corpus.")
  result <- VCorpus(VectorSource(result))

  result <- removeReturnsFromCorpus(result)

  if (gram) {
    # Create compound terms
    print("Cleaning: create N-grams.")
    result <- createNGrams(result)
  }

  # Remove special characters
  print("Cleaning: remove special characters.")
  result <- removeSpecialCharacters(result)

  # Remove stopwords
  print("Cleaning: remove stopwords pass 1.")
  result <- removeStopWords(result, clean_extra_stopwords)

  if (stem) {
    # Stem the corpus
    print("Cleaning: stemming corpus.")
    result <- stemText(result)

    # Remove stopwords after stemming
    print("Cleaning: remove stopwords pass 2.")
    result <- removeStopWords(result, clean_extra_stopwords)
  }

  # If any weirdly long words are left, remove them
  # print("Cleaning: remove long words.")
  # result <- removeOverlyLongWords(result)

  # Return as corpus
  return(result)
}

addIDs <- function(corpus) {
  for (i in 1:length(corpus)) {
    corpus[[i]]$meta$id = i
  }

  return(corpus)
}

readCSV <- function(csv_location) {
  if (!file.exists(csv_location)) {
    stop("Corpus file not found")
  }

  text <- as.matrix(read.csv(file = csv_location, header = FALSE, quote = "")[1])

  return(text)
}

runPreprocessing <- function(csv_location, stem = TRUE, gram = TRUE) {
  # Load libraries
  suppressMessages(library(tm))
  suppressMessages(library(SnowballC))
  suppressMessages(library(quanteda))
  suppressMessages(library(stringi))
  suppressMessages(library(stringr))
  suppressMessages(library(methods))

  documents <- readCSV(csv_location)

  # Start!
  print("Starting cleaning process.")
  clean_corpus <- cleanMyText(documents, stem, gram)

  print("Cleaning: add back document ids.")
  clean_corpus <- addIDs(clean_corpus)

  return(clean_corpus)
}

setupPreprocessing <- function(project_name, csv_name) {
  data_folder <- getProjectFolder(project_name)

  csv_location <- paste(corpus_folder, "/", csv_name, ".csv", sep = "")

  corpus_filename <- "clean_corpus_1"

  # run_version <- getLastVersion("clean_corpus", data_folder)

  # DIRECTORY TESTING
  if (!file.exists(csv_location)) {
    stop(paste("CSV input file not found at:", csv_location))
  }

  if (!dir.exists(data_folder)) {
    stop("Data directory does not exist")
  }

  if (dir.exists(data_folder)) {
    corpus_filename <- paste("clean_corpus_", getLastVersion("clean_corpus", data_folder), sep = "")
  }

  if (clean_force && dir.exists(data_folder)) {
    print("Writing directory already exists")

    if (clean_overwrite == TRUE) {
      print("OVERWRITING")
      overwriteFiles("clean_corpus", data_folder)
    } else {
      corpus_filename <- getNewVersion("clean_corpus", data_folder)
    }
  }

  # RETRIEVE STORED CLEANED CORPUS
  corpus_location <- paste(data_folder, "/", corpus_filename, ".rds", sep = "")

  if (!clean_force && file.exists(corpus_location)) {
    print(paste("Reading corpus from RDS: ", corpus_location))

    clean_corpus <- readRDS(corpus_location)
  }

  # CLEAN NEW CORPUS
  if (!exists("clean_corpus")) {
    print(paste("Reading corpus from CSV: ", csv_location))

    clean_corpus <- runPreprocessing(csv_location, clean_stem, clean_gram)

    if (clean_store == TRUE) {
      print(paste("Writing to: ", corpus_location))

      if (!dir.exists(data_folder)) {
        dir.create(data_folder)
      }

      print(paste("Writing corpus to RDS: ", corpus_location))
      saveRDS(clean_corpus, corpus_location)
    }
  }

  return(clean_corpus)
}
