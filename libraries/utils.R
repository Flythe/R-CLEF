sizeTrue <- function(x) {
  return(length(x[x == TRUE]))
}

getProjectFolder <- function(project_name) {
  return(paste(data_folder, project_name, sep = "/"))
}

getAllOfVersion <- function(file_pattern, folder, version) {
  patt <- paste("^", file_pattern, ".*_", version, ".rds", sep = "")

  files <- list.files(path = folder, pattern = patt)

  return(files)
}

getLastVersion <- function(file_pattern, folder) {
  if (!dir.exists(folder) || length(list.files(path = folder, pattern = file_pattern)) < 1) {
    return(1)
  }

  files <- list.files(path = folder, pattern = file_pattern)

  if (length(files) < 1) {
    return(1)
  }

  highest_version <- 0

  for (file in files) {
    filename <- unlist(strsplit(file, ".(?=[^.]+$)", perl = TRUE))[1]
    filename_split <- unlist(strsplit(filename, "_", fixed = TRUE))

    version <- as.numeric(filename_split[length(filename_split)])

    if (version > highest_version) {
      highest_version <- version
    }
  }

  return(highest_version)
}

getNewVersion <- function(file_pattern, folder) {
  version <- getLastVersion(file_pattern, folder)

  return(paste(file_pattern, "_", version + 1, sep = ""))
}

askOverwrite <- function(file_pattern, folder) {
  answer <- ask("File already exists, do you want to overwrite? (y/n): ")

  if (answer == "y") {
    overwriteFiles(file_pattern, folder)
  }

  return(getNewVersion(file_pattern, folder))
}

overwriteFiles <- function(file_pattern, folder, file_version = 1) {
  unlink(paste(folder, "/", file_pattern, ".*_", file_version, ".rds", sep = ""))
}

ask <- function(question) {
  if (interactive()) {
    answer <- readline(prompt = question)
  } else {
    cat(question);
    answer <- readLines("stdin", n = 1);
  }

  return(answer)
}