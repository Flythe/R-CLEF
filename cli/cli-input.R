args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("No input arguments given")
}

print("Taking cli arguments.")

workspace <- args[1]

print(paste("Changing to workspace:", workspace))

setwd(workspace)

# Load the config
if (!exists("configLoaded")) {
  source("config.R")
}