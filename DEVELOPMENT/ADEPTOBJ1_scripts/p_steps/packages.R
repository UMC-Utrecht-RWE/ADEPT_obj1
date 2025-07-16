# ==========================
# Load Required Packages
# ==========================

# Function to check if each package is installed.
# If not, it installs the package and then loads it.
# Uses suppressPackageStartupMessages to keep the console clean.

load_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

# List of all required packages
required_packages <- c("data.table", "lubridate", "RcppAlgos", "stringr", "readxl", "readr", "fs", "crayon", "AdhereR")

# Load each package
invisible(lapply(required_packages, load_package))
