# setup.R

library(dplyr)    # Data manipulation
library(tidyr)    # Reshaping
library(readr)    # Reading and writing tables
library(ggplot2)  # Plotting
library(fixest)   # Econometric regressions
library(here)     # Project-relative file paths

# Path under outputs/. The parent directory is created when a file is saved.
output_file <- function(...) {
  path <- here("outputs", ...)
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  path
}
