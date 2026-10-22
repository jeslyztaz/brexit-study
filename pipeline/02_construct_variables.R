# Residence-quarter regression dataset.
# Source this from the project root, after pipeline/01_prep_data.R.
# The formulas live in functions/construct_variables.R.
# The file is not restricted to a sample. UK-born age, qualification, and
# ethnicity controls are included. The regression chooses the endogenous
# ratio, the instrument, and the sample.

source("setup.R")
source(here("functions", "construct_variables.R"))

panel_file <- here("data", "processed", "residence_quarter_panel_full.csv")
if (!file.exists(panel_file)) {
  stop("Cannot find ", panel_file, ". Run pipeline/01_prep_data.R first.")
}

panel <- readr::read_csv(panel_file, show_col_types = FALSE)
vars <- construct_variables(panel)

out <- here("data", "processed", "residence_quarter_regression.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(vars, out)
message("Wrote ", nrow(vars), " rows to ", out)
