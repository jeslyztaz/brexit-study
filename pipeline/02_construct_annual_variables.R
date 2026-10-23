# Residence-year regression file for the annual robustness check.
# Source this from the project root, after pipeline/01_build_annual_panel.R.
# The formulas are in functions/construct_variables_annual.R. This does not
# write the quarterly regression file.

source("setup.R")
source(here("pipeline", "annual_common.R"))
source(here("functions", "construct_variables.R"))
source(here("functions", "construct_variables_annual.R"))

run_guarded({
  panel_file <- here("data", "processed", "residence_annual_panel.csv")
  if (!file.exists(panel_file)) {
    stop("Cannot find ", panel_file, ". Run pipeline/01_build_annual_panel.R first.")
  }

  panel <- readr::read_csv(panel_file, show_col_types = FALSE)
  vars <- construct_variables_annual(panel)
  if ("quarter" %in% names(vars)) {
    stop("Annual regression file still has a quarter column.", call. = FALSE)
  }

  out <- here("data", "processed", "residence_annual_regression.csv")
  assert_annual_output(out)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(vars, out)
  message("Wrote ", nrow(vars), " rows to ", out)
})
