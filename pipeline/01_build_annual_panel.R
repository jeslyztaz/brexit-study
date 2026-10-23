# Residence-year panel for the annual robustness check.
# Source this from the project root. The quarterly pipeline is unchanged.
# Population stocks are the average of the four quarterly stocks. A calendar
# year is included only when Q1, Q2, Q3, and Q4 are all present.
# `years` is left unset, so every complete year from 2006 is built.

source("setup.R")
source(here("pipeline", "annual_common.R"))
source(here("functions", "data_cleaners.R"))
source(here("functions", "lfs_panel.R"))
source(here("functions", "lfs_panel_annual.R"))

run_guarded({
  panel <- build_residence_year_panel(
    here("data", "quarterly_lfs_raw"),
    from_year = 2006L
  )
  coverage <- attr(panel, "year_coverage")
  checks <- attr(panel, "aggregation_checks")
  if (is.null(coverage) || is.null(checks)) {
    stop("Annual panel is missing its coverage or aggregation checks.", call. = FALSE)
  }
  if ("quarter" %in% names(panel)) {
    stop("Annual panel still has a quarter column.", call. = FALSE)
  }

  panel_path <- here("data", "processed", "residence_annual_panel.csv")
  coverage_path <- here("outputs", "metadata", "annual_year_coverage.csv")
  checks_path <- here("outputs", "metadata", "aggregation_checks.csv")
  for (path in c(panel_path, coverage_path, checks_path)) {
    assert_annual_output(path)
  }
  dir.create(dirname(coverage_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(panel_path), recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(panel, panel_path)
  readr::write_csv(coverage, coverage_path)
  readr::write_csv(checks, checks_path)
  message("Wrote ", nrow(panel), " rows to ", panel_path)
  excluded <- coverage[!coverage$included, ]
  if (nrow(excluded) == 0L) {
    message("No calendar year was excluded.")
  } else {
    message(
      "Excluded years: ",
      paste(sprintf("%s (%s)", excluded$year, excluded$reason), collapse = "; "),
      "."
    )
  }
  summarise_native_pay_sample_annual(panel)
})
