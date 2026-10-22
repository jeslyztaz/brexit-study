# Residence-quarter panel from data/quarterly_lfs_raw, 2014 onward.
# Source this from the project root. It stops if a file in that window is
# not a calendar quarter (jm, aj, js, od). Earlier files are left unread.

source("setup.R")
source(here("functions", "data_cleaners.R"))
source(here("functions", "lfs_panel.R"))

panel <- build_residence_quarter_panel(
  here("data", "quarterly_lfs_raw"),
  from_year = 2006L
)

out <- here("data", "processed", "residence_quarter_panel_full.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(panel, out)
message("Wrote ", nrow(panel), " rows to ", out)
