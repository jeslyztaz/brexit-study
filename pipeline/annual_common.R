# Shared paths for the annual robustness pipeline.
# Source from the project root after setup.R. These helpers keep annual
# writes out of the quarterly output paths.

quarterly_artifact_paths <- function() {
  c(
    here("data", "processed", "residence_quarter_panel_full.csv"),
    here("data", "processed", "residence_quarter_regression.csv"),
    here("data", "processed", "residence_quarter_panel.csv"),
    here("data", "processed", "residence_quarter_panel_v0.csv"),
    here("outputs", "main_qoq_coefficients.csv"),
    list.files(here("outputs", "figures"), full.names = TRUE),
    list.files(here("outputs", "tables"), full.names = TRUE)
  )
}

snapshot_quarterly_artifacts <- function() {
  paths <- quarterly_artifact_paths()
  paths <- paths[file.exists(paths)]
  list(
    digest = tools::md5sum(paths),
    figures = sort(list.files(here("outputs", "figures"), full.names = TRUE)),
    tables = sort(list.files(here("outputs", "tables"), full.names = TRUE))
  )
}

assert_snapshot_unchanged <- function(snapshot) {
  missing <- !file.exists(names(snapshot$digest))
  if (any(missing)) {
    stop(
      "Quarterly output disappeared: ",
      paste(names(snapshot$digest)[missing], collapse = ", "),
      call. = FALSE
    )
  }
  now <- tools::md5sum(names(snapshot$digest))
  changed <- names(snapshot$digest)[unname(now) != unname(snapshot$digest)]
  if (length(changed) > 0L) {
    stop(
      "Quarterly output changed: ",
      paste(changed, collapse = ", "),
      call. = FALSE
    )
  }
  figures <- sort(list.files(here("outputs", "figures"), full.names = TRUE))
  tables <- sort(list.files(here("outputs", "tables"), full.names = TRUE))
  if (!identical(figures, snapshot$figures) || !identical(tables, snapshot$tables)) {
    stop(
      "Files were added or removed in a quarterly figure or table directory.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_annual_output <- function(path) {
  full <- normalizePath(path, mustWork = FALSE)
  banned <- normalizePath(
    c(
      here("data", "processed", "residence_quarter_panel_full.csv"),
      here("data", "processed", "residence_quarter_regression.csv"),
      here("data", "processed", "residence_quarter_panel.csv"),
      here("data", "processed", "residence_quarter_panel_v0.csv"),
      here("outputs", "main_qoq_coefficients.csv")
    ),
    mustWork = FALSE
  )
  if (full %in% banned) {
    stop(path, " is a quarterly output.", call. = FALSE)
  }
  figures <- normalizePath(here("outputs", "figures"), mustWork = FALSE)
  tables <- normalizePath(here("outputs", "tables"), mustWork = FALSE)
  inside <- function(dir) {
    identical(full, dir) || startsWith(full, paste0(dir, .Platform$file.sep))
  }
  if (inside(figures) || inside(tables)) {
    stop(path, " is inside a quarterly output directory.", call. = FALSE)
  }
  invisible(full)
}

# Run an annual step, then stop if it changed a quarterly output.
run_guarded <- function(expr) {
  snapshot <- snapshot_quarterly_artifacts()
  on.exit(
    {
      assert_snapshot_unchanged(snapshot)
      message("Quarterly outputs are unchanged.")
    },
    add = TRUE
  )
  force(expr)
}
