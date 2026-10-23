# Shared paths for the annual robustness pipeline.
# Source from the project root after setup.R. These helpers keep annual
# writes out of the quarterly output paths.

quarterly_output_dirs <- function() {
  c(
    here("outputs", "descriptive", "quarterly"),
    here("outputs", "regressions", "quarterly"),
    here("outputs", "figures"),
    here("outputs", "tables")
  )
}

files_under <- function(dir) {
  if (!dir.exists(dir)) {
    return(character())
  }
  sort(list.files(dir, recursive = TRUE, full.names = TRUE))
}

quarterly_artifact_paths <- function() {
  c(
    here("data", "processed", "residence_quarter_panel_full.csv"),
    here("data", "processed", "residence_quarter_regression.csv"),
    here("data", "processed", "residence_quarter_panel.csv"),
    here("data", "processed", "residence_quarter_panel_v0.csv"),
    here("outputs", "main_qoq_coefficients.csv"),
    unlist(lapply(quarterly_output_dirs(), files_under), use.names = FALSE)
  )
}

snapshot_quarterly_artifacts <- function() {
  paths <- quarterly_artifact_paths()
  paths <- paths[file.exists(paths)]
  list(
    digest = tools::md5sum(paths),
    directories = lapply(quarterly_output_dirs(), files_under)
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
  current <- lapply(quarterly_output_dirs(), files_under)
  if (!identical(current, snapshot$directories)) {
    stop(
      "Files were added or removed in a quarterly output directory.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# normalizePath leaves a missing relative path unchanged, so a new file
# under an existing directory would not match an absolute banned prefix.
absolute_path <- function(path) {
  existing <- path
  pending <- character()
  repeat {
    if (file.exists(existing) || dir.exists(existing)) break
    parent <- dirname(existing)
    if (identical(parent, existing)) break
    pending <- c(basename(existing), pending)
    existing <- parent
  }
  root <- normalizePath(existing, mustWork = FALSE)
  if (length(pending) == 0L) {
    return(root)
  }
  file.path(root, paste(pending, collapse = .Platform$file.sep))
}

assert_annual_output <- function(path) {
  full <- absolute_path(path)
  banned <- vapply(
    c(
      here("data", "processed", "residence_quarter_panel_full.csv"),
      here("data", "processed", "residence_quarter_regression.csv"),
      here("data", "processed", "residence_quarter_panel.csv"),
      here("data", "processed", "residence_quarter_panel_v0.csv"),
      here("outputs", "regressions", "quarterly", "coefficients", "main_qoq_coefficients.csv"),
      here("outputs", "main_qoq_coefficients.csv")
    ),
    absolute_path,
    character(1)
  )
  if (full %in% banned) {
    stop(path, " is a quarterly output.", call. = FALSE)
  }
  inside <- function(dir) {
    root <- absolute_path(dir)
    identical(full, root) || startsWith(full, paste0(root, .Platform$file.sep))
  }
  if (any(vapply(quarterly_output_dirs(), inside, logical(1)))) {
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
