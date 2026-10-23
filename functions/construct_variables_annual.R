# Annual migration ratios for the shift-share robustness check.
#
# Input is the residence-year panel from build_residence_year_panel().
# Natives, migrants, baselines, and leave-one-out ratios use the same
# definitions as construct_variables(). Source that file first.
#
# The quarterly regression's quarter-to-quarter change, d_qoq_*, is the
# change since the previous quarter. Its annual counterpart is the change
# since the previous calendar year:
#   d_annual_* = level[t] - level[t - 1]
# The quarterly file also stores a year-on-year change, d_yoy_*, and lags
# it by four quarters. d_yoy_*_l4 in quarter t is the ratio at t-4 minus
# the ratio at t-8. That is the previous year's one-year change, so on the
# annual panel it is
#   d_annual_*_l1 = level[t - 1] - level[t - 2]
# which is d_annual_* dated one year earlier. It is not the contemporaneous
# annual change. The main annual regression uses d_annual_*, matching
# pipeline/04_main_regressions.R, which uses d_qoq_* and does not put
# d_yoy_*_l4 in the equation. d_annual_*_l1 is stored so that timing is
# still available.
#
# At an annual frequency the one-quarter change and the four-quarter
# change are not two different operations on the annual series. Both are
# one calendar year apart. The annual file therefore does not create a
# second copy of d_annual_* under a d_yoy_ name.
#
# A lag is matched on the calendar year, not on the previous row. If the
# previous year is absent, the change is missing.
#
# Baselines are pooled annual stocks: sum of migrant over sum of native
# inside 2017-2019 or 2010-2015. The annual stock is the average of the
# four quarterly stocks, so this ratio equals the quarterly pooled ratio
# when every baseline year has four quarters. The sums use birthplace
# totals, not ratios and not pop times a birth share.
#
# shift_share_annual_* is the baseline ratio times the one-year change in
# the leave-one-out national ratio. The 2010-2015 columns use that
# baseline and the same leave-one-out change. shift_share_annual_*_l1 is
# the instrument dated one year earlier, the counterpart of
# shift_share_yoy_*_l4.

construct_variables_annual <- function(panel) {
  if (!exists("ratio_or_na", mode = "function") ||
      !exists("native_composition_controls", mode = "function")) {
    stop("Source functions/construct_variables.R first.", call. = FALSE)
  }
  numeric_cols <- c(
    "pop",
    "pop_uk_born", "pop_eu_born", "pop_non_eu_born", "pop_birthplace_unknown",
    "share_birth_uk", "share_birth_eu", "share_birth_non_eu",
    "mean_log_hourpay", "mean_log_hourpay_uk_born"
  )
  required <- c("residence", "year", numeric_cols, "n_pay_uk_born")
  missing <- setdiff(required, names(panel))
  if (length(missing) > 0L) {
    stop(
      "Panel is missing ", paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if ("quarter" %in% names(panel)) {
    stop("Annual variable construction received a quarterly panel.", call. = FALSE)
  }

  panel <- panel %>%
    mutate(
      residence = as.character(residence),
      year = as.integer(year),
      across(all_of(numeric_cols), as.numeric)
    )
  if (anyNA(panel$year)) stop("Year must be non-missing.", call. = FALSE)
  if (anyDuplicated(panel[c("residence", "year")]) > 0L) {
    stop("Duplicate residence-year rows.", call. = FALSE)
  }
  stop_unless_annual_baseline(panel, 2010L, 2015L)
  stop_unless_annual_baseline(panel, 2017L, 2019L)

  local <- panel %>%
    mutate(
      native = pop_uk_born,
      eu = pop_eu_born,
      non_eu = pop_non_eu_born,
      migrant = eu + non_eu,
      ratio_migrant = ratio_or_na(migrant, native),
      ratio_eu = ratio_or_na(eu, native),
      ratio_non_eu = ratio_or_na(non_eu, native)
    ) %>%
    select(
      residence, year,
      native, eu, non_eu, migrant,
      ratio_migrant, ratio_eu, ratio_non_eu
    )
  # The quarterly count checks key on year and quarter. A constant quarter
  # lets those checks run on a residence-year file. It is removed before
  # the annual file is returned.
  panel$quarter <- 1L
  local$quarter <- 1L
  check_group_counts(panel, local)

  national <- local %>%
    group_by(year) %>%
    summarise(
      national_native = sum(native),
      national_eu = sum(eu),
      national_non_eu = sum(non_eu),
      national_migrant = sum(migrant),
      .groups = "drop"
    ) %>%
    mutate(
      national_ratio_migrant = ratio_or_na(national_migrant, national_native),
      national_ratio_eu = ratio_or_na(national_eu, national_native),
      national_ratio_non_eu = ratio_or_na(national_non_eu, national_native)
    )

  wage_cols_in <- c(
    "residence", "year",
    "mean_log_hourpay", "mean_log_hourpay_uk_born", "n_pay_uk_born"
  )
  extra_wage <- intersect(
    c(
      "birthplace_source", "birthplace_scheme",
      "birthplace_source_by_quarter", "birthplace_scheme_by_quarter"
    ),
    names(panel)
  )
  wages <- panel %>% select(all_of(c(wage_cols_in, extra_wage)))

  out <- local %>%
    left_join(national, by = "year") %>%
    left_join(wages, by = c("residence", "year")) %>%
    mutate(
      loo_ratio_migrant = ratio_or_na(
        national_migrant - migrant, national_native - native
      ),
      loo_ratio_eu = ratio_or_na(
        national_eu - eu, national_native - native
      ),
      loo_ratio_non_eu = ratio_or_na(
        national_non_eu - non_eu, national_native - native
      )
    )
  check_aggregated_counts(local, out)
  out$quarter <- NULL
  local$quarter <- NULL

  ratio_cols <- c("ratio_migrant", "ratio_eu", "ratio_non_eu")
  national_cols <- c(
    "national_ratio_migrant", "national_ratio_eu", "national_ratio_non_eu"
  )
  loo_cols <- c("loo_ratio_migrant", "loo_ratio_eu", "loo_ratio_non_eu")
  wage_cols <- c("mean_log_hourpay", "mean_log_hourpay_uk_born")
  out <- difference_from_year(out, ratio_cols, 1L, "d_annual_")
  out <- difference_from_year(out, national_cols, 1L, "d_annual_")
  out <- difference_from_year(out, loo_cols, 1L, "d_annual_")
  out <- difference_from_year(out, wage_cols, 1L, "d_annual_")
  out <- copy_from_year(out, paste0("d_annual_", ratio_cols), 1L, "_l1")

  out <- attach_annual_baseline(out, 2017L, 2019L, "")
  out <- copy_from_year(
    out,
    c(
      "shift_share_annual_migrant", "shift_share_annual_eu",
      "shift_share_annual_non_eu"
    ),
    1L,
    "_l1"
  )
  out <- attach_annual_baseline(out, 2010L, 2015L, "_2010_2015")
  out <- copy_from_year(
    out,
    c(
      "shift_share_annual_migrant_2010_2015",
      "shift_share_annual_eu_2010_2015",
      "shift_share_annual_non_eu_2010_2015"
    ),
    1L,
    "_l1"
  )
  assert_instruments(out)
  for (col in c(ratio_cols, "mean_log_hourpay_uk_born")) {
    assert_year_difference(out, col, paste0("d_annual_", col))
  }
  # The lagged one-year change is the previous year's change, not the
  # change in the change and not the contemporaneous change.
  assert_copied_from_previous_year(
    out, "d_annual_ratio_migrant", "d_annual_ratio_migrant_l1"
  )
  assert_copied_from_previous_year(
    out, "shift_share_annual_migrant", "shift_share_annual_migrant_l1"
  )

  out <- out %>%
    select(
      -national_native, -national_eu, -national_non_eu, -national_migrant,
      -starts_with("loo_ratio_"),
      -starts_with("d_annual_loo_ratio_")
    ) %>%
    mutate(period = as.character(year))

  # native_composition_controls() requires a quarter column. The value is
  # not used in the shares. It is not kept on the annual file.
  control_input <- panel
  control_input$quarter <- 1L
  controls <- native_composition_controls(control_input)
  controls$quarter <- NULL
  n_before <- nrow(out)
  out <- out %>% left_join(controls, by = c("residence", "year"))
  if (nrow(out) != n_before) {
    stop("UK-born controls changed the row count.", call. = FALSE)
  }
  out <- difference_from_year(
    out,
    c(
      "mean_age_uk_born", "share_uk_degree", "share_uk_low_qual",
      "share_uk_asian", "share_uk_black", "share_uk_chinese",
      "share_uk_eth_other"
    ),
    1L,
    "d_annual_"
  )
  assert_no_gap_fill(out)
  if ("quarter" %in% names(out) || any(grepl("loo_ratio", names(out)))) {
    stop("Helper columns remain on the annual regression file.", call. = FALSE)
  }
  out %>% arrange(residence, year)
}

stop_unless_annual_baseline <- function(panel, year_min, year_max) {
  label <- paste0(year_min, "-", year_max)
  years <- seq.int(year_min, year_max)
  missing_years <- setdiff(years, unique(panel$year))
  if (length(missing_years) > 0L) {
    stop(
      "Panel does not cover the ", label, " baseline. Missing years: ",
      paste(missing_years, collapse = ", "),
      ". Rebuild the annual panel so it includes ", label, ".",
      call. = FALSE
    )
  }
  areas <- sort(unique(panel$residence))
  grid <- merge(
    data.frame(residence = areas, stringsAsFactors = FALSE),
    data.frame(year = as.integer(years))
  )
  have_rows <- panel %>%
    filter(year >= year_min, year <= year_max) %>%
    distinct(residence, year)
  missing_rows <- anti_join(grid, have_rows, by = c("residence", "year"))
  if (nrow(missing_rows) > 0L) {
    stop(
      label, " is missing ", nrow(missing_rows),
      " residence-years, including ",
      paste(
        apply(
          head(missing_rows, 5L), 1,
          function(row) paste(row[["residence"]], row[["year"]])
        ),
        collapse = "; "
      ),
      ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

copy_from_year <- function(data, cols, k, suffix) {
  keys <- tibble(
    residence = data$residence,
    .annual_row = seq_len(nrow(data)),
    lag_year = as.integer(data$year) - as.integer(k)
  )
  past <- data %>% select(residence, year, all_of(cols))
  joined <- left_join(
    keys, past,
    by = c("residence", "lag_year" = "year"),
    relationship = "many-to-one"
  )
  joined <- joined %>% arrange(.annual_row)
  if (nrow(joined) != nrow(data) || any(joined$.annual_row != seq_len(nrow(data)))) {
    stop("Lag join changed the annual row order.", call. = FALSE)
  }
  out <- data
  for (col in cols) out[[paste0(col, suffix)]] <- joined[[col]]
  out
}

difference_from_year <- function(data, cols, k, name_prefix) {
  past <- copy_from_year(data, cols, k, "__past")
  for (col in cols) {
    data[[paste0(name_prefix, col)]] <- data[[col]] - past[[paste0(col, "__past")]]
  }
  data
}

attach_annual_baseline <- function(data, year_min, year_max, suffix) {
  label <- paste0(year_min, "-", year_max)
  baseline_rows <- data %>%
    filter(year >= year_min, year <= year_max) %>%
    filter(is.finite(native), is.finite(eu), is.finite(non_eu))
  baseline <- baseline_rows %>%
    group_by(residence) %>%
    summarise(
      baseline_ratio_migrant = ratio_or_na(sum(migrant), sum(native)),
      baseline_ratio_eu = ratio_or_na(sum(eu), sum(native)),
      baseline_ratio_non_eu = ratio_or_na(sum(non_eu), sum(native)),
      .groups = "drop"
    )
  if (any(!is.finite(baseline$baseline_ratio_migrant))) {
    bad <- baseline$residence[!is.finite(baseline$baseline_ratio_migrant)]
    stop(
      "No usable ", label, " native total for: ",
      paste(bad, collapse = ", "), ".",
      call. = FALSE
    )
  }
  ratio_cols <- c(
    "baseline_ratio_migrant", "baseline_ratio_eu", "baseline_ratio_non_eu"
  )
  names(baseline)[match(ratio_cols, names(baseline))] <- paste0(ratio_cols, suffix)
  national_native <- sum(baseline_rows$native)
  n_before <- nrow(data)
  data <- data %>% left_join(baseline, by = "residence")
  if (nrow(data) != n_before) stop("Baseline join changed the row count.", call. = FALSE)
  data[[paste0("baseline_national_ratio_migrant", suffix)]] <-
    ratio_or_na(sum(baseline_rows$migrant), national_native)
  data[[paste0("baseline_national_ratio_eu", suffix)]] <-
    ratio_or_na(sum(baseline_rows$eu), national_native)
  data[[paste0("baseline_national_ratio_non_eu", suffix)]] <-
    ratio_or_na(sum(baseline_rows$non_eu), national_native)
  data[[paste0("shift_share_annual_migrant", suffix)]] <-
    data[[paste0("baseline_ratio_migrant", suffix)]] * data$d_annual_loo_ratio_migrant
  data[[paste0("shift_share_annual_eu", suffix)]] <-
    data[[paste0("baseline_ratio_eu", suffix)]] * data$d_annual_loo_ratio_eu
  data[[paste0("shift_share_annual_non_eu", suffix)]] <-
    data[[paste0("baseline_ratio_non_eu", suffix)]] * data$d_annual_loo_ratio_non_eu
  data
}

# level[t] - level[t - 1], with the previous year brought forward so this
# is not the same join as copy_from_year().
assert_copied_from_previous_year <- function(data, source_col, lag_col) {
  previous <- data[c("residence", "year", source_col)]
  names(previous)[3] <- "previous_value"
  previous$year <- previous$year + 1L
  joined <- left_join(
    data[c("residence", "year", lag_col)],
    previous,
    by = c("residence", "year"),
    relationship = "many-to-one"
  )
  if (nrow(joined) != nrow(data)) {
    stop("Lag check changed the row count.", call. = FALSE)
  }
  values_agree(joined[[lag_col]], joined$previous_value, lag_col)
}

assert_year_difference <- function(data, level_col, change_col) {
  previous <- data[c("residence", "year", level_col)]
  names(previous)[3] <- "previous_level"
  previous$year <- previous$year + 1L
  joined <- left_join(
    data[c("residence", "year", level_col, change_col)],
    previous,
    by = c("residence", "year"),
    relationship = "many-to-one"
  )
  if (nrow(joined) != nrow(data)) {
    stop("Difference check changed the row count.", call. = FALSE)
  }
  values_agree(
    joined[[change_col]],
    joined[[level_col]] - joined$previous_level,
    change_col
  )
}

assert_instruments <- function(data) {
  specs <- list(
    list("national_migrant", "migrant", "baseline_ratio_migrant", "shift_share_annual_migrant"),
    list("national_eu", "eu", "baseline_ratio_eu", "shift_share_annual_eu"),
    list("national_non_eu", "non_eu", "baseline_ratio_non_eu", "shift_share_annual_non_eu"),
    list("national_migrant", "migrant", "baseline_ratio_migrant_2010_2015", "shift_share_annual_migrant_2010_2015"),
    list("national_eu", "eu", "baseline_ratio_eu_2010_2015", "shift_share_annual_eu_2010_2015"),
    list("national_non_eu", "non_eu", "baseline_ratio_non_eu_2010_2015", "shift_share_annual_non_eu_2010_2015")
  )
  for (spec in specs) {
    loo <- ratio_or_na(
      data[[spec[[1]]]] - data[[spec[[2]]]],
      data$national_native - data$native
    )
    previous <- tibble(
      residence = data$residence,
      year = data$year + 1L,
      previous_loo = loo
    )
    joined <- left_join(
      tibble(
        residence = data$residence,
        year = data$year,
        loo = loo,
        baseline = data[[spec[[3]]]],
        shift = data[[spec[[4]]]]
      ),
      previous,
      by = c("residence", "year"),
      relationship = "many-to-one"
    )
    values_agree(
      joined$shift,
      joined$baseline * (joined$loo - joined$previous_loo),
      spec[[4]]
    )
  }
  invisible(TRUE)
}

assert_no_gap_fill <- function(data) {
  change_cols <- grep("^(d_annual_|shift_share_)", names(data), value = TRUE)
  keys <- paste(data$residence, data$year)
  previous_exists <- paste(data$residence, as.integer(data$year) - 1L) %in% keys
  for (col in change_cols) {
    if (any(!previous_exists & is.finite(data[[col]]))) {
      stop(col, " bridges a missing calendar year.", call. = FALSE)
    }
  }
  invisible(TRUE)
}
