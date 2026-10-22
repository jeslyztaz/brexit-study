# Migration ratios for the shift-share regression.
#
# Input is the residence-quarter panel from build_residence_quarter_panel(),
# which must cover 2010 Q1 through 2015 Q4 and 2017 Q1 through 2019 Q4.
# Natives are UK-born. Migrants are EU-born plus non-EU-born. Regions are
# the URESMC residence areas.
#
# native, eu, and non_eu are the person-weighted totals pop_uk_born,
# pop_eu_born, and pop_non_eu_born. Those totals leave out missing
# birthplace. pop still includes it, and the birth shares are among people
# with a known birthplace, so pop times a share is larger than the group
# total when any birthplace is missing. That scale cancels in a regional
# ratio. It does not cancel in a sum across areas or across quarters, so
# national ratios, leave-one-out ratios, baselines, and the shift-share
# use the totals directly.
#
# The lagged year-on-year change, d_yoy_ratio_*_l4, is the migration term
# Jaeger, Ruist and Stuhler put on the right-hand side. In quarter t it is
# the ratio four quarters earlier minus the ratio eight quarters earlier.
#
# The shift-share is that area's baseline ratio times the leave-one-out
# change in the national ratio. Leaving the area out matters because these
# residence areas, especially London, are large shares of the national
# total. The inclusive national change would partly be the area's own
# migration. qoq and yoy instruments are both stored. The regression
# chooses which one to use. Rows are not restricted to a sample.
#
# baseline_ratio_* and shift_share_* use the 2017-2019 pooled stock. The
# same construction for 2010-2015 is stored on the columns that end in
# _2010_2015. The leave-one-out change is the same in both.
#
# UK-born age, qualification, and ethnicity controls are read from the
# panel columns that end in _uk_born. Chinese is its own share. White is
# the omitted ethnicity. Higher education and A-level are the omitted
# middle of the qualification split.
#
# Age, qualification, and ethnicity controls are also stored as
# quarter-to-quarter changes. A QoQ wage equation uses those changes, with
# the same one-quarter gap as d_qoq_mean_log_hourpay. The levels stay for a
# levels specification. White is omitted, so it has no change column.

construct_variables <- function(panel) {
  numeric_cols <- c(
    "pop",
    "pop_uk_born", "pop_eu_born", "pop_non_eu_born", "pop_birthplace_unknown",
    "share_birth_uk", "share_birth_eu", "share_birth_non_eu",
    "mean_log_hourpay", "mean_log_hourpay_uk_born"
  )
  required <- c("residence", "year", "quarter", numeric_cols, "n_pay_uk_born")
  missing <- setdiff(required, names(panel))
  if (length(missing) > 0L) {
    stop(
      "Panel is missing ", paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  panel <- panel %>%
    mutate(
      residence = as.character(residence),
      year = as.integer(year),
      quarter = as.integer(quarter),
      across(all_of(numeric_cols), as.numeric)
    )
  if (anyNA(panel$year) || anyNA(panel$quarter)) {
    stop("Year and quarter must be non-missing.", call. = FALSE)
  }
  if (anyDuplicated(panel[c("residence", "year", "quarter")]) > 0L) {
    stop("Duplicate residence-year-quarter rows.", call. = FALSE)
  }

  stop_unless_baseline_window(panel, 2010L, 2015L)
  stop_unless_baseline_window(panel, 2017L, 2019L)

  # An empty group is already 0 on the panel. Missing birthplace is not
  # inside these totals, so it is not allocated across the three groups.
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
      residence, year, quarter,
      native, eu, non_eu, migrant,
      ratio_migrant, ratio_eu, ratio_non_eu
    )
  check_group_counts(panel, local)

  national <- local %>%
    group_by(year, quarter) %>%
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

  # n_pay_uk_born is the unweighted count behind mean_log_hourpay_uk_born.
  # It is not a regressor and it does not drop cells. birthplace_source
  # records the UKDA column used in that quarter.
  wage_cols_in <- c(
    "residence", "year", "quarter",
    "mean_log_hourpay", "mean_log_hourpay_uk_born", "n_pay_uk_born"
  )
  extra_wage <- intersect(c("birthplace_source", "birthplace_scheme"), names(panel))
  wages <- panel %>% select(all_of(c(wage_cols_in, extra_wage)))

  out <- local %>%
    left_join(national, by = c("year", "quarter")) %>%
    left_join(wages, by = c("residence", "year", "quarter")) %>%
    mutate(
      # Other areas only. Subtracting this area from the national total
      # is the leave-one-out ratio.
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

  ratio_cols <- c("ratio_migrant", "ratio_eu", "ratio_non_eu")
  national_cols <- c(
    "national_ratio_migrant", "national_ratio_eu", "national_ratio_non_eu"
  )
  loo_cols <- c("loo_ratio_migrant", "loo_ratio_eu", "loo_ratio_non_eu")
  wage_cols <- c("mean_log_hourpay", "mean_log_hourpay_uk_born")
  out <- difference_from(out, ratio_cols, 1L, "d_qoq_")
  out <- difference_from(out, ratio_cols, 4L, "d_yoy_")
  out <- difference_from(out, national_cols, 1L, "d_qoq_")
  out <- difference_from(out, national_cols, 4L, "d_yoy_")
  out <- difference_from(out, loo_cols, 1L, "d_qoq_")
  out <- difference_from(out, loo_cols, 4L, "d_yoy_")
  out <- difference_from(out, wage_cols, 1L, "d_qoq_")
  out <- difference_from(out, wage_cols, 4L, "d_yoy_")
  # Lag the year-on-year change, rather than differencing it again.
  yoy_cols <- paste0("d_yoy_", ratio_cols)
  out <- copy_from(out, yoy_cols, 4L, "_l4")

  baseline_rows <- out %>%
    filter(year >= 2017L, year <= 2019L) %>%
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
      "No usable 2017-2019 native total for: ",
      paste(bad, collapse = ", "), ".",
      call. = FALSE
    )
  }

  # Outside mutate. out still has the quarterly national_native column,
  # and mutate() would use that column as the denominator.
  baseline_native_total <- sum(baseline_rows$native)
  out <- out %>% left_join(baseline, by = "residence")
  out$baseline_national_ratio_migrant <- ratio_or_na(
    sum(baseline_rows$migrant), baseline_native_total
  )
  out$baseline_national_ratio_eu <- ratio_or_na(
    sum(baseline_rows$eu), baseline_native_total
  )
  out$baseline_national_ratio_non_eu <- ratio_or_na(
    sum(baseline_rows$non_eu), baseline_native_total
  )
  values_agree(
    out$baseline_national_ratio_migrant,
    rep(ratio_or_na(sum(baseline_rows$migrant), baseline_native_total), nrow(out)),
    "2017-2019 national migrant baseline"
  )
  values_agree(
    out$baseline_national_ratio_eu,
    rep(ratio_or_na(sum(baseline_rows$eu), baseline_native_total), nrow(out)),
    "2017-2019 national EU baseline"
  )
  values_agree(
    out$baseline_national_ratio_non_eu,
    rep(ratio_or_na(sum(baseline_rows$non_eu), baseline_native_total), nrow(out)),
    "2017-2019 national non-EU baseline"
  )
  out <- out %>%
    mutate(
      shift_share_qoq_migrant =
        baseline_ratio_migrant * d_qoq_loo_ratio_migrant,
      shift_share_qoq_eu = baseline_ratio_eu * d_qoq_loo_ratio_eu,
      shift_share_qoq_non_eu =
        baseline_ratio_non_eu * d_qoq_loo_ratio_non_eu,
      shift_share_yoy_migrant =
        baseline_ratio_migrant * d_yoy_loo_ratio_migrant,
      shift_share_yoy_eu = baseline_ratio_eu * d_yoy_loo_ratio_eu,
      shift_share_yoy_non_eu =
        baseline_ratio_non_eu * d_yoy_loo_ratio_non_eu,
      period = paste0(year, "Q", quarter)
    )
  out <- copy_from(
    out,
    c("shift_share_yoy_migrant", "shift_share_yoy_eu", "shift_share_yoy_non_eu"),
    4L,
    "_l4"
  )
  # Same pooled stock and shift-share, with the 2010-2015 baseline ratio.
  # The leave-one-out changes above are not rebuilt.
  out <- attach_baseline_shift_share(out, 2010L, 2015L, "_2010_2015")
  out <- copy_from(
    out,
    c(
      "shift_share_yoy_migrant_2010_2015",
      "shift_share_yoy_eu_2010_2015",
      "shift_share_yoy_non_eu_2010_2015"
    ),
    4L,
    "_l4"
  )
  out <- out %>%
    select(
      -national_native, -national_eu, -national_non_eu, -national_migrant,
      -starts_with("loo_ratio_"),
      -starts_with("d_qoq_loo_ratio_"),
      -starts_with("d_yoy_loo_ratio_")
    )

  controls <- native_composition_controls(panel)
  n_before <- nrow(out)
  out <- out %>% left_join(controls, by = c("residence", "year", "quarter"))
  if (nrow(out) != n_before) {
    stop("UK-born controls changed the row count.", call. = FALSE)
  }
  # Same one-quarter gap as the QoQ wage change. Levels remain above.
  out <- difference_from(
    out,
    c(
      "mean_age_uk_born", "share_uk_degree", "share_uk_low_qual",
      "share_uk_asian", "share_uk_black", "share_uk_chinese",
      "share_uk_eth_other"
    ),
    1L,
    "d_qoq_"
  )

  out %>% arrange(residence, year, quarter)
}

# A non-positive or missing denominator is a missing ratio, not infinity.
ratio_or_na <- function(numerator, denominator) {
  out <- numerator / denominator
  out[!is.finite(denominator) | denominator <= 0] <- NA_real_
  out
}

# Finite values agree. A row that is finite on only one side disagrees.
values_agree <- function(actual, expected, label) {
  if (length(actual) != length(expected)) {
    stop(label, " has a different number of rows.", call. = FALSE)
  }
  actual_ok <- is.finite(actual)
  expected_ok <- is.finite(expected)
  if (any(actual_ok != expected_ok)) {
    stop(label, " is not finite on the same rows.", call. = FALSE)
  }
  if (!any(actual_ok)) return(invisible(NULL))
  scale <- pmax(1, abs(actual[actual_ok]), abs(expected[expected_ok]))
  if (any(abs(actual[actual_ok] - expected[expected_ok]) > 1e-8 * scale)) {
    stop(label, " is outside tolerance.", call. = FALSE)
  }
  invisible(NULL)
}

stop_if_invalid_counts <- function(x, label) {
  if (any(!is.finite(x) | x < 0)) {
    stop(label, " must be finite and non-negative.", call. = FALSE)
  }
  invisible(NULL)
}

# Direct counts, the birthplace partition of pop, and the regional ratios.
# The share ratio is defined on the same rows because the known-birthplace
# total cancels.
check_group_counts <- function(panel, local) {
  if (!identical(local$residence, panel$residence) ||
      !identical(local$year, panel$year) ||
      !identical(local$quarter, panel$quarter)) {
    stop("Birthplace counts are not on the same rows as the panel.", call. = FALSE)
  }
  stop_if_invalid_counts(
    c(
      panel$pop, panel$pop_uk_born, panel$pop_eu_born, panel$pop_non_eu_born,
      panel$pop_birthplace_unknown, local$native, local$eu, local$non_eu,
      local$migrant
    ),
    "Population counts"
  )
  known <- panel$pop_uk_born + panel$pop_eu_born + panel$pop_non_eu_born
  tol <- 1e-8 * pmax(1, abs(panel$pop))
  if (any(known - panel$pop > tol)) {
    stop("Birthplace group counts exceed pop.", call. = FALSE)
  }
  values_agree(
    known + panel$pop_birthplace_unknown, panel$pop,
    "Sum of birthplace counts"
  )
  values_agree(local$native, panel$pop_uk_born, "Native count")
  values_agree(local$eu, panel$pop_eu_born, "EU count")
  values_agree(local$non_eu, panel$pop_non_eu_born, "Non-EU count")
  values_agree(local$migrant, local$eu + local$non_eu, "Migrant count")
  values_agree(
    local$ratio_migrant,
    ratio_or_na(
      panel$share_birth_eu + panel$share_birth_non_eu, panel$share_birth_uk
    ),
    "Regional migrant ratio"
  )
  values_agree(
    local$ratio_eu,
    ratio_or_na(panel$share_birth_eu, panel$share_birth_uk),
    "Regional EU ratio"
  )
  values_agree(
    local$ratio_non_eu,
    ratio_or_na(panel$share_birth_non_eu, panel$share_birth_uk),
    "Regional non-EU ratio"
  )
  invisible(NULL)
}

# National and leave-one-out ratios from the sums of the area counts.
check_aggregated_counts <- function(local, joined) {
  counts <- c("native", "eu", "non_eu", "migrant")
  national_counts <- c(
    "national_native", "national_eu", "national_non_eu", "national_migrant"
  )
  stop_if_invalid_counts(
    c(as.matrix(local[counts]), as.matrix(joined[national_counts])),
    "Aggregated population counts"
  )
  key <- paste(local$year, local$quarter)
  totals <- rowsum(as.matrix(local[counts]), key)
  joined_key <- paste(joined$year, joined$quarter)
  values_agree(
    joined$national_native, totals[joined_key, "native"],
    "National native count"
  )
  values_agree(
    joined$national_eu, totals[joined_key, "eu"],
    "National EU count"
  )
  values_agree(
    joined$national_non_eu, totals[joined_key, "non_eu"],
    "National non-EU count"
  )
  values_agree(
    joined$national_migrant, totals[joined_key, "migrant"],
    "National migrant count"
  )
  values_agree(
    joined$national_migrant,
    joined$national_eu + joined$national_non_eu,
    "National migrant total"
  )
  values_agree(
    joined$national_ratio_migrant,
    ratio_or_na(joined$national_migrant, joined$national_native),
    "National migrant ratio"
  )
  values_agree(
    joined$national_ratio_eu,
    ratio_or_na(joined$national_eu, joined$national_native),
    "National EU ratio"
  )
  values_agree(
    joined$national_ratio_non_eu,
    ratio_or_na(joined$national_non_eu, joined$national_native),
    "National non-EU ratio"
  )
  values_agree(
    joined$loo_ratio_migrant,
    ratio_or_na(
      joined$national_migrant - joined$migrant,
      joined$national_native - joined$native
    ),
    "Leave-one-out migrant ratio"
  )
  values_agree(
    joined$loo_ratio_eu,
    ratio_or_na(
      joined$national_eu - joined$eu,
      joined$national_native - joined$native
    ),
    "Leave-one-out EU ratio"
  )
  values_agree(
    joined$loo_ratio_non_eu,
    ratio_or_na(
      joined$national_non_eu - joined$non_eu,
      joined$national_native - joined$native
    ),
    "Leave-one-out non-EU ratio"
  )
  invisible(NULL)
}

stop_unless_baseline_window <- function(panel, year_min, year_max) {
  label <- paste0(year_min, "-", year_max)
  window <- expand.grid(
    year = seq.int(year_min, year_max),
    quarter = 1:4,
    stringsAsFactors = FALSE
  )
  window$year <- as.integer(window$year)
  window$quarter <- as.integer(window$quarter)
  have <- panel %>% distinct(year, quarter)
  missing_periods <- anti_join(window, have, by = c("year", "quarter"))
  if (nrow(missing_periods) > 0L) {
    stop(
      "Panel does not cover the ", label, " baseline. Missing years: ",
      paste(sort(unique(missing_periods$year)), collapse = ", "),
      ". Rebuild the panel so it includes ", label, ".",
      call. = FALSE
    )
  }

  areas <- sort(unique(panel$residence))
  grid <- merge(data.frame(residence = areas, stringsAsFactors = FALSE), window)
  have_rows <- panel %>%
    filter(year >= year_min, year <= year_max) %>%
    distinct(residence, year, quarter)
  missing_rows <- anti_join(
    grid, have_rows, by = c("residence", "year", "quarter")
  )
  if (nrow(missing_rows) > 0L) {
    stop(
      label, " is missing ", nrow(missing_rows),
      " residence-quarters, including ",
      paste(
        apply(
          head(missing_rows, 5L), 1,
          function(row) paste(row[["residence"]], row[["year"]], "Q", row[["quarter"]], sep = " ")
        ),
        collapse = "; "
      ),
      ".",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# Pooled baseline ratios and the shift-shares that multiply them by the
# leave-one-out changes already on data. suffix is appended to the
# baseline column names. A quarter counts only when native, eu, and
# non_eu are all finite.
attach_baseline_shift_share <- function(data, year_min, year_max, suffix) {
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
  if (nrow(data) != n_before) {
    stop("Baseline join changed the row count.", call. = FALSE)
  }

  data[[paste0("baseline_national_ratio_migrant", suffix)]] <-
    ratio_or_na(sum(baseline_rows$migrant), national_native)
  data[[paste0("baseline_national_ratio_eu", suffix)]] <-
    ratio_or_na(sum(baseline_rows$eu), national_native)
  data[[paste0("baseline_national_ratio_non_eu", suffix)]] <-
    ratio_or_na(sum(baseline_rows$non_eu), national_native)
  data[[paste0("shift_share_qoq_migrant", suffix)]] <-
    data[[paste0("baseline_ratio_migrant", suffix)]] * data$d_qoq_loo_ratio_migrant
  data[[paste0("shift_share_qoq_eu", suffix)]] <-
    data[[paste0("baseline_ratio_eu", suffix)]] * data$d_qoq_loo_ratio_eu
  data[[paste0("shift_share_qoq_non_eu", suffix)]] <-
    data[[paste0("baseline_ratio_non_eu", suffix)]] * data$d_qoq_loo_ratio_non_eu
  data[[paste0("shift_share_yoy_migrant", suffix)]] <-
    data[[paste0("baseline_ratio_migrant", suffix)]] * data$d_yoy_loo_ratio_migrant
  data[[paste0("shift_share_yoy_eu", suffix)]] <-
    data[[paste0("baseline_ratio_eu", suffix)]] * data$d_yoy_loo_ratio_eu
  data[[paste0("shift_share_yoy_non_eu", suffix)]] <-
    data[[paste0("baseline_ratio_non_eu", suffix)]] * data$d_yoy_loo_ratio_non_eu
  data
}

# Value in this quarter minus the value k quarters earlier, matched on the
# calendar. A missing earlier quarter stays missing.
difference_from <- function(data, cols, k, name_prefix) {
  past <- copy_from(data, cols, k, "__past")
  for (col in cols) {
    data[[paste0(name_prefix, col)]] <- data[[col]] - past[[paste0(col, "__past")]]
  }
  data
}

# Copies cols from k quarters earlier onto the current row, with suffix
# appended. The earlier quarter is matched on the calendar.
copy_from <- function(data, cols, k, suffix) {
  index <- data$year * 4L + (data$quarter - 1L) - as.integer(k)
  keys <- tibble(
    residence = data$residence,
    .row = seq_len(nrow(data)),
    lag_year = index %/% 4L,
    lag_quarter = as.integer(index %% 4L) + 1L
  )
  past <- data %>% select(residence, year, quarter, all_of(cols))
  joined <- left_join(
    keys,
    past,
    by = c("residence", "lag_year" = "year", "lag_quarter" = "quarter")
  )
  joined <- joined %>% arrange(.row)
  if (nrow(joined) != nrow(data)) {
    stop("Lag join changed the row count.", call. = FALSE)
  }
  out <- data
  for (col in cols) out[[paste0(col, suffix)]] <- joined[[col]]
  out
}

# Quarterly UK-born controls from the panel's _uk_born columns.
# Qualification shares on the panel include "don't know" in the denominator.
# The two qualification controls drop it. Chinese is separate from Asian
# and from the mixed/other residual, so white is the omitted ethnicity.
native_composition_controls <- function(panel) {
  qual <- c(
    "share_qual_degree_uk_born", "share_qual_higher_ed_uk_born",
    "share_qual_alevel_uk_born", "share_qual_gcse_uk_born",
    "share_qual_other_uk_born", "share_qual_none_uk_born",
    "share_qual_dont_know_uk_born"
  )
  eth <- c(
    "share_eth_white_uk_born", "share_eth_mixed_uk_born",
    "share_eth_asian_uk_born", "share_eth_chinese_uk_born",
    "share_eth_black_uk_born", "share_eth_other_uk_born"
  )
  needed <- c(qual, eth, "mean_age_uk_born")
  missing <- setdiff(needed, names(panel))
  if (length(missing) > 0L) {
    stop(
      "Panel is missing ", paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  known_qual <- panel$share_qual_degree_uk_born +
    panel$share_qual_higher_ed_uk_born + panel$share_qual_alevel_uk_born +
    panel$share_qual_gcse_uk_born + panel$share_qual_other_uk_born +
    panel$share_qual_none_uk_born
  known_eth <- panel$share_eth_white_uk_born + panel$share_eth_mixed_uk_born +
    panel$share_eth_asian_uk_born + panel$share_eth_chinese_uk_born +
    panel$share_eth_black_uk_born + panel$share_eth_other_uk_born

  tibble(
    residence = as.character(panel$residence),
    year = as.integer(panel$year),
    quarter = as.integer(panel$quarter),
    mean_age_uk_born = as.numeric(panel$mean_age_uk_born),
    share_uk_degree = ratio_or_na(panel$share_qual_degree_uk_born, known_qual),
    share_uk_low_qual = ratio_or_na(
      panel$share_qual_gcse_uk_born + panel$share_qual_other_uk_born +
        panel$share_qual_none_uk_born,
      known_qual
    ),
    share_uk_asian = ratio_or_na(panel$share_eth_asian_uk_born, known_eth),
    share_uk_black = ratio_or_na(panel$share_eth_black_uk_born, known_eth),
    share_uk_chinese = ratio_or_na(panel$share_eth_chinese_uk_born, known_eth),
    share_uk_eth_other = ratio_or_na(
      panel$share_eth_mixed_uk_born + panel$share_eth_other_uk_born,
      known_eth
    )
  )
}
