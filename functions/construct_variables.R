# Migration ratios for the shift-share regression.
#
# Input is the residence-quarter panel from build_residence_quarter_panel(),
# which must cover 2010 Q1 through 2015 Q4. Natives are UK-born. Migrants
# are EU-born plus non-EU-born. Regions are the URESMC residence areas.
#
# Counts are person-weighted: pop times the birth share. pop includes people
# with missing birthplace, while the shares do not, so the count matches the
# weighted group total only when birthplace is known for everyone in pop.
# The regional ratio does not depend on pop, because pop cancels.
#
# The lagged year-on-year change, d_yoy_ratio_*_l4, is the migration term
# Jaeger, Ruist and Stuhler put on the right-hand side. In quarter t it is
# the ratio four quarters earlier minus the ratio eight quarters earlier.
#
# The shift-share is that area's 2010-2015 baseline ratio times the
# leave-one-out change in the national ratio. Leaving the area out matters
# because these residence areas, especially London, are large shares of the
# national total. The inclusive national change would partly be the area's
# own migration. qoq and yoy instruments are both stored. The regression
# chooses which one to use. Rows are not restricted to a sample.
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
  required <- c(
    "residence", "year", "quarter", "pop",
    "share_birth_uk", "share_birth_eu", "share_birth_non_eu",
    "mean_log_hourpay", "mean_log_hourpay_uk_born"
  )
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
      across(all_of(required[4:9]), as.numeric)
    )
  if (anyNA(panel$year) || anyNA(panel$quarter)) {
    stop("Year and quarter must be non-missing.", call. = FALSE)
  }
  if (anyDuplicated(panel[c("residence", "year", "quarter")]) > 0L) {
    stop("Duplicate residence-year-quarter rows.", call. = FALSE)
  }

  stop_unless_baseline_window(panel)

  # An empty sample has pop 0 and missing shares. That is a zero count,
  # not a missing area. A positive pop with a missing share stays missing.
  local <- panel %>%
    mutate(
      native = group_count(pop, share_birth_uk),
      eu = group_count(pop, share_birth_eu),
      non_eu = group_count(pop, share_birth_non_eu),
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

  wages <- panel %>%
    select(residence, year, quarter, mean_log_hourpay, mean_log_hourpay_uk_born)

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
    filter(year >= 2010L, year <= 2015L) %>%
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
      "No usable 2010-2015 native total for: ",
      paste(bad, collapse = ", "), ".",
      call. = FALSE
    )
  }

  national_native <- sum(baseline_rows$native)
  out <- out %>%
    left_join(baseline, by = "residence") %>%
    mutate(
      baseline_national_ratio_migrant =
        ratio_or_na(sum(baseline_rows$migrant), national_native),
      baseline_national_ratio_eu =
        ratio_or_na(sum(baseline_rows$eu), national_native),
      baseline_national_ratio_non_eu =
        ratio_or_na(sum(baseline_rows$non_eu), national_native),
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

group_count <- function(pop, share) {
  out <- pop * share
  empty <- is.finite(pop) & pop == 0 & !is.finite(share)
  out[empty] <- 0
  out
}

# A non-positive or missing denominator is a missing ratio, not infinity.
ratio_or_na <- function(numerator, denominator) {
  out <- numerator / denominator
  out[!is.finite(denominator) | denominator <= 0] <- NA_real_
  out
}

stop_unless_baseline_window <- function(panel) {
  window <- expand.grid(
    year = 2010:2015,
    quarter = 1:4,
    stringsAsFactors = FALSE
  )
  window$year <- as.integer(window$year)
  window$quarter <- as.integer(window$quarter)
  have <- panel %>% distinct(year, quarter)
  missing_periods <- anti_join(window, have, by = c("year", "quarter"))
  if (nrow(missing_periods) > 0L) {
    stop(
      "Panel does not cover the 2010-2015 baseline. Missing years: ",
      paste(sort(unique(missing_periods$year)), collapse = ", "),
      ". Rebuild the panel from 2010.",
      call. = FALSE
    )
  }

  areas <- sort(unique(panel$residence))
  grid <- merge(data.frame(residence = areas, stringsAsFactors = FALSE), window)
  have_rows <- panel %>%
    filter(year >= 2010L, year <= 2015L) %>%
    distinct(residence, year, quarter)
  missing_rows <- anti_join(
    grid, have_rows, by = c("residence", "year", "quarter")
  )
  if (nrow(missing_rows) > 0L) {
    stop(
      "2010-2015 is missing ", nrow(missing_rows),
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
