# Synthetic checks for weighted birthplace counts.
# From the project root:
#   Rscript tests/test_birthplace_counts.R

source("setup.R")
source("functions/lfs_panel.R")
source("functions/construct_variables.R")

passed <- 0L
pass <- function(label) {
  passed <<- passed + 1L
  message("PASS ", label)
}

expect_close <- function(actual, expected, label, tol = 1e-8) {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  if (length(actual) != length(expected)) {
    stop(label, ": length ", length(actual), " != ", length(expected), call. = FALSE)
  }
  actual_ok <- is.finite(actual)
  expected_ok <- is.finite(expected)
  if (any(actual_ok != expected_ok)) {
    stop(label, ": finite rows do not match.", call. = FALSE)
  }
  if (any(actual_ok)) {
    scale <- pmax(1, abs(actual[actual_ok]), abs(expected[expected_ok]))
    gap <- abs(actual[actual_ok] - expected[expected_ok])
    if (any(gap > tol * scale)) {
      stop(
        label, ": largest gap ", max(gap), ".",
        call. = FALSE
      )
    }
  }
  invisible(NULL)
}

must_stop <- function(expr, pattern, label) {
  message <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (is.null(message) || !grepl(pattern, message)) {
    stop(label, ": expected an error matching '", pattern, "'. Got: ", message, call. = FALSE)
  }
  invisible(message)
}

# Ages outside 16-64, non-positive weights, missing residence, and missing
# birthplace. Only the stated rows should enter the group totals.
people <- tibble::tibble(
  AGE = c(
    15, 16, 64, 65, NA, 30, 40, 30, 30, 20, 30, 40, 32, 40, 41
  ),
  RESIDENCE_LABEL = c(
    "Wales", "Wales", "Wales", "Wales", "Wales", "Wales", NA,
    "Wales", "Wales", "Wales", "Inner London", "Wales", "Wales",
    "South West", "South West"
  ),
  WEIGHT = c(
    100, 10, 20, 100, 100, 5, 50, 0, -3, 0.5, 3, 7, NA, 12, 8
  ),
  birth3 = factor(
    c(
      "UK", "UK", "EU", "non-EU", "UK", NA, "UK", "UK", "UK", "EU",
      "EU", "non-EU", "EU", NA, NA
    ),
    levels = unname(birth_shares)
  )
)

areas <- aggregate_residence_quarter(people, list(
  path = "synthetic/UKDA-1-tab/lfsp_jm10_synthetic.tab",
  year = 2010L,
  quarter = 1L,
  quarter_code = "jm",
  pwt_name = "PWT14",
  piwt_name = "PIWT14"
))

if (nrow(areas) != length(residence_labels)) {
  stop("Expected one row for every residence area.", call. = FALSE)
}

wales <- areas[areas$residence == "Wales", ]
inner <- areas[areas$residence == "Inner London", ]
south <- areas[areas$residence == "South West", ]
tyne <- areas[areas$residence == "Tyne & Wear", ]

expect_close(wales$pop, 42.5, "Wales pop")
expect_close(wales$pop_uk_born, 10, "Wales UK-born")
expect_close(wales$pop_eu_born, 20.5, "Wales EU-born")
expect_close(wales$pop_non_eu_born, 7, "Wales non-EU-born")
expect_close(wales$pop_birthplace_unknown, 5, "Wales unknown birthplace")
expect_close(wales$birthplace_missing_rate, 5 / 42.5, "Wales missingness rate")
expect_close(
  (wales$pop_eu_born + wales$pop_non_eu_born) / wales$pop_uk_born,
  (wales$share_birth_eu + wales$share_birth_non_eu) / wales$share_birth_uk,
  "Wales count ratio and share ratio"
)

expect_close(inner$pop, 3, "Inner London pop")
expect_close(inner$pop_uk_born, 0, "Inner London UK-born")
expect_close(inner$pop_eu_born, 3, "Inner London EU-born")
expect_close(inner$pop_non_eu_born, 0, "Inner London non-EU-born")
expect_close(inner$pop_birthplace_unknown, 0, "Inner London unknown birthplace")
if (!is.na(ratio_or_na(inner$pop_eu_born, inner$pop_uk_born)) ||
    !is.na(ratio_or_na(inner$share_birth_eu, inner$share_birth_uk))) {
  stop("A zero UK-born total should leave both ratios missing.", call. = FALSE)
}

expect_close(south$pop, 20, "South West pop")
expect_close(south$pop_uk_born, 0, "South West UK-born")
expect_close(south$pop_eu_born, 0, "South West EU-born")
expect_close(south$pop_non_eu_born, 0, "South West non-EU-born")
expect_close(south$pop_birthplace_unknown, 20, "South West unknown birthplace")
expect_close(south$birthplace_missing_rate, 1, "South West missingness rate")
if (!all(is.na(c(south$share_birth_uk, south$share_birth_eu, south$share_birth_non_eu)))) {
  stop("Missing birthplace should leave the birth shares missing.", call. = FALSE)
}

expect_close(tyne$pop, 0, "Empty area pop")
expect_close(tyne$pop_uk_born, 0, "Empty area UK-born")
expect_close(tyne$pop_eu_born, 0, "Empty area EU-born")
expect_close(tyne$pop_non_eu_born, 0, "Empty area non-EU-born")
expect_close(tyne$pop_birthplace_unknown, 0, "Empty area unknown birthplace")
if (!is.na(tyne$birthplace_missing_rate) || is.nan(tyne$birthplace_missing_rate)) {
  stop("An empty area should have a missing birthplace rate.", call. = FALSE)
}

expect_close(sum(areas$pop_uk_born), 10, "UK-born total across areas")
expect_close(sum(areas$pop_eu_born), 23.5, "EU-born total across areas")
expect_close(sum(areas$pop_non_eu_born), 7, "Non-EU-born total across areas")
expect_close(
  areas$pop_uk_born + areas$pop_eu_born + areas$pop_non_eu_born +
    areas$pop_birthplace_unknown,
  areas$pop,
  "Birthplace partition of pop"
)
pass("residence-quarter birthplace counts")

empty <- residence_stats(
  data.frame(
    birth3 = factor(character(), levels = unname(birth_shares)),
    WEIGHT = numeric()
  ),
  NA_character_,
  NA_character_
)
expect_close(empty$pop, 0, "Zero-row pop")
expect_close(empty$pop_uk_born, 0, "Zero-row UK-born")
expect_close(empty$pop_eu_born, 0, "Zero-row EU-born")
expect_close(empty$pop_non_eu_born, 0, "Zero-row non-EU-born")
expect_close(empty$pop_birthplace_unknown, 0, "Zero-row unknown birthplace")
if (!is.na(empty$birthplace_missing_rate)) {
  stop("A zero-row area should have a missing birthplace rate.", call. = FALSE)
}
pass("empty residence sample")

must_stop(
  residence_stats(
    data.frame(
      birth3 = factor("UK", levels = unname(birth_shares)),
      WEIGHT = Inf
    ),
    NA_character_,
    NA_character_
  ),
  "finite",
  "Infinite weight"
)
pass("infinite weight is rejected")

# Three areas. London's missing birthplace switches on in odd quarters, and
# its composition switches with it, so pop times the birth share is not the
# group total in a sum across areas or quarters.
periods <- rbind(
  expand.grid(year = 2010:2015, quarter = 1:4, stringsAsFactors = FALSE),
  expand.grid(year = 2017:2019, quarter = 1:4, stringsAsFactors = FALSE)
)
panel <- merge(
  data.frame(residence = c("North", "Midlands", "London"), stringsAsFactors = FALSE),
  periods,
  by = NULL
)
odd <- panel$quarter %in% c(1L, 3L)
late <- panel$year >= 2017L
london <- panel$residence == "London"
north <- panel$residence == "North"
midlands <- panel$residence == "Midlands"
panel$pop_uk_born <- NA_real_
panel$pop_eu_born <- NA_real_
panel$pop_non_eu_born <- NA_real_
panel$pop_birthplace_unknown <- NA_real_
panel$pop_uk_born[north] <- 1000
panel$pop_eu_born[north] <- 100
panel$pop_non_eu_born[north] <- 50
panel$pop_birthplace_unknown[north] <- 0
panel$pop_uk_born[midlands] <- 400
panel$pop_eu_born[midlands] <- 200
panel$pop_non_eu_born[midlands] <- 100
panel$pop_birthplace_unknown[midlands] <- 0
panel$pop_uk_born[london & odd] <- 800
panel$pop_eu_born[london & odd] <- 50
panel$pop_non_eu_born[london & odd] <- 50
panel$pop_birthplace_unknown[london & odd] <- 400
panel$pop_uk_born[london & !odd & !late] <- 400
panel$pop_eu_born[london & !odd & !late] <- 200
panel$pop_non_eu_born[london & !odd & !late] <- 100
panel$pop_birthplace_unknown[london & !odd & !late] <- 0
panel$pop_uk_born[london & !odd & late] <- 200
panel$pop_eu_born[london & !odd & late] <- 200
panel$pop_non_eu_born[london & !odd & late] <- 200
panel$pop_birthplace_unknown[london & !odd & late] <- 0
known <- panel$pop_uk_born + panel$pop_eu_born + panel$pop_non_eu_born
panel$pop <- known + panel$pop_birthplace_unknown
panel$share_birth_uk <- panel$pop_uk_born / known
panel$share_birth_eu <- panel$pop_eu_born / known
panel$share_birth_non_eu <- panel$pop_non_eu_born / known
panel$mean_log_hourpay <- 2.5
panel$mean_log_hourpay_uk_born <- 2.4
panel$mean_age_uk_born <- 40
panel$share_qual_degree_uk_born <- 0.25
panel$share_qual_higher_ed_uk_born <- 0.15
panel$share_qual_alevel_uk_born <- 0.15
panel$share_qual_gcse_uk_born <- 0.15
panel$share_qual_other_uk_born <- 0.10
panel$share_qual_none_uk_born <- 0.10
panel$share_qual_dont_know_uk_born <- 0.10
panel$share_eth_white_uk_born <- 0.70
panel$share_eth_mixed_uk_born <- 0.05
panel$share_eth_asian_uk_born <- 0.10
panel$share_eth_chinese_uk_born <- 0.05
panel$share_eth_black_uk_born <- 0.05
panel$share_eth_other_uk_born <- 0.05

out <- construct_variables(panel)
keys <- c("residence", "year", "quarter")
merged <- merge(out, panel, by = keys, suffixes = c("", "_panel"))

expect_close(merged$native, merged$pop_uk_born, "native copies pop_uk_born")
expect_close(merged$eu, merged$pop_eu_born, "eu copies pop_eu_born")
expect_close(merged$non_eu, merged$pop_non_eu_born, "non_eu copies pop_non_eu_born")
expect_close(merged$migrant, merged$eu + merged$non_eu, "migrant is eu plus non_eu")
if (any(!is.finite(c(merged$native, merged$eu, merged$non_eu, merged$migrant))) ||
    any(c(merged$native, merged$eu, merged$non_eu, merged$migrant) < 0)) {
  stop("Constructed counts include a negative or non-finite value.", call. = FALSE)
}

share_ratio <- ratio_or_na(
  merged$share_birth_eu + merged$share_birth_non_eu, merged$share_birth_uk
)
expect_close(merged$ratio_migrant, share_ratio, "Regional ratio matches shares")
old_native <- merged$pop * merged$share_birth_uk
old_eu <- merged$pop * merged$share_birth_eu
old_non_eu <- merged$pop * merged$share_birth_non_eu
expect_close(
  merged$ratio_migrant,
  ratio_or_na(old_eu + old_non_eu, old_native),
  "Regional ratio matches the old share scaling"
)

national <- aggregate(
  cbind(native, eu, non_eu, migrant, old_native, old_eu, old_non_eu) ~ year + quarter,
  data = data.frame(
    year = merged$year,
    quarter = merged$quarter,
    native = merged$native,
    eu = merged$eu,
    non_eu = merged$non_eu,
    migrant = merged$migrant,
    old_native = old_native,
    old_eu = old_eu,
    old_non_eu = old_non_eu
  ),
  FUN = sum
)
national$ratio <- national$migrant / national$native
national$old_ratio <- (national$old_eu + national$old_non_eu) / national$old_native
got_national <- unique(merged[c("year", "quarter", "national_ratio_migrant")])
national <- merge(national, got_national, by = c("year", "quarter"))
expect_close(
  national$national_ratio_migrant, national$ratio,
  "National ratio is the ratio of summed counts"
)
if (max(abs(national$ratio - national$old_ratio)) < 1e-4) {
  stop("National ratios did not move relative to pop times the birth share.", call. = FALSE)
}

north_q1 <- merged[merged$residence == "North" & merged$year == 2010 & merged$quarter == 1, ]
north_q2 <- merged[merged$residence == "North" & merged$year == 2010 & merged$quarter == 2, ]
north_2011 <- merged[merged$residence == "North" & merged$year == 2011 & merged$quarter == 1, ]
london_rows <- merged[merged$residence == "London", ]
expect_close(north_q1$national_ratio_migrant, 0.25, "2010 Q1 national migrant ratio")
expect_close(north_q1$national_ratio_eu, 350 / 2200, "2010 Q1 national EU ratio")
expect_close(north_q1$national_ratio_non_eu, 200 / 2200, "2010 Q1 national non-EU ratio")
expect_close(north_q2$national_ratio_migrant, 750 / 1800, "2010 Q2 national migrant ratio")
expect_close(north_q2$ratio_migrant, 0.15, "North regional migrant ratio")
expect_close(north_q2$shift_share_qoq_migrant, 0.0625, "North 2010 Q2 shift-share")
expect_close(north_2011$shift_share_qoq_migrant, -0.0625, "North 2011 Q1 quarter-to-quarter shift-share")
expect_close(north_2011$shift_share_yoy_migrant, 0, "North 2011 Q1 year-on-year shift-share")
expect_close(unique(london_rows$baseline_ratio_migrant), 1 / 3, "London 2010-2015 baseline")
window_2010 <- merged$year >= 2010 & merged$year <= 2015
pooled_native_2010 <- sum(merged$native[window_2010])
expect_close(
  unique(merged$baseline_national_ratio_migrant),
  sum(merged$migrant[window_2010]) / pooled_native_2010,
  "2010-2015 national baseline"
)
expect_close(
  unique(merged$baseline_national_ratio_eu),
  sum(merged$eu[window_2010]) / pooled_native_2010,
  "2010-2015 national EU baseline"
)
expect_close(
  unique(merged$baseline_national_ratio_non_eu),
  sum(merged$non_eu[window_2010]) / pooled_native_2010,
  "2010-2015 national non-EU baseline"
)
expect_close(
  unique(london_rows$baseline_ratio_migrant_2017_2019), 0.5,
  "London 2017-2019 baseline"
)
if (abs(unique(london_rows$baseline_ratio_migrant) - 2 / 7) < 1e-8) {
  stop("London baseline still uses pop times the birth share.", call. = FALSE)
}
pass("national, leave-one-out, baseline, and shift-share counts")

must_stop(
  construct_variables(panel[names(panel) != "pop_uk_born"]),
  "pop_uk_born",
  "Missing pop_uk_born"
)
negative <- panel
negative$pop_uk_born[1] <- -1
must_stop(construct_variables(negative), "non-negative", "Negative count")
scaled <- panel
scaled$pop_eu_born <- scaled$pop_eu_born + 10
scaled$pop <- scaled$pop + 10
must_stop(construct_variables(scaled), "Regional migrant ratio", "Stale birth shares")
pass("validation rejects bad counts")

message("All ", passed, " birthplace-count checks passed.")
