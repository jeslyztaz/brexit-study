# Synthetic checks for the annual residence-year robustness pipeline.
# From the project root:
#   Rscript tests/test_annual_panel.R

source("setup.R")
source("functions/lfs_panel.R")
source("functions/lfs_panel_annual.R")
source("functions/construct_variables.R")
source("functions/construct_variables_annual.R")
source("pipeline/annual_common.R")

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
      stop(label, ": largest gap ", max(gap), ".", call. = FALSE)
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

# --- coverage --------------------------------------------------------------

files <- data.frame(
  year = c(rep(2010, 4), 2011, 2011, rep(2014, 4)),
  quarter = c(1:4, 1, 2, 1:4)
)
coverage <- calendar_year_coverage(files)
expect_included <- function(year, included) {
  row <- coverage[coverage$year == year, ]
  if (nrow(row) != 1L || !identical(row$included, included)) {
    stop("Coverage for ", year, " is not ", included, ".", call. = FALSE)
  }
}
expect_included(2010, TRUE)
expect_included(2011, FALSE)
expect_included(2012, FALSE)
expect_included(2013, FALSE)
expect_included(2014, TRUE)
if (!grepl("incomplete", coverage$reason[coverage$year == 2011])) {
  stop("2011 should be described as incomplete.", call. = FALSE)
}
if (!grepl("no quarterly file", coverage$reason[coverage$year == 2012])) {
  stop("2012 should be described as missing.", call. = FALSE)
}
pass("calendar-year coverage")

must_stop(
  calendar_year_coverage(data.frame(year = c(2010, 2010), quarter = c(1, 1))),
  "more than once",
  "Duplicate quarter"
)
pass("duplicate quarter is rejected")

# --- aggregation -----------------------------------------------------------

person <- function(age, weight, birth, residence = "Wales",
                   hourpay = NA_real_, iw = NA_real_, statr = NA_character_,
                   ethnicity = NA_real_) {
  data.frame(
    AGE = age,
    WEIGHT = weight,
    RESIDENCE_LABEL = residence,
    HOURPAY = hourpay,
    INCOME_WEIGHT = iw,
    STATR_LABEL = statr,
    ETHNICITY = ethnicity,
    birth = birth,
    stringsAsFactors = FALSE
  )
}

finish <- function(df, origin = NA_character_) {
  df$birth3 <- factor(df$birth, levels = unname(birth_shares))
  df$birth <- NULL
  if (!is.na(origin)) {
    attr(df, "source_variables") <- c(ETHNICITY = origin)
  }
  df
}

synthetic_quarter <- function(people, quarter, year = 2010L, eth_origin = NA_character_) {
  info <- list(
    path = sprintf(
      "synthetic/UKDA-1-tab/lfsp_%s%02d_synthetic.tab",
      c("jm", "aj", "js", "od")[[quarter]], year %% 100L
    ),
    year = as.integer(year),
    quarter = as.integer(quarter),
    quarter_code = c("jm", "aj", "js", "od")[[quarter]],
    pwt_name = "PWT18",
    piwt_name = "PIWT18"
  )
  list(
    lfs = people,
    info = info,
    quarter = as.integer(quarter),
    source_file = lfs_source_name(info$path),
    person_weight = info$pwt_name,
    income_weight = info$piwt_name,
    eth_origin = eth_origin,
    occ_scheme = NA_character_,
    birthplace_source = "synthetic",
    birthplace_scheme = "synthetic",
    overlap = list(
      compared = NA_character_, n = NA_integer_,
      n_disagree = NA_integer_, n_crown = NA_integer_
    )
  )
}

# The age-15 and missing-residence rows must not enter the annual stock.
# Q2, Q3, and Q4 repeat a person-quarter structure. They are not linked to Q1.
q1_people <- finish(rbind(
  person(20, 1, "UK", hourpay = 10, iw = 1, statr = "Employee"),
  person(40, 1, "UK", hourpay = 30, iw = 1, statr = "Employee"),
  person(30, 4, "EU"),
  person(25, 2, NA),
  person(15, 1000, "UK"),
  person(40, 500, "UK", residence = NA)
))
later_people <- finish(rbind(
  person(40, 10, "UK", hourpay = 30, iw = 10, statr = "Employee"),
  person(30, 2, "EU")
))
quarters <- list(
  synthetic_quarter(q1_people, 1L),
  synthetic_quarter(later_people, 2L),
  synthetic_quarter(later_people, 3L),
  synthetic_quarter(later_people, 4L)
)
annual <- aggregate_residence_year(quarters, 2010L)
check <- check_annual_year(quarters, annual, 2010L)
if (nrow(check) != 1L || check$year != 2010L) {
  stop("Aggregation check did not return the tested year.", call. = FALSE)
}

wales <- annual[annual$residence == "Wales", ]
tyne <- annual[annual$residence == "Tyne & Wear", ]
expect_close(wales$pop, 11, "Wales annual pop is the average")
expect_close(wales$pop_uk_born, 8, "Wales UK-born average")
expect_close(wales$pop_eu_born, 2.5, "Wales EU-born average")
expect_close(wales$pop_non_eu_born, 0, "Wales non-EU average")
expect_close(wales$pop_birthplace_unknown, 0.5, "Wales unknown average")
expect_close(
  wales$pop_uk_born + wales$pop_eu_born + wales$pop_non_eu_born +
    wales$pop_birthplace_unknown,
  wales$pop,
  "Wales annual partition"
)
if (abs(wales$pop - 44) < 1) {
  stop("Annual population summed the quarterly stocks.", call. = FALSE)
}
expect_close(wales$n, 10, "Wales person-quarter count")
expect_close(wales$n_pay_uk_born, 5, "Wales pooled wage count")
expect_close(tyne$pop, 0, "Empty area pop")
expect_close(tyne$n, 0, "Empty area count")
if (!is.na(tyne$birthplace_missing_rate)) {
  stop("An empty area should have a missing birthplace rate.", call. = FALSE)
}

expect_close(wales$mean_age_uk_born, 1260 / 32, "Pooled UK-born mean age")
if (abs(wales$mean_age_uk_born - 37.5) < 1e-8) {
  stop("Mean age is the unweighted average of quarterly means.", call. = FALSE)
}
wage_w <- c(1, 1, 10, 10, 10)
wage_pay <- c(10, 30, 30, 30, 30)
expect_close(
  wales$mean_log_hourpay_uk_born,
  sum(wage_w * log(wage_pay)) / sum(wage_w),
  "Pooled UK-born mean log wage"
)
q1_mean <- (log(10) + log(30)) / 2
unweighted_wage <- (q1_mean + 3 * log(30)) / 4
if (abs(wales$mean_log_hourpay_uk_born - unweighted_wage) < 1e-8) {
  stop("Mean log wage is the unweighted average of quarterly means.", call. = FALSE)
}
expect_close(wales$median_hourpay_uk_born, 30, "Pooled UK-born median wage")
if (abs(wales$median_hourpay_uk_born - 25) < 1e-8) {
  stop("Median wage is the average of quarterly medians.", call. = FALSE)
}
expect_close(wales$share_birth_uk, 8 / 10.5, "Pooled UK birth share")
unweighted_share <- ((2 / 6) + 3 * (10 / 12)) / 4
if (abs(wales$share_birth_uk - unweighted_share) < 1e-8) {
  stop("Birth share is the unweighted average of quarterly shares.", call. = FALSE)
}
pass("annual stocks, pooled means, and pooled median")

must_stop(
  aggregate_residence_year(quarters[1:3], 2010L),
  "four quarters",
  "Three-quarter year"
)
pass("incomplete year is rejected")

# ETHUKEUL code 3 is Indian. Four identical quarters, so the annual share
# equals the quarterly share. Code 5 never appears, so Bangladeshi is zero.
eth_people <- finish(rbind(
  person(30, 1, "UK", ethnicity = 3),
  person(30, 1, "UK", ethnicity = 4),
  person(30, 2, "EU", ethnicity = 1)
), origin = "ETHUKEUL")
eth_quarters <- lapply(1:4, function(q) {
  synthetic_quarter(eth_people, q, year = 2011L, eth_origin = "ETHUKEUL")
})
eth_annual <- aggregate_residence_year(eth_quarters, 2011L)
check_annual_year(eth_quarters, eth_annual, 2011L)
eth_wales <- eth_annual[eth_annual$residence == "Wales", ]
expect_close(eth_wales$share_eth_indian, 0.25, "Indian share")
expect_close(eth_wales$share_eth_pakistani, 0.25, "Pakistani share")
expect_close(eth_wales$share_eth_bangladeshi, 0, "Bangladeshi share")
expect_close(eth_wales$share_eth_indian_uk_born, 0.5, "UK-born Indian share")
expect_close(eth_wales$pop, 4, "Ethnicity-test population")
pass("pooled ethnicity detail shares")

# ETHCEN6 code 3 is the whole Asian group, not Indian.
cen_people <- finish(
  person(30, 1, "UK", ethnicity = 3),
  origin = "ETHCEN6"
)
cen_quarters <- lapply(1:4, function(q) {
  synthetic_quarter(cen_people, q, eth_origin = "ETHCEN6")
})
cen_annual <- aggregate_residence_year(cen_quarters, 2010L)
check_annual_year(cen_quarters, cen_annual, 2010L)
cen_wales <- cen_annual[cen_annual$residence == "Wales", ]
if (!is.na(cen_wales$share_eth_indian)) {
  stop("ETHCEN6 code 3 was treated as Indian.", call. = FALSE)
}
pass("ETHCEN6 does not create an Indian share")

# --- variables -------------------------------------------------------------

qual_eth <- c(
  share_qual_degree_uk_born = 0.25,
  share_qual_higher_ed_uk_born = 0.15,
  share_qual_alevel_uk_born = 0.15,
  share_qual_gcse_uk_born = 0.15,
  share_qual_other_uk_born = 0.10,
  share_qual_none_uk_born = 0.10,
  share_qual_dont_know_uk_born = 0.10,
  share_eth_white_uk_born = 0.70,
  share_eth_mixed_uk_born = 0.05,
  share_eth_asian_uk_born = 0.10,
  share_eth_chinese_uk_born = 0.05,
  share_eth_black_uk_born = 0.05,
  share_eth_other_uk_born = 0.05
)

make_row <- function(residence, year, uk, eu, non_eu, unknown, wage_uk) {
  known <- uk + eu + non_eu
  row <- data.frame(
    residence = residence,
    year = as.integer(year),
    pop = known + unknown,
    pop_uk_born = uk,
    pop_eu_born = eu,
    pop_non_eu_born = non_eu,
    pop_birthplace_unknown = unknown,
    share_birth_uk = uk / known,
    share_birth_eu = eu / known,
    share_birth_non_eu = non_eu / known,
    mean_log_hourpay = 2.5,
    mean_log_hourpay_uk_born = wage_uk,
    n_pay_uk_born = 40,
    mean_age_uk_born = 40,
    stringsAsFactors = FALSE
  )
  for (nm in names(qual_eth)) row[[nm]] <- unname(qual_eth[[nm]])
  row
}

rows <- list()
for (residence in c("North", "South")) {
  for (year in c(2010:2021, 2023)) {
    if (residence == "North") {
      eu <- 100
      wage <- 2
      if (year == 2018L) {
        eu <- 300
        wage <- 3
      }
      if (year == 2023L) eu <- 500
      rows[[length(rows) + 1L]] <- make_row(residence, year, 1000, eu, 50, 100, wage)
    } else {
      early <- year <= 2015L
      rows[[length(rows) + 1L]] <- make_row(
        residence, year, 400,
        if (early) 80 else 200,
        if (early) 40 else 100,
        0, 2
      )
    }
  }
}
panel <- bind_rows(rows)
out <- construct_variables_annual(panel)
north_2018 <- out[out$residence == "North" & out$year == 2018L, ]
north_2019 <- out[out$residence == "North" & out$year == 2019L, ]
north_2010 <- out[out$residence == "North" & out$year == 2010L, ]
north_2023 <- out[out$residence == "North" & out$year == 2023L, ]
south_2016 <- out[out$residence == "South" & out$year == 2016L, ]
south_2018 <- out[out$residence == "South" & out$year == 2018L, ]
south_2023 <- out[out$residence == "South" & out$year == 2023L, ]

expect_close(north_2018$d_annual_ratio_migrant, 0.20, "North own 2018 change")
expect_close(north_2018$shift_share_annual_migrant, 0, "North instrument ignores its own change")
expect_close(south_2018$d_annual_ratio_migrant, 0, "South own 2018 change")
expect_close(
  south_2018$shift_share_annual_migrant,
  0.75 * (350 / 1000 - 150 / 1000),
  "South leave-one-out instrument"
)
expect_close(south_2018$shift_share_annual_migrant, 0.15, "South 2018 shift-share")
expect_close(
  south_2018$shift_share_annual_migrant_2010_2015, 0.06,
  "South 2010-2015 baseline instrument"
)
expect_close(north_2019$d_annual_ratio_migrant, -0.20, "North 2019 contemporaneous change")
expect_close(north_2019$d_annual_ratio_migrant_l1, 0.20, "North 2019 lagged one-year change")
if (abs(north_2019$d_annual_ratio_migrant - north_2019$d_annual_ratio_migrant_l1) < 1e-8) {
  stop("The lagged annual change equals the contemporaneous change.", call. = FALSE)
}
expect_close(north_2018$d_annual_mean_log_hourpay_uk_born, 1, "North 2018 UK-born wage change")
expect_close(north_2018$d_annual_mean_log_hourpay, 0, "Overall wage did not change")
expect_close(north_2019$d_annual_mean_log_hourpay_uk_born, -1, "North 2019 wage change")
if ("d_annual_mean_log_hourpay_uk_born_l1" %in% names(out)) {
  stop("The annual wage lag should not be created. Only the Jaeger ratio lag is stored.", call. = FALSE)
}
if (is.finite(north_2010$d_annual_ratio_migrant) || is.finite(north_2010$shift_share_annual_migrant)) {
  stop("The first year should not have a one-year change.", call. = FALSE)
}
if (is.finite(north_2023$d_annual_ratio_migrant) || is.finite(south_2023$d_annual_ratio_migrant) ||
    is.finite(north_2023$d_annual_mean_log_hourpay_uk_born)) {
  stop("A missing calendar year was bridged.", call. = FALSE)
}
expect_close(south_2016$d_annual_ratio_migrant, 0.45, "South 2016 change")
expect_close(unique(out$baseline_ratio_migrant[out$residence == "North"]), 650 / 3000, "North 2017-2019 baseline")
expect_close(unique(out$baseline_ratio_migrant[out$residence == "South"]), 0.75, "South 2017-2019 baseline")
expect_close(
  unique(out$baseline_ratio_migrant_2010_2015[out$residence == "South"]), 0.30,
  "South 2010-2015 baseline"
)
expect_close(unique(out$baseline_national_ratio_migrant), 1550 / 4200, "2017-2019 national baseline")
expect_close(unique(out$baseline_national_ratio_eu), 1100 / 4200, "2017-2019 national EU baseline")
expect_close(
  unique(out$baseline_national_ratio_migrant_2010_2015), 270 / 1400,
  "2010-2015 national baseline"
)
y2017 <- out[out$year == 2017L, ]
expect_close(unique(y2017$national_ratio_migrant), 450 / 1400, "2017 national ratio")
if (abs(unique(y2017$national_ratio_migrant) - mean(y2017$ratio_migrant)) < 1e-8) {
  stop("National ratio is the average of the area ratios.", call. = FALSE)
}
expect_close(out$share_uk_degree[1], 0.25 / 0.90, "Degree share drops don't know")
expect_close(out$share_uk_low_qual[1], 0.35 / 0.90, "Low qualification share")
expect_close(out$share_uk_eth_other[1], 0.10, "Mixed and other ethnicity share")
if (any(out$period != as.character(out$year))) {
  stop("period is not the calendar year.", call. = FALSE)
}
if ("quarter" %in% names(out)) stop("Annual regression file contains a quarter.", call. = FALSE)
pass("annual baselines, leave-one-out instruments, and lags")

must_stop(
  construct_variables_annual(panel[names(panel) != "pop_uk_born"]),
  "pop_uk_born",
  "Missing pop_uk_born"
)
negative <- panel
negative$pop_uk_born[1] <- -1
must_stop(construct_variables_annual(negative), "non-negative", "Negative count")
gapped <- panel[!(panel$year == 2014L), ]
must_stop(construct_variables_annual(gapped), "2010-2015", "Missing baseline year")
with_quarter <- panel
with_quarter$quarter <- 1L
must_stop(construct_variables_annual(with_quarter), "quarterly", "Quarter column")
pass("annual variable checks reject bad inputs")

assert_annual_output(here("outputs", "annual", "main_annual_coefficients.csv"))
must_stop(
  assert_annual_output(here("outputs", "main_qoq_coefficients.csv")),
  "quarterly",
  "Quarterly coefficient path"
)
must_stop(
  assert_annual_output(here("outputs", "figures", "ratio_levels_by_residence.png")),
  "quarterly",
  "Quarterly figure path"
)
pass("annual outputs cannot use quarterly paths")

message("All ", passed, " annual-panel checks passed.")
