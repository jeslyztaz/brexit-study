# Compare the main quarterly specification with the annual robustness check.
# Source this from the project root after both regression files and both
# coefficient files exist. It reads the quarterly outputs and writes only
# under outputs/annual/.

source("setup.R")
source(here("pipeline", "annual_common.R"))

run_guarded({
  residence_levels <- c(
    "Tyne & Wear", "Rest of Northern region", "South Yorkshire",
    "West Yorkshire", "Rest of Yorks & Humberside", "East Midlands",
    "East Anglia", "Inner London", "Outer London", "Rest of South East",
    "South West", "West Midlands (met county)", "Rest of West Midlands",
    "Greater Manchester", "Merseyside", "Rest of North West", "Wales",
    "Strathclyde", "Rest of Scotland", "Northern Ireland"
  )
  comparison_label <- "quarterly main specification vs annual robustness check"

  q_panel_file <- here("data", "processed", "residence_quarter_panel_full.csv")
  a_panel_file <- here("data", "processed", "residence_annual_panel.csv")
  q_reg_file <- here("data", "processed", "residence_quarter_regression.csv")
  a_reg_file <- here("data", "processed", "residence_annual_regression.csv")
  q_coef_file <- here("outputs", "main_qoq_coefficients.csv")
  a_coef_file <- here("outputs", "annual", "main_annual_coefficients.csv")
  checks_file <- here("outputs", "annual", "aggregation_checks.csv")
  notes_file <- here("outputs", "annual", "estimation_notes.csv")
  for (path in c(
    q_panel_file, a_panel_file, q_reg_file, a_reg_file,
    q_coef_file, a_coef_file, checks_file, notes_file
  )) {
    if (!file.exists(path)) {
      stop("Cannot find ", path, ".", call. = FALSE)
    }
  }

  q_panel <- read_csv(q_panel_file, show_col_types = FALSE)
  a_panel <- read_csv(a_panel_file, show_col_types = FALSE)
  q_reg <- read_csv(q_reg_file, show_col_types = FALSE)
  a_reg <- read_csv(a_reg_file, show_col_types = FALSE)
  q_coef <- read_csv(q_coef_file, show_col_types = FALSE)
  a_coef <- read_csv(a_coef_file, show_col_types = FALSE)
  build_checks <- read_csv(checks_file, show_col_types = FALSE)
  notes <- read_csv(notes_file, show_col_types = FALSE)

  rel_gaps <- function(actual, expected) {
    if (length(actual) != length(expected)) {
      stop("Compared columns have different lengths.", call. = FALSE)
    }
    scale <- pmax(1, abs(actual), abs(expected))
    gap <- abs(actual - expected) / scale
    both_missing <- is.na(actual) & is.na(expected)
    one_missing <- is.na(actual) != is.na(expected)
    gap[both_missing] <- 0
    gap[one_missing] <- Inf
    gap
  }

  checks <- tibble(
    check = character(),
    required = logical(),
    n = integer(),
    max_rel_gap = double(),
    limit = double(),
    passed = logical(),
    note = character()
  )
  add_check <- function(check, required, n, max_rel_gap, limit, note = "") {
    passed <- isTRUE(!required) || (
      is.finite(max_rel_gap) && n > 0L && max_rel_gap <= limit
    )
    checks <<- bind_rows(checks, tibble(
      check = check,
      required = required,
      n = as.integer(n),
      max_rel_gap = max_rel_gap,
      limit = if (isTRUE(required)) limit else NA_real_,
      passed = passed,
      note = note
    ))
  }

  no_quarter <- !"quarter" %in% names(a_panel) && !"quarter" %in% names(a_reg)
  add_check(
    "annual files have no quarter column",
    TRUE, 1L, if (no_quarter) 0 else 1, 0,
    "The annual panel and regression file are residence-year."
  )
  dup_panel <- anyDuplicated(a_panel[c("residence", "year")]) > 0L
  dup_reg <- anyDuplicated(a_reg[c("residence", "year")]) > 0L
  add_check(
    "one row per residence-year",
    TRUE, nrow(a_panel), if (dup_panel || dup_reg) 1 else 0, 0,
    "Duplicates are counted as a failure."
  )

  residence_ok <- identical(sort(unique(a_panel$residence)), sort(residence_levels)) &&
    identical(sort(unique(a_reg$residence)), sort(residence_levels)) &&
    identical(sort(unique(q_panel$residence)), sort(residence_levels))
  add_check(
    "URESMC residence set",
    TRUE, length(residence_levels), if (residence_ok) 0 else 1, 0,
    "Twenty residence labels, the same set on both panels."
  )

  quarter_sets <- q_panel %>%
    group_by(residence, year) %>%
    summarise(
      quarters = paste(sort(unique(as.integer(quarter))), collapse = ","),
      n_q = n(),
      .groups = "drop"
    )
  complete_cells <- quarter_sets %>%
    filter(quarters == "1,2,3,4", n_q == 4L)
  complete_years <- complete_cells %>%
    count(year) %>%
    filter(n == length(residence_levels)) %>%
    pull(year)
  annual_years <- sort(unique(as.integer(a_panel$year)))
  reg_years <- sort(unique(as.integer(a_reg$year)))
  years_ok <- identical(annual_years, sort(as.integer(complete_years))) &&
    identical(reg_years, annual_years)
  incomplete_years <- setdiff(sort(unique(as.integer(q_panel$year))), as.integer(complete_years))
  incomplete_absent <- !any(incomplete_years %in% annual_years)
  add_check(
    "annual years are the complete calendar years",
    TRUE, length(annual_years), if (years_ok && incomplete_absent) 0 else 1, 0,
    paste0(
      "Complete years: ", paste(sort(as.integer(complete_years)), collapse = ", "),
      ". Excluded quarterly years: ",
      if (length(incomplete_years) == 0L) "none" else paste(incomplete_years, collapse = ", "),
      "."
    )
  )
  per_year_n <- a_panel %>% count(year)
  add_check(
    "twenty residences in every annual year",
    TRUE, nrow(per_year_n),
    if (all(per_year_n$n == length(residence_levels))) 0 else 1, 0,
    ""
  )

  stock_cols <- c(
    "pop", "pop_uk_born", "pop_eu_born", "pop_non_eu_born",
    "pop_birthplace_unknown", "pop_pay"
  )
  q_avg <- q_panel %>%
    group_by(residence, year) %>%
    summarise(
      across(all_of(stock_cols), list(avg = mean, total = sum), .names = "{.col}_{.fn}"),
      n_sum = sum(n),
      n_pay_uk_sum = sum(n_pay_uk_born),
      uk_sum = sum(pop_uk_born),
      known_sum = sum(pop_uk_born + pop_eu_born + pop_non_eu_born),
      .groups = "drop"
    )
  stocks <- a_panel %>%
    mutate(year = as.integer(year)) %>%
    left_join(q_avg %>% mutate(year = as.integer(year)), by = c("residence", "year"))
  if (nrow(stocks) != nrow(a_panel)) {
    stop("Stock comparison changed the annual row count.", call. = FALSE)
  }
  for (column in stock_cols) {
    gaps <- rel_gaps(stocks[[column]], stocks[[paste0(column, "_avg")]])
    add_check(
      paste("annual", column, "equals the mean of quarterly stocks"),
      TRUE, length(gaps), max(gaps), 1e-6,
      "Quarterly stocks are not summed."
    )
  }
  closer_to_sum <- stocks$pop_total > 0 &
    abs(stocks$pop - stocks$pop_avg) >= abs(stocks$pop - stocks$pop_total) - 1e-6
  add_check(
    "annual population is not the sum of quarterly stocks",
    TRUE, sum(stocks$pop_total > 0), if (any(closer_to_sum)) 1 else 0, 0,
    "A summed stock is about four times the annual population."
  )
  count_gap <- max(
    abs(stocks$n - stocks$n_sum),
    abs(stocks$n_pay_uk_born - stocks$n_pay_uk_sum)
  )
  add_check(
    "annual person counts equal the sum of quarterly counts",
    TRUE, nrow(stocks), count_gap, 0,
    "n and n_pay_uk_born count person-quarters. They are not divided by 4."
  )

  partition <- stocks$pop_uk_born + stocks$pop_eu_born + stocks$pop_non_eu_born +
    stocks$pop_birthplace_unknown
  add_check(
    "annual birthplace totals add up to pop",
    TRUE, nrow(stocks), max(rel_gaps(partition, stocks$pop)), 1e-6,
    "Unknown birthplace stays in pop and out of the three birthplace groups."
  )
  expected_share <- ifelse(stocks$known_sum > 0, stocks$uk_sum / stocks$known_sum, NA_real_)
  add_check(
    "annual UK birth share uses pooled known-birthplace weights",
    TRUE, nrow(stocks), max(rel_gaps(stocks$share_birth_uk, expected_share)), 1e-6,
    "This is the ratio of summed quarterly stocks, not the average of quarterly shares."
  )

  build_gap_cols <- grep("^max_rel_", names(build_checks), value = TRUE)
  build_max <- max(as.matrix(build_checks[build_gap_cols]), na.rm = TRUE)
  add_check(
    "within-build quarterly sufficient statistics",
    TRUE, nrow(build_checks), build_max, 1e-6,
    paste(
      "Maximum of", paste(build_gap_cols, collapse = ", "),
      "in aggregation_checks.csv. Wages are income-weighted pooled means."
    )
  )

  # The quarterly file does not contain the UK-born income-weight total, so
  # the wage cannot be rebuilt here. The gap below is recorded because an
  # unweighted average of quarterly means is not the annual estimator.
  wage_q <- q_panel %>%
    group_by(residence, year) %>%
    summarise(
      unw = mean(mean_log_hourpay_uk_born),
      w = sum(n_pay_uk_born),
      wx = sum(n_pay_uk_born * mean_log_hourpay_uk_born, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(count_weighted = ifelse(w > 0, wx / w, NA_real_))
  wage_cmp <- a_panel %>%
    select(residence, year, mean_log_hourpay_uk_born) %>%
    left_join(wage_q, by = c("residence", "year"))
  add_check(
    "gap versus the unweighted average of quarterly UK-born log wages",
    FALSE, nrow(wage_cmp),
    max(rel_gaps(wage_cmp$mean_log_hourpay_uk_born, wage_cmp$unw)),
    NA_real_,
    "Recorded only. The annual wage pools income weights and is not this average."
  )

  ratio_names <- c("migrant", "eu", "non_eu")
  q_year <- q_reg %>%
    group_by(year) %>%
    summarise(
      q_native = sum(native),
      q_migrant = sum(migrant),
      q_eu = sum(eu),
      q_non_eu = sum(non_eu),
      .groups = "drop"
    ) %>%
    mutate(
      pooled_migrant = q_migrant / q_native,
      pooled_eu = q_eu / q_native,
      pooled_non_eu = q_non_eu / q_native
    )
  q_nat_mean <- q_reg %>%
    distinct(year, quarter, national_ratio_migrant, national_ratio_eu, national_ratio_non_eu) %>%
    group_by(year) %>%
    summarise(
      across(starts_with("national_ratio_"), mean),
      .groups = "drop"
    )
  a_nat <- a_reg %>%
    distinct(
      year, national_ratio_migrant, national_ratio_eu, national_ratio_non_eu
    )
  nat <- a_nat %>%
    left_join(q_year, by = "year") %>%
    left_join(q_nat_mean, by = "year", suffix = c("", "_unweighted_mean"))
  if (nrow(nat) != nrow(a_nat)) {
    stop("National-ratio comparison changed the year count.", call. = FALSE)
  }
  for (group in ratio_names) {
    gaps <- rel_gaps(nat[[paste0("national_ratio_", group)]], nat[[paste0("pooled_", group)]])
    add_check(
      paste("annual national", group, "ratio equals pooled quarterly counts"),
      TRUE, length(gaps), max(gaps), 1e-5,
      "The average of the four quarterly stocks cancels in the ratio."
    )
    recorded <- rel_gaps(
      nat[[paste0("national_ratio_", group)]],
      nat[[paste0("national_ratio_", group, "_unweighted_mean")]]
    )
    add_check(
      paste("gap versus the unweighted mean of quarterly national", group, "ratios"),
      FALSE, length(recorded), max(recorded), NA_real_,
      "Recorded only. The annual ratio does not average quarterly ratios."
    )
  }

  baseline_cols <- c(
    "baseline_ratio_migrant", "baseline_ratio_eu", "baseline_ratio_non_eu",
    "baseline_ratio_migrant_2010_2015", "baseline_ratio_eu_2010_2015",
    "baseline_ratio_non_eu_2010_2015",
    "baseline_national_ratio_migrant", "baseline_national_ratio_eu",
    "baseline_national_ratio_non_eu",
    "baseline_national_ratio_migrant_2010_2015",
    "baseline_national_ratio_eu_2010_2015",
    "baseline_national_ratio_non_eu_2010_2015"
  )
  varied <- function(data, columns) {
    any(vapply(columns, function(column) {
      n_distinct(data[[column]], na.rm = FALSE) != 1L
    }, logical(1)))
  }
  q_base_ok <- q_reg %>%
    group_by(residence) %>%
    group_modify(~ tibble(varies = varied(.x, baseline_cols))) %>%
    pull(varies)
  a_base_ok <- a_reg %>%
    group_by(residence) %>%
    group_modify(~ tibble(varies = varied(.x, baseline_cols))) %>%
    pull(varies)
  add_check(
    "baseline ratio is constant within residence",
    TRUE, length(q_base_ok), if (any(q_base_ok) || any(a_base_ok)) 1 else 0, 0,
    "The same pooled baseline is repeated on every period."
  )
  q_base <- q_reg %>%
    group_by(residence) %>%
    summarise(across(all_of(baseline_cols), ~ dplyr::first(.)), .groups = "drop")
  a_base <- a_reg %>%
    group_by(residence) %>%
    summarise(across(all_of(baseline_cols), ~ dplyr::first(.)), .groups = "drop")
  bases <- a_base %>%
    left_join(q_base, by = "residence", suffix = c("_annual", "_quarterly"))
  for (column in baseline_cols) {
    gaps <- rel_gaps(
      bases[[paste0(column, "_annual")]],
      bases[[paste0(column, "_quarterly")]]
    )
    add_check(
      paste("annual", column, "matches the quarterly pooled baseline"),
      TRUE, length(gaps), max(gaps), 1e-5,
      "Both are summed stocks over the same window. Post-window years are not in the sum."
    )
  }

  # Rebuild one-year changes and shift-shares from the annual counts.
  # The previous year is the calendar year, not the previous row.
  national <- a_reg %>%
    group_by(year) %>%
    summarise(
      national_native = sum(native),
      national_migrant = sum(migrant),
      national_eu = sum(eu),
      national_non_eu = sum(non_eu),
      .groups = "drop"
    )
  rebuilt <- a_reg %>%
    left_join(national, by = "year") %>%
    mutate(
      loo_den = national_native - native,
      loo_migrant = (national_migrant - migrant) / loo_den,
      loo_eu = (national_eu - eu) / loo_den,
      loo_non_eu = (national_non_eu - non_eu) / loo_den
    )
  # Same rule as ratio_or_na(): a non-positive denominator is missing, not Inf.
  for (column in c("loo_migrant", "loo_eu", "loo_non_eu")) {
    rebuilt[[column]][!is.finite(rebuilt$loo_den) | rebuilt$loo_den <= 0] <- NA_real_
  }
  previous <- rebuilt %>%
    transmute(
      residence, year = year + 1L,
      ratio_migrant_lag = ratio_migrant,
      ratio_eu_lag = ratio_eu,
      ratio_non_eu_lag = ratio_non_eu,
      loo_migrant_lag = loo_migrant,
      loo_eu_lag = loo_eu,
      loo_non_eu_lag = loo_non_eu,
      d_annual_ratio_migrant_prev = d_annual_ratio_migrant,
      shift_share_annual_migrant_prev = shift_share_annual_migrant
    )
  rebuilt <- rebuilt %>%
    left_join(previous, by = c("residence", "year"))
  if (nrow(rebuilt) != nrow(a_reg)) {
    stop("Lag rebuild changed the annual row count.", call. = FALSE)
  }
  for (group in ratio_names) {
    d_hat <- rebuilt[[paste0("ratio_", group)]] - rebuilt[[paste0("ratio_", group, "_lag")]]
    gaps <- rel_gaps(rebuilt[[paste0("d_annual_ratio_", group)]], d_hat)
    add_check(
      paste("d_annual_ratio_", group, " is the previous calendar year", sep = ""),
      TRUE, length(gaps), max(gaps), 1e-6,
      "level[t] - level[t - 1]. A missing previous year stays missing."
    )
    shift_hat <- rebuilt[[paste0("baseline_ratio_", group)]] *
      (rebuilt[[paste0("loo_", group)]] - rebuilt[[paste0("loo_", group, "_lag")]])
    gaps <- rel_gaps(rebuilt[[paste0("shift_share_annual_", group)]], shift_hat)
    add_check(
      paste("shift_share_annual_", group, " is baseline times the leave-one-out change", sep = ""),
      TRUE, length(gaps), max(gaps), 1e-6,
      "Leave-one-out uses national counts minus the residence, then the ratio."
    )
  }
  l1_gap <- rel_gaps(
    rebuilt$d_annual_ratio_migrant_l1,
    rebuilt$d_annual_ratio_migrant_prev
  )
  add_check(
    "d_annual_ratio_migrant_l1 is the previous year's one-year change",
    TRUE, length(l1_gap), max(l1_gap), 1e-6,
    "This is the annual counterpart of d_yoy_ratio_migrant_l4. It is not d_annual itself."
  )
  shift_l1_gap <- rel_gaps(
    rebuilt$shift_share_annual_migrant_l1,
    rebuilt$shift_share_annual_migrant_prev
  )
  add_check(
    "shift_share_annual_migrant_l1 is the previous year's instrument",
    TRUE, length(shift_l1_gap), max(shift_l1_gap), 1e-6,
    "Stored analogue of shift_share_yoy_migrant_l4. Not used in the annual regression."
  )

  # Recompute the 2017-2019 and 2010-2015 baselines from annual stocks only.
  baseline_window <- function(data, year_min, year_max, suffix) {
    rows <- data %>%
      filter(year >= year_min, year <= year_max) %>%
      group_by(residence) %>%
      summarise(
        migrant = sum(migrant) / sum(native),
        eu = sum(eu) / sum(native),
        non_eu = sum(non_eu) / sum(native),
        .groups = "drop"
      )
    names(rows)[-1] <- paste0(
      c("baseline_ratio_migrant", "baseline_ratio_eu", "baseline_ratio_non_eu"),
      suffix
    )
    rows
  }
  for (spec in list(
    list(2017L, 2019L, ""),
    list(2010L, 2015L, "_2010_2015")
  )) {
    rebuilt_base <- baseline_window(a_reg, spec[[1]], spec[[2]], spec[[3]])
    got <- a_reg %>%
      distinct(residence, across(all_of(paste0(
        c("baseline_ratio_migrant", "baseline_ratio_eu", "baseline_ratio_non_eu"),
        spec[[3]]
      )))) %>%
      left_join(rebuilt_base, by = "residence", suffix = c("", "_hat"))
    for (group in ratio_names) {
      column <- paste0("baseline_ratio_", group, spec[[3]])
      gaps <- rel_gaps(got[[column]], got[[paste0(column, "_hat")]])
      add_check(
        paste(column, "uses only", spec[[1]], "through", spec[[2]]),
        TRUE, length(gaps), max(gaps), 1e-6,
        "Recomputed from annual stocks inside the window. Later years are not used."
      )
    }
  }

  period_gap <- if (
    all(as.character(a_reg$period) == as.character(a_reg$year))
  ) 0 else 1
  add_check(
    "annual period is the calendar year",
    TRUE, nrow(a_reg), period_gap, 0,
    "Year fixed effects use this column. It is not a quarter label."
  )

  reg_columns <- c(
    "d_annual_mean_log_hourpay_uk_born", "residence", "period",
    "d_annual_ratio_migrant", "shift_share_annual_migrant",
    "d_annual_ratio_eu", "d_annual_ratio_non_eu",
    "shift_share_annual_eu", "shift_share_annual_non_eu",
    "d_annual_share_uk_asian", "d_annual_share_uk_black",
    "d_annual_share_uk_chinese", "d_annual_share_uk_eth_other",
    "d_annual_share_uk_degree", "d_annual_share_uk_low_qual",
    "d_annual_mean_age_uk_born"
  )
  sample <- a_reg %>%
    filter(year > 2015) %>%
    filter(if_all(all_of(reg_columns), ~ !is.na(.)))
  sample_gap <- max(abs(a_coef$n_obs - nrow(sample)))
  cluster_gap <- max(abs(a_coef$n_clusters - n_distinct(sample$residence)))
  add_check(
    "annual regression sample is year > 2015 complete cases",
    TRUE, nrow(a_coef), max(sample_gap, cluster_gap), 0,
    paste0(
      nrow(sample), " residence-years and ",
      n_distinct(sample$residence), " residences. n_obs and n_clusters match."
    )
  )
  jaeger_in_regression <- any(grepl("_l1$", a_coef$endogenous))
  add_check(
    "annual regression endogenous variable is the contemporaneous change",
    TRUE, nrow(a_coef), if (jaeger_in_regression) 1 else 0, 0,
    "d_annual_*_l1 is not a regressor."
  )

  # read_csv turns the blank step-0 fixed effect into NA.
  fixed_effects <- notes$fixed_effects
  fixed_effects[is.na(fixed_effects)] <- ""
  formula <- notes$formula
  formula[is.na(formula)] <- ""
  cluster <- notes$cluster
  cluster[is.na(cluster)] <- ""
  collinear <- notes$collinear
  collinear[is.na(collinear)] <- ""
  fe_expected <- ifelse(notes$step == 0, "", "residence + period")
  formula_gap <- if (
    all(cluster == "residence") &&
      all(fixed_effects == fe_expected) &&
      !any(grepl("_l1", formula)) &&
      all(grepl("d_annual_", formula))
  ) 0 else 1
  add_check(
    "annual formulas use year fixed effects and residence clusters",
    TRUE, nrow(notes), formula_gap, 0,
    "Steps 1-4 use residence + period. The Jaeger lag is not in the formula."
  )
  if (any(nzchar(collinear))) {
    message(
      "Collinear variables were recorded: ",
      paste(unique(collinear[nzchar(collinear)]), collapse = " | "),
      "."
    )
  }

  q_coef <- q_coef %>%
    mutate(endogenous_annual = sub("^d_qoq_ratio_", "d_annual_ratio_", endogenous))
  mapped <- all(grepl("^d_annual_ratio_", q_coef$endogenous_annual))
  cmp <- q_coef %>%
    inner_join(
      a_coef,
      by = c("step", "design", "estimator", "endogenous_annual" = "endogenous"),
      suffix = c("_quarterly", "_annual")
    )
  join_ok <- mapped && nrow(cmp) == nrow(q_coef) && nrow(cmp) == nrow(a_coef)
  flags_ok <- join_ok &&
    all(cmp$residence_fe_quarterly == cmp$residence_fe_annual) &&
    all(cmp$ethnicity_quarterly == cmp$ethnicity_annual) &&
    all(cmp$education_quarterly == cmp$education_annual) &&
    all(cmp$age_quarterly == cmp$age_annual) &&
    all(cmp$quarter_fe == cmp$year_fe)
  add_check(
    "quarterly and annual specifications align by step",
    TRUE, nrow(cmp), if (flags_ok) 0 else 1, 0,
    "Residence, ethnicity, education, and age flags match. Quarter FE matches year FE."
  )

  if (join_ok) {
    cmp <- cmp %>%
      transmute(
        comparison = comparison_label,
        step, design, estimator,
        endogenous_quarterly = endogenous,
        endogenous_annual,
        residence_fe = residence_fe_quarterly,
        quarter_fe, year_fe,
        ethnicity = ethnicity_quarterly,
        education = education_quarterly,
        age = age_quarterly,
        coefficient_quarterly, std_error_quarterly, p_value_quarterly,
        n_obs_quarterly, n_clusters_quarterly,
        first_stage_f_quarterly, hausman_p_quarterly,
        coefficient_annual, std_error_annual, p_value_annual,
        n_obs_annual, n_clusters_annual,
        first_stage_f_annual, hausman_p_annual
      ) %>%
      arrange(step, design, estimator, endogenous_annual)
  }

  out_dir <- here("outputs", "annual")
  validation_path <- file.path(out_dir, "validation_against_quarterly.csv")
  compare_path <- file.path(out_dir, "quarterly_vs_annual_coefficients.csv")
  text_path <- file.path(out_dir, "quarterly_vs_annual_coefficients.txt")
  for (path in c(validation_path, compare_path, text_path)) {
    assert_annual_output(path)
  }
  write_csv(checks, validation_path)

  show_num <- function(x) ifelse(is.na(x), "", sprintf("%.4g", x))
  if (join_ok) {
    write_csv(cmp, compare_path, na = "")
    lines <- c(
      paste0("Comparison: ", comparison_label, "."),
      "The quarterly estimates are the main specification. The annual estimates are a robustness check.",
      ""
    )
    for (i in seq_len(nrow(cmp))) {
      row <- cmp[i, ]
      lines <- c(
        lines,
        sprintf(
          "step %s | %s | %s | %s",
          row$step, row$design, row$estimator, row$endogenous_annual
        ),
        sprintf(
          "  quarterly: coef %s  se %s  p %s  n %s  clusters %s  first-stage F %s  hausman p %s",
          show_num(row$coefficient_quarterly), show_num(row$std_error_quarterly),
          show_num(row$p_value_quarterly), row$n_obs_quarterly, row$n_clusters_quarterly,
          show_num(row$first_stage_f_quarterly), show_num(row$hausman_p_quarterly)
        ),
        sprintf(
          "  annual:    coef %s  se %s  p %s  n %s  clusters %s  first-stage F %s  hausman p %s",
          show_num(row$coefficient_annual), show_num(row$std_error_annual),
          show_num(row$p_value_annual), row$n_obs_annual, row$n_clusters_annual,
          show_num(row$first_stage_f_annual), show_num(row$hausman_p_annual)
        ),
        ""
      )
    }
    writeLines(lines, text_path)
    message("Wrote ", compare_path, " and ", text_path, ".")
  }
  message("Wrote ", validation_path, ".")
  failed <- checks$check[!checks$passed]
  if (length(failed) > 0L) {
    stop(
      "Annual validation failed: ", paste(failed, collapse = "; "), ".",
      call. = FALSE
    )
  }
})
