# Residence-by-year panel from the same quarterly LFS person files as
# build_residence_quarter_panel().
#
# This is the annual robustness check. It does not replace the quarterly
# panel. One row is one URESMC residence in one calendar year, and only a
# year with Q1, Q2, Q3, and Q4 is kept.
#
# Population stocks are the arithmetic average of the four quarterly
# weighted stocks. Each quarter's person weight already grosses up to that
# quarter's population, so the quarters are not added. Dividing the person
# and income weights by four and then summing is that average. Means,
# shares, and weighted medians are ratios, so the same rescaling leaves
# them equal to the statistic pooled over the four quarters' eligible
# person-quarters. That is not the unweighted average of the four quarterly
# means, shares, or medians.
#
# The LFS interviews respondents for five consecutive quarters, so the same
# person can appear in more than one quarter of a calendar year. Those
# records are not linked. A repeated person does not add a second copy of
# the population, because the annual stock averages the quarterly stocks.
# The same person can contribute more than one observation to a mean,
# share, or median, each with that quarter's weight. Counts such as n and
# n_pay_uk_born are pooled person-quarter counts, not unique people and not
# averages of the quarterly counts.
#
# Source functions/lfs_panel.R first.

stock_columns <- c(
  "pop", "pop_uk_born", "pop_eu_born", "pop_non_eu_born",
  "pop_birthplace_unknown", "pop_pay"
)

one_chr <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) return(NA_character_)
  as.character(x[[1L]])
}

paste_unique <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "NA"
  paste(unique(x), collapse = "|")
}

paste_by_quarter <- function(quarter, x) {
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  paste(sprintf("Q%s:%s", quarter, x), collapse = ";")
}

empty_overlap <- function() {
  list(
    compared = NA_character_, n = NA_integer_,
    n_disagree = NA_integer_, n_crown = NA_integer_
  )
}

sum_or_na <- function(x) {
  x <- as.numeric(x)
  if (!any(is.finite(x))) return(NA_real_)
  sum(x[is.finite(x)])
}

# Ages 16-64 with a residence. The same cut as aggregate_residence_quarter().
aged_people <- function(df) {
  if (!"AGE" %in% names(df) || !"RESIDENCE_LABEL" %in% names(df)) {
    stop("Person file has no AGE or RESIDENCE_LABEL column.", call. = FALSE)
  }
  aged <- df[!is.na(df$AGE) & df$AGE >= 16 & df$AGE <= 64, , drop = FALSE]
  aged[!is.na(aged$RESIDENCE_LABEL), , drop = FALSE]
}

# Indian, Pakistani, and Bangladeshi flags for the schemes that separate
# them. ETHCEN6 and ETH01 have one Asian code, so the flags stay missing,
# matching residence_stats().
ethnicity_detail_flags <- function(code, origin) {
  maps <- list(
    ETHUKEUL = c(indian = 3, pakistani = 4, bangladeshi = 5),
    ETHUK14 = c(indian = 6, pakistani = 7, bangladeshi = 8),
    ETHUK8 = c(indian = 3, pakistani = 4, bangladeshi = 5)
  )
  n <- length(code)
  blank <- rep(NA, n)
  empty <- list(
    eth_indian = blank, eth_pakistani = blank, eth_bangladeshi = blank,
    eth_indian_uk = blank, eth_pakistani_uk = blank, eth_bangladeshi_uk = blank
  )
  if (length(origin) != 1L || is.na(origin) || !origin %in% names(maps)) {
    return(empty)
  }
  lut <- maps[[origin]]
  known <- !is.na(code)
  flag_one <- function(value, among) {
    out <- rep(NA, n)
    keep <- known & among
    keep[is.na(keep)] <- FALSE
    out[keep] <- code[keep] == value
    out
  }
  uk <- rep(TRUE, n)
  list(
    eth_indian = flag_one(unname(lut[["indian"]]), uk),
    eth_pakistani = flag_one(unname(lut[["pakistani"]]), uk),
    eth_bangladeshi = flag_one(unname(lut[["bangladeshi"]]), uk)
  )
}

add_detail_flags <- function(df, origin) {
  code <- if ("ETHNICITY" %in% names(df)) df$ETHNICITY else rep(NA_real_, nrow(df))
  flags <- ethnicity_detail_flags(code, origin)
  uk <- df$birth3 == "UK"
  uk[is.na(uk)] <- FALSE
  for (nm in c("eth_indian", "eth_pakistani", "eth_bangladeshi")) {
    df[[nm]] <- flags[[nm]]
    uk_flag <- flags[[nm]]
    uk_flag[!uk] <- NA
    df[[paste0(nm, "_uk")]] <- uk_flag
  }
  df
}

assert_close <- function(actual, expected, label, tol = 1e-6) {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  if (length(actual) != length(expected)) {
    stop(label, " has a different length.", call. = FALSE)
  }
  actual_ok <- is.finite(actual)
  expected_ok <- is.finite(expected)
  if (any(actual_ok != expected_ok)) {
    stop(label, " is not finite on the same rows.", call. = FALSE)
  }
  if (!any(actual_ok)) return(0)
  scale <- pmax(1, abs(actual[actual_ok]), abs(expected[expected_ok]))
  gap <- abs(actual[actual_ok] - expected[expected_ok]) / scale
  if (any(gap > tol)) {
    stop(
      label, " is outside tolerance. Largest relative gap ", max(gap), ".",
      call. = FALSE
    )
  }
  max(gap)
}

# Which calendar years have all four quarters. An incomplete year, including
# a year with no file inside the span, is reported and not treated as complete.
calendar_year_coverage <- function(files) {
  if (!all(c("year", "quarter") %in% names(files))) {
    stop("File list needs year and quarter.", call. = FALSE)
  }
  if (nrow(files) == 0L) stop("No quarterly files.", call. = FALSE)
  if (any(duplicated(files[c("year", "quarter")]))) {
    stop("A year-quarter appears more than once.", call. = FALSE)
  }
  years <- seq.int(min(files$year), max(files$year))
  rows <- lapply(years, function(yr) {
    have <- sort(unique(files$quarter[files$year == yr]))
    complete <- identical(as.integer(have), 1:4)
    reason <- if (complete) {
      "complete calendar year"
    } else if (length(have) == 0L) {
      "no quarterly file"
    } else {
      paste0(
        "incomplete calendar year; present quarters: ",
        paste(have, collapse = ", ")
      )
    }
    tibble(
      year = as.integer(yr),
      n_quarters = length(have),
      quarters = if (length(have) == 0L) "" else paste(have, collapse = ","),
      included = complete,
      reason = reason
    )
  })
  bind_rows(rows)
}

validate_quarter_inputs <- function(quarters, year) {
  if (length(quarters) != 4L) {
    stop("A calendar year needs four quarters.", call. = FALSE)
  }
  qs <- vapply(quarters, function(q) as.integer(q$quarter), integer(1))
  ys <- vapply(quarters, function(q) as.integer(q$info$year), integer(1))
  if (!identical(sort(qs), 1:4)) {
    stop("Quarters must be 1, 2, 3, and 4 exactly once.", call. = FALSE)
  }
  if (length(unique(ys)) != 1L || unique(ys) != as.integer(year)) {
    stop("Quarter files are not all in calendar year ", year, ".", call. = FALSE)
  }
  invisible(TRUE)
}

validate_annual_rows <- function(rows) {
  if (nrow(rows) != length(residence_labels)) {
    stop("Expected one row for every residence area.", call. = FALSE)
  }
  if (!identical(rows$residence, residence_labels)) {
    stop("Residence rows are not in URESMC order.", call. = FALSE)
  }
  if (any(duplicated(rows$residence)) || any(rows$n_quarters != 4L)) {
    stop("Annual rows are not one complete year per residence.", call. = FALSE)
  }
  counts <- rows[c(
    "pop", "pop_uk_born", "pop_eu_born", "pop_non_eu_born", "pop_birthplace_unknown"
  )]
  if (any(!is.finite(as.matrix(counts))) || any(as.matrix(counts) < 0)) {
    stop("Annual population stocks must be finite and non-negative.", call. = FALSE)
  }
  total <- rows$pop_uk_born + rows$pop_eu_born + rows$pop_non_eu_born +
    rows$pop_birthplace_unknown
  assert_close(total, rows$pop, "Annual birthplace partition of pop", tol = 1e-8)
  known <- rows$pop_uk_born + rows$pop_eu_born + rows$pop_non_eu_born
  expect_share <- ifelse(known > 0, rows$pop_uk_born / known, NA_real_)
  assert_close(rows$share_birth_uk, expect_share, "Annual UK birth share", tol = 1e-8)
  expect_eu <- ifelse(known > 0, rows$pop_eu_born / known, NA_real_)
  expect_non <- ifelse(known > 0, rows$pop_non_eu_born / known, NA_real_)
  assert_close(rows$share_birth_eu, expect_eu, "Annual EU birth share", tol = 1e-8)
  assert_close(rows$share_birth_non_eu, expect_non, "Annual non-EU birth share", tol = 1e-8)
  expect_rate <- ifelse(rows$pop > 0, rows$pop_birthplace_unknown / rows$pop, NA_real_)
  assert_close(
    rows$birthplace_missing_rate, expect_rate,
    "Annual birthplace missing rate", tol = 1e-8
  )
  invisible(TRUE)
}

# Pool four cleaned quarterly person files into residence-year rows.
# `quarters` is a list of four quarter inputs from read_lfs_quarter() or
# the test helper. Weights on the input files are not modified.
aggregate_residence_year <- function(quarters, year) {
  validate_quarter_inputs(quarters, year)
  quarters <- quarters[order(vapply(quarters, function(q) q$quarter, integer(1)))]
  year <- as.integer(year)

  scaled <- lapply(quarters, function(q) {
    aged <- q$lfs[!is.na(q$lfs$AGE) & q$lfs$AGE >= 16 & q$lfs$AGE <= 64, , drop = FALSE]
    n_missing <- sum(is.na(aged$RESIDENCE_LABEL))
    if (n_missing > 0L) {
      message(
        q$source_file, ": ", n_missing,
        " people aged 16-64 have no residence and are left out."
      )
    }
    df <- aged[!is.na(aged$RESIDENCE_LABEL), , drop = FALSE]
    if (!"WEIGHT" %in% names(df)) {
      stop(q$source_file, " has no WEIGHT column.", call. = FALSE)
    }
    df <- add_detail_flags(df, one_chr(q$eth_origin))
    # Average of the four quarterly stocks. Ratios are unchanged.
    df$WEIGHT <- df$WEIGHT / 4
    if ("INCOME_WEIGHT" %in% names(df)) df$INCOME_WEIGHT <- df$INCOME_WEIGHT / 4
    df
  })
  pooled <- bind_rows(scaled)

  origins <- vapply(quarters, function(q) one_chr(q$eth_origin), character(1))
  schemes <- vapply(quarters, function(q) one_chr(q$occ_scheme), character(1))
  sources <- vapply(quarters, function(q) one_chr(q$birthplace_source), character(1))
  source_schemes <- vapply(quarters, function(q) one_chr(q$birthplace_scheme), character(1))
  weights <- vapply(quarters, function(q) one_chr(q$person_weight), character(1))
  income_weights <- vapply(quarters, function(q) one_chr(q$income_weight), character(1))
  q_numbers <- vapply(quarters, function(q) as.integer(q$quarter), integer(1))
  if (length(unique(origins)) > 1L) {
    message(
      "Year ", year, " pools ethnicity sources: ", paste(origins, collapse = " | "),
      ". Six-group shares use the harmonised groups. Indian, Pakistani, and ",
      "Bangladeshi shares pool the quarter-specific codes."
    )
  }
  if (length(unique(sources)) > 1L) {
    message(
      "Year ", year, " pools birthplace sources: ", paste(sources, collapse = " | "),
      "."
    )
  }
  mixed_occ <- length(unique(schemes)) != 1L
  occ_scheme <- if (mixed_occ) paste(unique(schemes), collapse = "|") else schemes[[1L]]
  if (mixed_occ) {
    message(
      "Year ", year, " mixes occupation schemes (", occ_scheme,
      "). Occupation shares are left missing."
    )
  }

  overlap_n <- sum_or_na(vapply(quarters, function(q) q$overlap$n, numeric(1)))
  overlap_disagree <- sum_or_na(vapply(quarters, function(q) q$overlap$n_disagree, numeric(1)))
  overlap_crown <- sum_or_na(vapply(quarters, function(q) q$overlap$n_crown, numeric(1)))
  overlap_var <- paste_unique(vapply(quarters, function(q) one_chr(q$overlap$compared), character(1)))

  rows <- lapply(seq_along(residence_labels), function(i) {
    area <- residence_labels[[i]]
    df <- pooled[as.character(pooled$RESIDENCE_LABEL) == area, , drop = FALSE]
    stats <- residence_stats(df, NA_character_, if (mixed_occ) NA_character_ else occ_scheme)
    stats$share_eth_indian <- wshare(df$eth_indian, df$WEIGHT, TRUE)
    stats$share_eth_pakistani <- wshare(df$eth_pakistani, df$WEIGHT, TRUE)
    stats$share_eth_bangladeshi <- wshare(df$eth_bangladeshi, df$WEIGHT, TRUE)
    stats$share_eth_indian_uk_born <- wshare(df$eth_indian_uk, df$WEIGHT, TRUE)
    stats$share_eth_pakistani_uk_born <- wshare(df$eth_pakistani_uk, df$WEIGHT, TRUE)
    stats$share_eth_bangladeshi_uk_born <- wshare(df$eth_bangladeshi_uk, df$WEIGHT, TRUE)
    if (mixed_occ) {
      for (k in 1:9) stats[[paste0("share_occ_", k)]] <- NA_real_
      stats$occ_scheme <- occ_scheme
    }
    bind_cols(
      tibble(
        residence = area,
        residence_code = i,
        year = year,
        n_quarters = 4L,
        source_file = paste(vapply(quarters, function(q) q$source_file, character(1)), collapse = " | "),
        person_weight = paste_unique(weights),
        income_weight = paste_unique(income_weights),
        birthplace_source = paste_unique(sources),
        birthplace_scheme = paste_unique(source_schemes),
        birthplace_source_by_quarter = paste_by_quarter(q_numbers, sources),
        birthplace_scheme_by_quarter = paste_by_quarter(q_numbers, source_schemes),
        eth_origin_by_quarter = paste_by_quarter(q_numbers, origins),
        occ_scheme_by_quarter = paste_by_quarter(q_numbers, schemes),
        birth_overlap_variable = overlap_var,
        birth_overlap_n = overlap_n,
        birth_overlap_disagree_n = overlap_disagree,
        birth_overlap_crown_n = overlap_crown
      ),
      stats
    )
  })
  out <- bind_rows(rows)
  validate_annual_rows(out)
  out
}

group_sum <- function(residence, value, weight) {
  ok <- is.finite(value) & is.finite(weight) & weight > 0
  if (!any(ok)) {
    return(tibble(residence = character(), w = numeric(), wx = numeric()))
  }
  # wt and value stay distinct from the summarised names. summarise() can
  # see a name it has just created, which would turn w * x into (sum w) * x.
  tibble(residence = as.character(residence[ok]), wt = weight[ok], value = value[ok]) %>%
    group_by(residence) %>%
    summarise(w = sum(wt), wx = sum(wt * value), .groups = "drop")
}

fill_parts <- function(parts) {
  base <- tibble(residence = residence_labels)
  out <- left_join(base, parts, by = "residence")
  out$w[is.na(out$w)] <- 0
  out$wx[is.na(out$wx)] <- 0
  out$mean <- ifelse(out$w > 0, out$wx / out$w, NA_real_)
  out
}

age_parts <- function(df) {
  aged <- aged_people(df)
  w <- if ("WEIGHT" %in% names(aged)) as.numeric(aged$WEIGHT) else rep(NA_real_, nrow(aged))
  age <- if ("AGE" %in% names(aged)) as.numeric(aged$AGE) else rep(NA_real_, nrow(aged))
  uk <- aged$birth3 == "UK"
  ok <- !is.na(w) & w > 0 & uk %in% TRUE & !is.na(age)
  ok[is.na(ok)] <- FALSE
  group_sum(aged$RESIDENCE_LABEL, age, ifelse(ok, w, NA_real_))
}

uk_log_pay_parts <- function(df) {
  aged <- aged_people(df)
  iw <- if ("INCOME_WEIGHT" %in% names(aged)) as.numeric(aged$INCOME_WEIGHT) else rep(NA_real_, nrow(aged))
  hourpay <- if ("HOURPAY" %in% names(aged)) as.numeric(aged$HOURPAY) else rep(NA_real_, nrow(aged))
  status <- if ("STATR_LABEL" %in% names(aged)) aged$STATR_LABEL else rep(NA_character_, nrow(aged))
  uk <- aged$birth3 == "UK"
  employee <- status == "Employee"
  ok <- employee %in% TRUE & uk %in% TRUE & !is.na(hourpay) & hourpay > 0 &
    !is.na(iw) & iw > 0
  ok[is.na(ok)] <- FALSE
  log_pay <- rep(NA_real_, length(hourpay))
  log_pay[ok] <- log(hourpay[ok])
  group_sum(aged$RESIDENCE_LABEL, log_pay, ifelse(ok, iw, NA_real_))
}

# Annual rows against the quarterly aggregation of the same person files.
# Stocks must match the average of the quarterly stocks, not the sum.
# Counts must match the sum of the quarterly counts. Means must match the
# weight-pooled quarterly sufficient statistics.
check_annual_year <- function(quarters, annual_rows, year) {
  validate_quarter_inputs(quarters, year)
  quarters <- quarters[order(vapply(quarters, function(q) q$quarter, integer(1)))]
  q_panels <- lapply(quarters, function(q) {
    suppressMessages(aggregate_residence_quarter(q$lfs, q$info))
  })
  q_panel <- bind_rows(q_panels)
  if (any(q_panel$year != as.integer(year))) {
    stop("Quarterly check rows are not in year ", year, ".", call. = FALSE)
  }

  # Name the mean and the sum explicitly. A later summarise expression
  # would otherwise see the mean that across() has just created.
  avg <- q_panel %>%
    group_by(residence) %>%
    summarise(
      across(
        all_of(stock_columns),
        list(avg = ~ mean(.), total = ~ sum(.)),
        .names = "{.col}_{.fn}"
      ),
      n_sum = sum(n),
      n_pay_uk_sum = sum(n_pay_uk_born),
      .groups = "drop"
    )
  got <- annual_rows %>%
    select(residence, all_of(stock_columns), n, n_pay_uk_born)
  cmp <- left_join(got, avg, by = "residence")
  if (nrow(cmp) != nrow(annual_rows) || anyNA(cmp$pop_avg)) {
    stop("Annual and quarterly residence sets differ in ", year, ".", call. = FALSE)
  }
  gaps <- c(
    pop = assert_close(cmp$pop, cmp$pop_avg, paste(year, "pop")),
    pop_uk = assert_close(cmp$pop_uk_born, cmp$pop_uk_born_avg, paste(year, "UK-born pop")),
    pop_eu = assert_close(cmp$pop_eu_born, cmp$pop_eu_born_avg, paste(year, "EU-born pop")),
    pop_non_eu = assert_close(
      cmp$pop_non_eu_born, cmp$pop_non_eu_born_avg, paste(year, "non-EU-born pop")
    ),
    pop_unknown = assert_close(
      cmp$pop_birthplace_unknown, cmp$pop_birthplace_unknown_avg,
      paste(year, "unknown-birthplace pop")
    ),
    pop_pay = assert_close(cmp$pop_pay, cmp$pop_pay_avg, paste(year, "pay weight"))
  )
  closer_to_sum <- cmp$pop_total > 0 &
    abs(cmp$pop - cmp$pop_avg) >= abs(cmp$pop - cmp$pop_total) - 1e-6
  if (any(closer_to_sum)) {
    stop(
      "Annual population in ", year,
      " is closer to the sum of quarterly stocks than to their average.",
      call. = FALSE
    )
  }
  if (any(as.numeric(cmp$n) != as.numeric(cmp$n_sum)) ||
      any(as.numeric(cmp$n_pay_uk_born) != as.numeric(cmp$n_pay_uk_sum))) {
    stop(
      "Annual observation counts in ", year,
      " are not the sum of the quarterly person-quarter counts.",
      call. = FALSE
    )
  }

  age_q <- lapply(quarters, function(q) age_parts(q$lfs))
  age_gap <- 0
  age_weight_gap <- 0
  for (i in seq_along(quarters)) {
    parts <- fill_parts(age_q[[i]])
    panel_i <- q_panels[[i]] %>% select(residence, mean_age_uk_born, pop_uk_born)
    merged <- left_join(parts, panel_i, by = "residence")
    age_gap <- max(age_gap, assert_close(
      merged$mean, merged$mean_age_uk_born, paste(year, "Q", i, "mean age")
    ))
    age_weight_gap <- max(age_weight_gap, assert_close(
      merged$w, merged$pop_uk_born, paste(year, "Q", i, "age weight")
    ))
  }
  age_pool <- fill_parts(bind_rows(age_q) %>%
    group_by(residence) %>%
    summarise(w = sum(w), wx = sum(wx), .groups = "drop"))
  age_annual <- left_join(
    age_pool,
    annual_rows %>% select(residence, mean_age_uk_born),
    by = "residence"
  )
  age_gap <- max(age_gap, assert_close(
    age_annual$mean, age_annual$mean_age_uk_born, paste(year, "annual mean age")
  ))

  pay_q <- lapply(quarters, function(q) uk_log_pay_parts(q$lfs))
  pay_gap <- 0
  for (i in seq_along(quarters)) {
    parts <- fill_parts(pay_q[[i]])
    panel_i <- q_panels[[i]] %>% select(residence, mean_log_hourpay_uk_born)
    merged <- left_join(parts, panel_i, by = "residence")
    pay_gap <- max(pay_gap, assert_close(
      merged$mean, merged$mean_log_hourpay_uk_born,
      paste(year, "Q", i, "UK-born mean log pay")
    ))
  }
  pay_pool <- fill_parts(bind_rows(pay_q) %>%
    group_by(residence) %>%
    summarise(w = sum(w), wx = sum(wx), .groups = "drop"))
  pay_annual <- left_join(
    pay_pool,
    annual_rows %>% select(residence, mean_log_hourpay_uk_born),
    by = "residence"
  )
  pay_gap <- max(pay_gap, assert_close(
    pay_annual$mean, pay_annual$mean_log_hourpay_uk_born,
    paste(year, "annual UK-born mean log pay")
  ))

  # Flag shares against residence_stats() on each quarter, then against the
  # pooled annual share. This keeps the ETHCEN6 Asian code from being read
  # as Indian.
  detail_names <- c(
    "share_eth_indian", "share_eth_pakistani", "share_eth_bangladeshi",
    "share_eth_indian_uk_born", "share_eth_pakistani_uk_born",
    "share_eth_bangladeshi_uk_born"
  )
  detail_gap <- 0
  detail_pool <- lapply(quarters, function(q) {
    df <- add_detail_flags(aged_people(q$lfs), one_chr(q$eth_origin))
    rows <- lapply(residence_labels, function(area) {
      sub <- df[as.character(df$RESIDENCE_LABEL) == area, , drop = FALSE]
      w <- if (nrow(sub) == 0L) numeric() else sub$WEIGHT
      tibble(
        residence = area,
        share_eth_indian = wshare(sub$eth_indian, w, TRUE),
        share_eth_pakistani = wshare(sub$eth_pakistani, w, TRUE),
        share_eth_bangladeshi = wshare(sub$eth_bangladeshi, w, TRUE),
        share_eth_indian_uk_born = wshare(sub$eth_indian_uk, w, TRUE),
        share_eth_pakistani_uk_born = wshare(sub$eth_pakistani_uk, w, TRUE),
        share_eth_bangladeshi_uk_born = wshare(sub$eth_bangladeshi_uk, w, TRUE),
        w = sum(w[is.finite(w) & w > 0])
      )
    })
    bind_rows(rows)
  })
  for (i in seq_along(quarters)) {
    panel_i <- q_panels[[i]] %>% select(residence, all_of(detail_names))
    merged <- left_join(
      detail_pool[[i]], panel_i, by = "residence", suffix = c("_flag", "_quarter")
    )
    for (nm in detail_names) {
      detail_gap <- max(detail_gap, assert_close(
        merged[[paste0(nm, "_flag")]], merged[[paste0(nm, "_quarter")]],
        paste(year, "Q", i, nm)
      ))
    }
  }
  # Pooled detail share uses the unscaled quarterly weights. The annual row
  # divided every weight by four, which does not change the share.
  detail_weights <- bind_rows(lapply(seq_along(detail_pool), function(i) {
    df <- add_detail_flags(aged_people(quarters[[i]]$lfs), one_chr(quarters[[i]]$eth_origin))
    if (nrow(df) == 0L) return(NULL)
    tibble(
      residence = as.character(df$RESIDENCE_LABEL),
      w_indian = ifelse(!is.na(df$eth_indian) & is.finite(df$WEIGHT) & df$WEIGHT > 0, df$WEIGHT, 0),
      indian = ifelse(df$eth_indian %in% TRUE & is.finite(df$WEIGHT) & df$WEIGHT > 0, df$WEIGHT, 0),
      w_indian_uk = ifelse(!is.na(df$eth_indian_uk) & is.finite(df$WEIGHT) & df$WEIGHT > 0, df$WEIGHT, 0),
      indian_uk = ifelse(df$eth_indian_uk %in% TRUE & is.finite(df$WEIGHT) & df$WEIGHT > 0, df$WEIGHT, 0)
    )
  }))
  if (is.null(detail_weights) || nrow(detail_weights) == 0L) {
    pooled_detail <- tibble(
      residence = residence_labels,
      share_eth_indian = NA_real_,
      share_eth_indian_uk_born = NA_real_
    )
  } else {
    pooled_detail <- detail_weights %>%
      group_by(residence) %>%
      summarise(
        share_eth_indian = ifelse(sum(w_indian) > 0, sum(indian) / sum(w_indian), NA_real_),
        share_eth_indian_uk_born = ifelse(
          sum(w_indian_uk) > 0, sum(indian_uk) / sum(w_indian_uk), NA_real_
        ),
        .groups = "drop"
      )
    pooled_detail <- left_join(
      tibble(residence = residence_labels), pooled_detail, by = "residence"
    )
  }
  detail_annual <- left_join(
    pooled_detail,
    annual_rows %>% select(residence, share_eth_indian, share_eth_indian_uk_born),
    by = "residence",
    suffix = c("_pool", "_annual")
  )
  detail_gap <- max(
    detail_gap,
    assert_close(
      detail_annual$share_eth_indian_annual, detail_annual$share_eth_indian_pool,
      paste(year, "annual Indian share")
    ),
    assert_close(
      detail_annual$share_eth_indian_uk_born_annual,
      detail_annual$share_eth_indian_uk_born_pool,
      paste(year, "annual UK-born Indian share")
    )
  )

  tibble(
    year = as.integer(year),
    max_rel_pop = gaps[["pop"]],
    max_rel_pop_uk = gaps[["pop_uk"]],
    max_rel_pop_eu = gaps[["pop_eu"]],
    max_rel_pop_non_eu = gaps[["pop_non_eu"]],
    max_rel_pop_unknown = gaps[["pop_unknown"]],
    max_rel_pop_pay = gaps[["pop_pay"]],
    max_rel_mean_age_uk = age_gap,
    max_rel_age_weight_vs_pop_uk = age_weight_gap,
    max_rel_mean_log_hourpay_uk = pay_gap,
    max_rel_share_eth_indian = detail_gap
  )
}

read_lfs_quarter <- function(info) {
  file <- lfs_source_name(info$path)
  message("Reading ", info$year, " Q", info$quarter, ": ", file)
  lfs <- read_lfs_microdata(
    info$path, info$pwt_name, info$piwt_name, info$columns[[1]]
  )
  check_survey_quarter(lfs, info$year, info$quarter, file)
  stop_if_codes_present(lfs, file)
  check_labels(lfs, file)
  lfs <- add_derived(lfs, file)
  overlap <- attr(lfs, "birth_overlap")
  if (is.null(overlap)) overlap <- empty_overlap()
  list(
    lfs = lfs,
    info = info,
    quarter = as.integer(info$quarter),
    source_file = file,
    person_weight = as.character(info$pwt_name),
    income_weight = as.character(info$piwt_name),
    eth_origin = source_of(lfs, "ETHNICITY"),
    occ_scheme = source_of(lfs, "OCCUGRP"),
    birthplace_source = one_chr(attr(lfs, "birthplace_source")),
    birthplace_scheme = one_chr(attr(lfs, "birthplace_scheme")),
    overlap = overlap
  )
}

# `years` limits the build to a subset of complete years. The main script
# leaves it unset so every complete calendar year is included.
build_residence_year_panel <- function(root, from_year = NULL, years = NULL) {
  files <- list_lfs_quarter_files(root, from_year = from_year)
  coverage <- calendar_year_coverage(files)
  excluded <- coverage[!coverage$included, ]
  if (nrow(excluded) > 0L) {
    message(
      "Excluding incomplete calendar years: ",
      paste(sprintf("%s (%s)", excluded$year, excluded$reason), collapse = "; "),
      "."
    )
  } else {
    message("Every calendar year in the file window has four quarters.")
  }
  complete_years <- coverage$year[coverage$included]
  if (!is.null(years)) {
    years <- as.integer(years)
    missing <- setdiff(years, complete_years)
    if (length(missing) > 0L) {
      stop(
        "Requested years are not complete calendar years: ",
        paste(missing, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    message(
      "Partial annual build for ", paste(years, collapse = ", "),
      ". The main annual panel uses every complete calendar year."
    )
    complete_years <- years
  }
  complete_years <- sort(as.integer(complete_years))
  if (length(complete_years) == 0L) {
    stop("No complete calendar year to aggregate.", call. = FALSE)
  }

  panels <- vector("list", length(complete_years))
  checks <- vector("list", length(complete_years))
  for (i in seq_along(complete_years)) {
    yr <- complete_years[[i]]
    message("Aggregating calendar year ", yr)
    subset <- files[files$year == yr, ]
    subset <- subset[order(subset$quarter), ]
    quarters <- lapply(seq_len(nrow(subset)), function(j) read_lfs_quarter(subset[j, ]))
    panels[[i]] <- aggregate_residence_year(quarters, yr)
    checks[[i]] <- check_annual_year(quarters, panels[[i]], yr)
    message(
      "Year ", yr, " matches the average of its quarterly stocks. ",
      "Largest population relative gap ",
      format(checks[[i]]$max_rel_pop, digits = 3), "."
    )
    rm(quarters)
    gc(verbose = FALSE)
  }

  panel <- bind_rows(panels) %>% arrange(year, residence_code)
  if (anyDuplicated(panel[c("residence", "year")]) > 0L) {
    stop("Duplicate residence-year rows.", call. = FALSE)
  }
  if (nrow(panel) != length(complete_years) * length(residence_labels)) {
    stop("Annual panel does not have one row per residence and complete year.", call. = FALSE)
  }
  panel$year <- as.integer(panel$year)
  n_before <- nrow(panel)
  covid_path <- "data/processed/ons_covid_by_area_20.csv"
  if (!file.exists(covid_path)) {
    stop("Cannot find ", covid_path, ". Run from the project root.", call. = FALSE)
  }
  panel <- add_covid_info(panel)
  if (nrow(panel) != n_before) {
    stop("COVID join changed the annual row count.", call. = FALSE)
  }
  attr(panel, "year_coverage") <- coverage
  attr(panel, "aggregation_checks") <- bind_rows(checks)
  panel
}

# Same cell-size report as summarise_native_pay_sample(), for residence-years.
# n_pay_uk_born is the pooled person-quarter count.
summarise_native_pay_sample_annual <- function(panel) {
  if ("quarter" %in% names(panel)) {
    stop("Annual pay summary received a quarterly panel.", call. = FALSE)
  }
  needed <- c("residence", "year", "n_pay_uk_born", "mean_log_hourpay_uk_born")
  missing <- setdiff(needed, names(panel))
  if (length(missing) > 0L) {
    stop("Panel is missing ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  n <- as.numeric(panel$n_pay_uk_born)
  wage <- as.numeric(panel$mean_log_hourpay_uk_born)
  if (any(!is.finite(n) | n < 0)) {
    stop("n_pay_uk_born must be finite and non-negative.", call. = FALSE)
  }
  cells <- length(n)
  missing_wage <- !is.finite(wage)
  below <- function(k) sum(n < k)
  share <- function(k) if (cells == 0L) NA_real_ else below(k) / cells
  band <- function(flag) sum(missing_wage & flag)
  message(
    "UK-born wage sample: ", cells, " residence-years. ",
    "n_pay_uk_born counts person-quarters in the pooled year, not unique people. ",
    "min ", min(n), ", median ", stats::median(n), ", max ", max(n), "."
  )
  message(
    "Average quarterly cell size, pooled count divided by 4: min ",
    min(n) / 4, ", median ", stats::median(n) / 4, ", max ", max(n) / 4, "."
  )
  message(
    "Cells below 30 / 50 / 100 pooled native wage observations: ",
    below(30), " (", round(100 * share(30), 1), "%), ",
    below(50), " (", round(100 * share(50), 1), "%), ",
    below(100), " (", round(100 * share(100), 1), "%)."
  )
  message(
    "Missing mean_log_hourpay_uk_born by n_pay_uk_born: ",
    "0 = ", band(n == 0),
    ", 1-29 = ", band(n > 0 & n < 30),
    ", 30-49 = ", band(n >= 30 & n < 50),
    ", 50-99 = ", band(n >= 50 & n < 100),
    ", 100+ = ", band(n >= 100), "."
  )
  invisible(list(cells = cells, min = min(n), median = stats::median(n), max = max(n)))
}
