# Annual robustness check of the quarterly wage regression.
#
# This is not the main specification. pipeline/04_main_regressions.R is.
# The outcome is the one-year change in UK-born mean log hourly pay. The
# endogenous regressor is the contemporaneous one-year change in the
# migrant ratio, d_annual_ratio_*, the annual counterpart of d_qoq_ratio_*.
# The instrument is shift_share_annual_*: the 2017-2019 baseline ratio
# times the one-year change in the leave-one-out national ratio.
#
# d_annual_ratio_*_l1 is the previous year's one-year change. It is the
# annual counterpart of d_yoy_*_l4 and is not in this equation, just as
# d_yoy_*_l4 is not in the quarterly equation.
#
# Sample is 2021 onward (year > 2020), on complete calendar years only. An
# incomplete year is already absent from the annual file. Controls are added
# in the same order as the quarterly regression, through age: residence and
# year fixed effects, ethnicity, the 2020 COVID mortality rate interacted
# with year, education, then age. There is no further step. In this file the
# calendar year is the time unit, so year fixed effects are the period
# fixed effects.
#
# The COVID control is one 2020 age-standardised mortality rate per
# residence, joined from ons_covid_by_area_20.csv. It is not written back
# to the regression file. 2021 is the omitted year.
#
# Standard errors are clustered by residence.
# hausman_p is the Wu-Hausman test that the endogenous regressors are
# exogenous. It is joint when EU and non-EU are both endogenous. It is not
# a test of the instrument.

source("setup.R")
source(here("pipeline", "annual_common.R"))

run_guarded({
  outcome <- "d_annual_mean_log_hourpay_uk_born"
  ethnicity <- c(
    "d_annual_share_uk_asian", "d_annual_share_uk_black",
    "d_annual_share_uk_chinese", "d_annual_share_uk_eth_other"
  )
  education <- c("d_annual_share_uk_degree", "d_annual_share_uk_low_qual")
  age <- "d_annual_mean_age_uk_born"

  data_file <- here("data", "processed", "residence_annual_regression.csv")
  if (!file.exists(data_file)) {
    stop(
      "Cannot find ", data_file,
      ". Run pipeline/02_construct_annual_variables.R first.",
      call. = FALSE
    )
  }

  raw <- read_csv(data_file, show_col_types = FALSE)
  if ("quarter" %in% names(raw)) {
    stop("Annual regression file has a quarter column.", call. = FALSE)
  }
  window <- raw %>% filter(year > 2020)
  if (nrow(window) == 0L) {
    stop("No residence-years with year > 2020.", call. = FALSE)
  }
  message(
    "Annual regression window is year > 2020: ",
    min(window$year), "-", max(window$year),
    " (", nrow(window), " residence-years before complete cases)."
  )

  needed <- c(
    outcome, "residence", "year", "period",
    "d_annual_ratio_migrant", "shift_share_annual_migrant",
    "d_annual_ratio_eu", "d_annual_ratio_non_eu",
    "shift_share_annual_eu", "shift_share_annual_non_eu",
    ethnicity, education, age
  )
  missing_cols <- setdiff(needed, names(window))
  if (length(missing_cols) > 0L) {
    stop(
      "Annual regression file is missing ",
      paste(missing_cols, collapse = ", "), ".",
      call. = FALSE
    )
  }
  na_n <- vapply(needed, function(column) sum(is.na(window[[column]])), integer(1))
  na_n <- na_n[na_n > 0L]
  if (length(na_n) > 0L) {
    message(
      "Complete-case filter sees missing values in: ",
      paste(sprintf("%s (%d)", names(na_n), na_n), collapse = ", "),
      "."
    )
  } else {
    message("Complete-case filter sees no missing values in the regression columns.")
  }

  reg <- window %>%
    filter(if_all(all_of(needed), ~ !is.na(.))) %>%
    mutate(
      residence = as.character(residence),
      year = as.integer(year),
      # Calendar-year label. The time fixed effect in the formula is year.
      # fixest treats a variable after | as a factor, including when that
      # column is numeric. It is not a linear time trend.
      period = as.character(year)
    )
  message(
    "Complete cases: ", nrow(reg), " of ", nrow(window),
    " residence-years, ", n_distinct(reg$residence), " residences, ",
    n_distinct(reg$year), " years."
  )
  if (nrow(reg) == 0L) {
    stop("Complete-case sample is empty. The specification was not changed.", call. = FALSE)
  }
  if (any(grepl("_l1$", needed))) {
    stop("The annual regression sample includes a lagged Jaeger column.", call. = FALSE)
  }

  covid_file <- here("data", "processed", "ons_covid_by_area_20.csv")
  if (!file.exists(covid_file)) {
    stop("Cannot find ", covid_file, ".", call. = FALSE)
  }
  covid <- read_csv(covid_file, show_col_types = FALSE) %>%
    transmute(
      residence = as.character(residence),
      covid_asmr_2020 = as.numeric(covid_asmr_2020)
    )
  if (anyDuplicated(covid$residence) > 0L || any(!is.finite(covid$covid_asmr_2020))) {
    stop("COVID file must have one finite 2020 rate per residence.", call. = FALSE)
  }
  n_before <- nrow(reg)
  reg <- reg %>% left_join(covid, by = "residence")
  if (nrow(reg) != n_before) {
    stop("COVID join changed the row count.", call. = FALSE)
  }
  missing_covid <- sort(unique(reg$residence[!is.finite(reg$covid_asmr_2020)]))
  if (length(missing_covid) > 0L) {
    stop(
      "No 2020 COVID rate for: ",
      paste(missing_covid, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (n_distinct(reg$residence) != 20L || min(reg$year) != 2021L) {
    stop("Estimation sample must be all 20 residences from 2021 onward.", call. = FALSE)
  }
  if (n_distinct(reg$residence[reg$year == 2021L]) != n_distinct(reg$residence)) {
    stop("2021 is missing for a residence, so it cannot be the omitted COVID year.", call. = FALSE)
  }
  # 2021 is the omitted year. Residence fixed effects absorb the level of the
  # 2020 rate, and the estimation sample starts in 2021.
  covid_years <- sort(setdiff(unique(reg$year), 2021L))
  covid_terms <- paste0("covid_asmr_2020_y", covid_years)
  for (yr in covid_years) {
    reg[[paste0("covid_asmr_2020_y", yr)]] <- reg$covid_asmr_2020 * (reg$year == yr)
  }

  steps <- data.frame(
    step = 0:5,
    residence_fe = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    year_fe = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
    ethnicity = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
    covid = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
    education = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
    age = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  step_labels <- c(
    "none", "year FE", "+ ethnicity", "+ covid", "+ education", "+ age"
  )

  designs <- list(
    pooled = list(
      endogenous = "d_annual_ratio_migrant",
      instrument = "shift_share_annual_migrant"
    ),
    eu_non_eu = list(
      endogenous = c("d_annual_ratio_eu", "d_annual_ratio_non_eu"),
      instrument = c("shift_share_annual_eu", "shift_share_annual_non_eu")
    )
  )

  fml <- function(rhs, fe, iv) {
    text <- paste(outcome, "~", rhs)
    if (nzchar(fe)) text <- paste(text, "|", fe)
    if (nzchar(iv)) text <- paste(text, "|", iv)
    as.formula(text)
  }

  coef_rows <- function(model, endogenous, estimator) {
    tab <- coeftable(model)
    bind_rows(lapply(endogenous, function(variable) {
      name <- if (estimator == "2SLS") paste0("fit_", variable) else variable
      if (!name %in% rownames(tab)) {
        dropped <- model$collin.var
        detail <- if (is.null(dropped) || length(dropped) == 0L) {
          ""
        } else {
          paste0(" Collinear variables: ", paste(dropped, collapse = ", "), ".")
        }
        stop("Missing coefficient ", name, ".", detail, call. = FALSE)
      }
      data.frame(
        endogenous = variable,
        coefficient = unname(tab[name, "Estimate"]),
        std_error = unname(tab[name, "Std. Error"]),
        p_value = unname(tab[name, "Pr(>|t|)"]),
        t_df = as.numeric(degrees_freedom(model, type = "t"))[1],
        stringsAsFactors = FALSE
      )
    }))
  }

  first_stage_f <- function(model, endogenous) {
    stats <- fitstat(model, "ivf", simplify = FALSE)
    sapply(endogenous, function(variable) {
      key <- paste0("ivf1::", variable)
      if (!key %in% names(stats)) {
        stop("No first-stage F for ", variable, ".", call. = FALSE)
      }
      unname(stats[[key]]$stat)
    })
  }

  collinear_text <- function(model) {
    vars <- model$collin.var
    if (is.null(vars) || length(vars) == 0L) "" else paste(vars, collapse = " | ")
  }

  fit_or_stop <- function(formula, label) {
    tryCatch(
      feols(formula, reg, vcov = ~ residence),
      error = function(e) {
        stop(
          label, " failed. The specification was not changed. ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  rows <- list()
  models <- list()
  notes <- list()

  for (design_name in names(designs)) {
    design <- designs[[design_name]]
    endogenous <- design$endogenous
    iv_text <- paste(
      paste(endogenous, collapse = " + "), "~",
      paste(design$instrument, collapse = " + ")
    )

    for (i in seq_len(nrow(steps))) {
      step <- steps[i, ]
      if (step$residence_fe != step$year_fe) {
        stop("Residence and year fixed effects are added together.", call. = FALSE)
      }
      if (isTRUE(step$education) && !isTRUE(step$covid)) {
        stop("COVID controls enter before education.", call. = FALSE)
      }
      controls <- c(
        if (isTRUE(step$ethnicity)) ethnicity,
        if (isTRUE(step$covid)) covid_terms,
        if (isTRUE(step$education)) education,
        if (isTRUE(step$age)) age
      )
      if (is.null(controls)) controls <- character()
      # year is the calendar year. fixest treats it as a factor after |.
      fe <- if (isTRUE(step$year_fe)) "residence + year" else ""
      ols_rhs <- paste(c(endogenous, controls), collapse = " + ")
      iv_rhs <- if (length(controls) == 0L) "1" else paste(controls, collapse = " + ")
      ols_formula <- fml(ols_rhs, fe, "")
      iv_formula <- fml(iv_rhs, fe, iv_text)
      label <- paste0(design_name, " step ", step$step)

      ols <- fit_or_stop(ols_formula, paste("OLS", label))
      iv <- fit_or_stop(iv_formula, paste("2SLS", label))
      dropped_covid <- unique(c(
        intersect(ols$collin.var, covid_terms),
        intersect(iv$collin.var, covid_terms)
      ))
      if (length(dropped_covid) > 0L) {
        stop(
          label,
          " dropped COVID controls as collinear: ",
          paste(dropped_covid, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
      if (nzchar(collinear_text(ols)) || nzchar(collinear_text(iv))) {
        message(
          label, " collinearity. OLS: ", collinear_text(ols),
          ". 2SLS: ", collinear_text(iv), "."
        )
      }
      if (nobs(ols) != nobs(iv) || nobs(ols) != nrow(reg)) {
        stop("A regression dropped rows.", call. = FALSE)
      }

      sample_stats <- fitstat(iv, ~ n + g + wh.p, simplify = FALSE)
      fs <- first_stage_f(iv, endogenous)
      flag <- data.frame(
        step = step$step,
        design = design_name,
        residence_fe = step$residence_fe,
        year_fe = step$year_fe,
        ethnicity = step$ethnicity,
        covid = step$covid,
        education = step$education,
        age = step$age,
        n_obs = unname(sample_stats$n),
        n_clusters = unname(sample_stats$g),
        hausman_p = unname(sample_stats$wh.p),
        stringsAsFactors = FALSE
      )

      for (estimator in c("OLS", "2SLS")) {
        model <- if (estimator == "OLS") ols else iv
        pulled <- coef_rows(model, endogenous, estimator)
        pulled$first_stage_f <- if (estimator == "OLS") NA_real_ else unname(fs)
        rows[[length(rows) + 1L]] <- bind_cols(flag, pulled)
        rows[[length(rows)]]$estimator <- estimator
        models[[length(models) + 1L]] <- list(
          design = design_name,
          estimator = estimator,
          model = model
        )
        notes[[length(notes) + 1L]] <- data.frame(
          design = design_name,
          estimator = estimator,
          step = step$step,
          formula = paste(
            deparse(if (estimator == "OLS") ols_formula else iv_formula),
            collapse = " "
          ),
          fixed_effects = fe,
          cluster = "residence",
          n_obs = nobs(model),
          collinear = collinear_text(model),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  coefs <- bind_rows(rows) %>%
    select(
      step, design, estimator, residence_fe, year_fe, ethnicity, covid, education, age,
      endogenous, coefficient, std_error, p_value, t_df, n_obs, n_clusters,
      first_stage_f, hausman_p
    )

  # 6 steps, 2 estimators, pooled (1 coefficient) and EU/non-EU (2 coefficients).
  if (nrow(coefs) != 36L) {
    stop("Expected 36 coefficient rows, got ", nrow(coefs), ".", call. = FALSE)
  }
  hausman_check <- coefs %>%
    group_by(design, step) %>%
    summarise(n_p = n_distinct(hausman_p), .groups = "drop")
  if (any(hausman_check$n_p != 1L)) {
    stop("Hausman p-value differs within a design and step.", call. = FALSE)
  }

  table_dir <- here("outputs", "regressions", "annual", "tables")
  coef_path <- here(
    "outputs", "regressions", "annual", "coefficients", "main_annual_coefficients.csv"
  )
  notes_path <- here(
    "outputs", "regressions", "annual", "diagnostics", "estimation_notes.csv"
  )
  assert_annual_output(table_dir)
  assert_annual_output(coef_path)
  assert_annual_output(notes_path)
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(coef_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(notes_path), recursive = TRUE, showWarnings = FALSE)
  assert_annual_output(coef_path)
  assert_annual_output(notes_path)
  write_csv(coefs, coef_path, na = "")
  write_csv(bind_rows(notes), notes_path, na = "")

  show_num <- function(x) ifelse(is.na(x), "", sprintf("%.4g", x))

  write_one_table <- function(design_name, estimator_name) {
    kept <- Filter(function(item) {
      item$design == design_name && item$estimator == estimator_name
    }, models)
    iv_rows <- coefs[coefs$design == design_name & coefs$estimator == "2SLS", ]
    iv_rows <- iv_rows[order(iv_rows$step, iv_rows$endogenous), ]
    endos <- unique(iv_rows$endogenous)
    if (length(endos) == 2L && !identical(
      endos, c("d_annual_ratio_eu", "d_annual_ratio_non_eu")
    )) {
      stop("First-stage labels assume EU then non-EU.", call. = FALSE)
    }
    one <- function(column) {
      vapply(sort(unique(iv_rows$step)), function(step_id) {
        vals <- unique(iv_rows[[column]][iv_rows$step == step_id])
        if (length(vals) != 1L) {
          stop("Expected one ", column, " per step.", call. = FALSE)
        }
        vals
      }, numeric(1))
    }
    first_stage <- lapply(endos, function(variable) {
      vals <- iv_rows$first_stage_f[iv_rows$endogenous == variable]
      if (estimator_name == "OLS") rep("", length(vals)) else show_num(vals)
    })
    names(first_stage) <- if (length(endos) == 1L) {
      "first_stage_f"
    } else {
      c("first_stage_f_eu", "first_stage_f_non_eu")
    }
    mods <- lapply(kept, `[[`, "model")
    if (length(mods) != length(step_labels)) {
      stop("Table columns do not match the control ladder.", call. = FALSE)
    }
    names(mods) <- step_labels
    n_per <- if (estimator_name == "2SLS") length(endos) + 1L else 1L
    expand <- function(x) rep(x, each = n_per)
    table_path <- file.path(
      table_dir,
      paste0(
        if (design_name == "pooled") "pooled" else "eu_non_eu",
        "_", tolower(estimator_name), ".txt"
      )
    )
    assert_annual_output(table_path)
    etable(
      mods,
      stage = if (estimator_name == "2SLS") 1:2 else 2,
      dict = c(
        fit_d_annual_ratio_migrant = "d_annual_ratio_migrant",
        fit_d_annual_ratio_eu = "d_annual_ratio_eu",
        fit_d_annual_ratio_non_eu = "d_annual_ratio_non_eu"
      ),
      fitstat = NA,
      extralines = c(
        list(
          n_obs = expand(as.character(one("n_obs"))),
          n_clusters = expand(as.character(one("n_clusters")))
        ),
        lapply(first_stage, expand),
        list(hausman_p = expand(show_num(one("hausman_p"))))
      ),
      page.width = 900,
      tex = FALSE,
      file = table_path,
      replace = TRUE
    )
  }

  options(width = 900)
  for (design_name in names(designs)) {
    write_one_table(design_name, "OLS")
    write_one_table(design_name, "2SLS")
  }

  message(
    "Wrote ", nrow(coefs), " rows to ", coef_path,
    " and four tables to ", table_dir, "."
  )
})
