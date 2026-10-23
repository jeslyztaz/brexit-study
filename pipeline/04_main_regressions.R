# UK-born quarter-to-quarter log wages on quarter-to-quarter migrant ratios.
#
# Sample is 2021 Q1 onward (year > 2020). Earlier rows stay in the
# regression file and are removed here. The instrument, shift_share_qoq_*,
# is the 2017-2019 baseline ratio times the leave-one-out national
# quarter-to-quarter change.
#
# Two designs, each estimated by OLS and 2SLS:
#   pooled: one endogenous variable, d_qoq_ratio_migrant
#   eu_non_eu: d_qoq_ratio_eu and d_qoq_ratio_non_eu in one equation
# Controls are added in order: residence and year fixed effects, ethnicity,
# the 2020 COVID mortality rate interacted with year, education, then age.
# The last specification keeps those controls and uses period fixed effects
# in place of year fixed effects. Period is the calendar quarter.
#
# The COVID control is one 2020 age-standardised mortality rate per
# residence, joined from ons_covid_by_area_20.csv. It is not written back
# to the regression file. 2021 is the omitted year.
#
# Standard errors are clustered by residence. That allows any within-area
# serial correlation, so a Newey-West term is not added.
# hausman_p is the Wu-Hausman test that the endogenous regressors are
# exogenous. It is joint when EU and non-EU are both endogenous. It is not
# a test of the instrument.

source("setup.R")

outcome <- "d_qoq_mean_log_hourpay_uk_born"
ethnicity <- c(
  "d_qoq_share_uk_asian", "d_qoq_share_uk_black", "d_qoq_share_uk_chinese", "d_qoq_share_uk_eth_other"
)
education <- c("d_qoq_share_uk_degree", "d_qoq_share_uk_low_qual")
age <- "d_qoq_mean_age_uk_born"

data_file <- here("data", "processed", "residence_quarter_regression.csv")
if (!file.exists(data_file)) {
  stop("Cannot find ", data_file, ". Run pipeline/02_construct_variables.R first.")
}

reg <- read_csv(data_file, show_col_types = FALSE) %>%
  filter(year > 2020) %>%
  filter(if_all(
    all_of(c(
      outcome, "residence", "period",
      "d_qoq_ratio_migrant", "shift_share_qoq_migrant",
      "d_qoq_ratio_eu", "d_qoq_ratio_non_eu",
      "shift_share_qoq_eu", "shift_share_qoq_non_eu",
      ethnicity, education, age
    )),
    ~ !is.na(.)
  ))

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
reg <- reg %>%
  mutate(residence = as.character(residence), year = as.integer(year)) %>%
  left_join(covid, by = "residence")
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
  step = 0:6,
  residence_fe = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  year_fe = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  period_fe = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
  ethnicity = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
  covid = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
  education = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
  age = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)
step_labels <- c(
  "none", "year FE", "+ ethnicity", "+ covid", "+ education", "+ age", "period FE"
)

designs <- list(
  pooled = list(
    endogenous = "d_qoq_ratio_migrant",
    instrument = "shift_share_qoq_migrant"
  ),
  eu_non_eu = list(
    endogenous = c("d_qoq_ratio_eu", "d_qoq_ratio_non_eu"),
    instrument = c("shift_share_qoq_eu", "shift_share_qoq_non_eu")
  )
)

# fixest uses | to separate controls, fixed effects, and the IV.
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
      stop("Missing coefficient ", name, ".", call. = FALSE)
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

rows <- list()
models <- list()

for (design_name in names(designs)) {
  design <- designs[[design_name]]
  endogenous <- design$endogenous
  iv_text <- paste(
    paste(endogenous, collapse = " + "), "~",
    paste(design$instrument, collapse = " + ")
  )

  for (i in seq_len(nrow(steps))) {
    step <- steps[i, ]
    if (isTRUE(step$year_fe) && isTRUE(step$period_fe)) {
      stop("Year and period fixed effects are not used together.", call. = FALSE)
    }
    if (isTRUE(step$residence_fe) != (isTRUE(step$year_fe) || isTRUE(step$period_fe))) {
      stop("Residence fixed effects enter with the time fixed effects.", call. = FALSE)
    }
    if (isTRUE(step$period_fe) && !isTRUE(step$age)) {
      stop(
        "Period fixed effects replace year fixed effects only after every control is included.",
        call. = FALSE
      )
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
    fe <- if (isTRUE(step$period_fe)) {
      "residence + period"
    } else if (isTRUE(step$year_fe)) {
      "residence + year"
    } else {
      ""
    }
    ols_rhs <- paste(c(endogenous, controls), collapse = " + ")
    iv_rhs <- if (length(controls) == 0L) "1" else paste(controls, collapse = " + ")

    ols <- feols(fml(ols_rhs, fe, ""), reg, vcov = ~ residence)
    iv <- feols(fml(iv_rhs, fe, iv_text), reg, vcov = ~ residence)
    if (nobs(ols) != nobs(iv) || nobs(ols) != nrow(reg)) {
      stop("A regression dropped rows.", call. = FALSE)
    }
    if (isTRUE(step$covid)) {
      dropped <- unique(c(ols$collin.var, iv$collin.var))
      dropped <- dropped[dropped %in% covid_terms]
      if (length(dropped) > 0L) {
        stop(
          "COVID controls were dropped as collinear: ",
          paste(dropped, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
    }

    sample_stats <- fitstat(iv, ~ n + g + wh.p, simplify = FALSE)
    fs <- first_stage_f(iv, endogenous)
    flag <- data.frame(
      step = step$step,
      design = design_name,
      residence_fe = step$residence_fe,
      year_fe = step$year_fe,
      period_fe = step$period_fe,
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
    }
  }
}

coefs <- bind_rows(rows) %>%
  select(
    step, design, estimator, residence_fe, year_fe, period_fe, ethnicity, covid,
    education, age,
    endogenous, coefficient, std_error, p_value, t_df, n_obs, n_clusters,
    first_stage_f, hausman_p
  )

# 7 steps, 2 estimators, pooled (1 coefficient) and EU/non-EU (2 coefficients).
if (nrow(coefs) != 42L) {
  stop("Expected 42 coefficient rows, got ", nrow(coefs), ".", call. = FALSE)
}
hausman_check <- coefs %>%
  group_by(design, step) %>%
  summarise(n_p = n_distinct(hausman_p), .groups = "drop")
if (any(hausman_check$n_p != 1L)) {
  stop("Hausman p-value differs within a design and step.", call. = FALSE)
}

table_dir <- here("outputs", "regressions", "quarterly", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
coef_path <- output_file(
  "regressions", "quarterly", "coefficients", "main_qoq_coefficients.csv"
)
write_csv(coefs, coef_path, na = "")

show_num <- function(x) ifelse(is.na(x), "", sprintf("%.4g", x))

write_one_table <- function(design_name, estimator_name) {
  kept <- Filter(function(item) {
    item$design == design_name && item$estimator == estimator_name
  }, models)
  iv_rows <- coefs[coefs$design == design_name & coefs$estimator == "2SLS", ]
  iv_rows <- iv_rows[order(iv_rows$step, iv_rows$endogenous), ]
  endos <- unique(iv_rows$endogenous)
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
  # 2SLS prints the first stage and the second stage. One endogenous
  # variable adds one first-stage column; two endogenous variables add two.
  n_per <- if (estimator_name == "2SLS") length(endos) + 1L else 1L
  expand <- function(x) rep(x, each = n_per)
  # fitstat = NA drops the default observation line so n_obs is not repeated.
  etable(
    mods,
    stage = if (estimator_name == "2SLS") 1:2 else 2,
    dict = c(
      fit_d_qoq_ratio_migrant = "d_qoq_ratio_migrant",
      fit_d_qoq_ratio_eu = "d_qoq_ratio_eu",
      fit_d_qoq_ratio_non_eu = "d_qoq_ratio_non_eu"
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
    file = file.path(
      table_dir,
      paste0(if (design_name == "pooled") "pooled" else "eu_non_eu", "_", tolower(estimator_name), ".txt")
    ),
    replace = TRUE
  )
}

# Wide enough for each control step and, in the 2SLS tables, both stages.
options(width = 900)
for (design_name in names(designs)) {
  write_one_table(design_name, "OLS")
  write_one_table(design_name, "2SLS")
}

message(
  "Wrote ", nrow(coefs), " rows to ", coef_path,
  " and four tables to ", table_dir, "."
)
