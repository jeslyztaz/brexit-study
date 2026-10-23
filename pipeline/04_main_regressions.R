# UK-born quarter-to-quarter log wages on quarter-to-quarter migrant ratios.
#
# Sample is 2016 Q1 onward. The instrument, shift_share_qoq_*, is the
# 2017-2019 baseline ratio times the leave-one-out national
# quarter-to-quarter change.
#
# Two designs, each estimated by OLS and 2SLS:
#   pooled: one endogenous variable, d_qoq_ratio_migrant
#   eu_non_eu: d_qoq_ratio_eu and d_qoq_ratio_non_eu in one equation
# Controls are added in order: residence and quarter fixed effects,
# ethnicity, education, then age.
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

steps <- data.frame(
  step = 0:4,
  residence_fe = c(FALSE, TRUE, TRUE, TRUE, TRUE),
  quarter_fe = c(FALSE, TRUE, TRUE, TRUE, TRUE),
  ethnicity = c(FALSE, FALSE, TRUE, TRUE, TRUE),
  education = c(FALSE, FALSE, FALSE, TRUE, TRUE),
  age = c(FALSE, FALSE, FALSE, FALSE, TRUE)
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
    if (step$residence_fe != step$quarter_fe) {
      stop("Residence and quarter fixed effects are added together.", call. = FALSE)
    }
    controls <- c(
      if (isTRUE(step$ethnicity)) ethnicity,
      if (isTRUE(step$education)) education,
      if (isTRUE(step$age)) age
    )
    if (is.null(controls)) controls <- character()
    fe <- if (isTRUE(step$residence_fe)) "residence + period" else ""
    ols_rhs <- paste(c(endogenous, controls), collapse = " + ")
    iv_rhs <- if (length(controls) == 0L) "1" else paste(controls, collapse = " + ")

    ols <- feols(fml(ols_rhs, fe, ""), reg, vcov = ~ residence)
    iv <- feols(fml(iv_rhs, fe, iv_text), reg, vcov = ~ residence)
    if (nobs(ols) != nobs(iv) || nobs(ols) != nrow(reg)) {
      stop("A regression dropped rows.", call. = FALSE)
    }

    sample_stats <- fitstat(iv, ~ n + g + wh.p, simplify = FALSE)
    fs <- first_stage_f(iv, endogenous)
    flag <- data.frame(
      step = step$step,
      design = design_name,
      residence_fe = step$residence_fe,
      quarter_fe = step$quarter_fe,
      ethnicity = step$ethnicity,
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
    step, design, estimator, residence_fe, quarter_fe, ethnicity, education, age,
    endogenous, coefficient, std_error, p_value, n_obs, n_clusters,
    first_stage_f, hausman_p
  )

if (nrow(coefs) != 30L) {
  stop("Expected 30 coefficient rows, got ", nrow(coefs), ".", call. = FALSE)
}
hausman_check <- coefs %>%
  group_by(design, step) %>%
  summarise(n_p = n_distinct(hausman_p), .groups = "drop")
if (any(hausman_check$n_p != 1L)) {
  stop("Hausman p-value differs within a design and step.", call. = FALSE)
}

out_dir <- here("outputs")
table_dir <- here("outputs", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
coef_path <- file.path(out_dir, "main_qoq_coefficients.csv")
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
  names(mods) <- c("none", "FE", "+ ethnicity", "+ education", "+ age")
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
