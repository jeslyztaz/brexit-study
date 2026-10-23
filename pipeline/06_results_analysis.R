# Figures that need the fitted post-2021 models.
#
# Step 5 is the reference specification: residence and year fixed effects,
# the quarter-to-quarter UK-born ethnicity changes, the 2020 COVID
# mortality rate interacted with year, the quarter-to-quarter UK-born
# education changes, and d_qoq_mean_age_uk_born. The instrument is the
# 2017-2019 baseline times the leave-one-out national quarter-to-quarter
# change.
# The coefficient file from pipeline/04_main_regressions.R is the source
# for the main ladder. Step 5 is refitted here for the partial-regression
# figures and checked against that file. Leave-one-region-out drops rows
# only; the stored instruments are not rebuilt.
#
# Sample-window and four-quarter re-estimates are not run. The reasons
# are written to outputs/metadata/figure_omissions.csv.

source("setup.R")

outcome <- "d_qoq_mean_log_hourpay_uk_born"
ethnicity <- c(
  "d_qoq_share_uk_asian", "d_qoq_share_uk_black",
  "d_qoq_share_uk_chinese", "d_qoq_share_uk_eth_other"
)
education <- c("d_qoq_share_uk_degree", "d_qoq_share_uk_low_qual")
age <- "d_qoq_mean_age_uk_born"
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
step_labels <- c(
  "0  No controls",
  "1  Residence and year FE",
  "2  + ethnicity",
  "3  + COVID x year",
  "4  + education",
  "5  + age (reference)",
  "6  Quarter FE replaces year FE"
)
outcome_labels <- c(
  d_qoq_ratio_migrant = "Pooled model",
  d_qoq_ratio_eu = "Joint model: EU",
  d_qoq_ratio_non_eu = "Joint model: non-EU",
  d_annual_ratio_migrant = "Pooled model",
  d_annual_ratio_eu = "Joint model: EU",
  d_annual_ratio_non_eu = "Joint model: non-EU"
)

write_csv(
  tibble(
    figure = c("R05", "R07", "R08"),
    status = "not produced",
    reason = c(
      "Post-2021 sample-window re-estimates were not run. A window that drops 2021 cannot keep 2021 as the omitted COVID year.",
      "The four-quarter step-5 model was not estimated. The regression file stores the levels and the quarter-to-quarter changes of ethnicity, education, and age. It does not store their four-quarter changes.",
      "No separate specification-stability figure. The main coefficient plot shows steps 0-6, and step 5 is the reference."
    )
  ),
  output_file("metadata", "figure_omissions.csv")
)

coef_path <- here(
  "outputs", "regressions", "quarterly", "coefficients", "main_qoq_coefficients.csv"
)
annual_coef_path <- here(
  "outputs", "regressions", "annual", "coefficients", "main_annual_coefficients.csv"
)
data_file <- here("data", "processed", "residence_quarter_regression.csv")
covid_file <- here("data", "processed", "ons_covid_by_area_20.csv")
for (path in c(coef_path, data_file, covid_file)) {
  if (!file.exists(path)) {
    stop("Cannot find ", path, ".", call. = FALSE)
  }
}

coefs <- read_csv(coef_path, show_col_types = FALSE)
needed_coef <- c(
  "step", "design", "estimator", "endogenous", "coefficient", "std_error",
  "p_value", "t_df", "n_obs", "n_clusters", "first_stage_f"
)
missing_coef <- setdiff(needed_coef, names(coefs))
if (length(missing_coef) > 0L) {
  stop(
    "Coefficient file is missing ", paste(missing_coef, collapse = ", "),
    ". Run pipeline/04_main_regressions.R again.",
    call. = FALSE
  )
}
if (!identical(as.integer(sort(unique(coefs$step))), 0:6)) {
  stop("Quarterly coefficient file does not contain steps 0-6.", call. = FALSE)
}
if (any(!is.finite(coefs$t_df) | coefs$t_df < 1)) {
  stop("A quarterly t degree of freedom is missing.", call. = FALSE)
}

results_theme <- function() {
  theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(hjust = 0, size = 8, lineheight = 1.05),
      plot.title = element_text(size = 12)
    )
}

save_results <- function(plot, ..., width, height) {
  path <- output_file(...)
  ggsave(
    filename = path, plot = plot, width = width, height = height,
    units = "in", dpi = 300, bg = "white"
  )
  message("Wrote ", path)
  invisible(path)
}

wrap_caption <- function(text, width = 110) {
  paste(strwrap(text, width = width), collapse = "\n")
}

# R01. Intervals use the t critical value stored with each estimate.
ladder <- coefs %>%
  mutate(
    critical = qt(0.975, t_df),
    low = coefficient - critical * std_error,
    high = coefficient + critical * std_error,
    step_label = factor(step_labels[step + 1L], levels = step_labels),
    outcome_label = factor(
      unname(outcome_labels[endogenous]),
      levels = c("Pooled model", "Joint model: EU", "Joint model: non-EU")
    ),
    estimator = factor(estimator, levels = c("OLS", "2SLS")),
    step_pos = as.numeric(step_label) + if_else(estimator == "OLS", -0.18, 0.18)
  )
if (anyNA(ladder$outcome_label)) {
  stop("A quarterly endogenous variable was not labelled.", call. = FALSE)
}
step5_f <- ladder %>%
  filter(step == 5L, estimator == "2SLS") %>%
  distinct(endogenous, first_stage_f)
save_results(
  ggplot(ladder, aes(coefficient, step_pos, colour = estimator)) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
    geom_errorbar(
      aes(xmin = low, xmax = high),
      orientation = "y", width = 0.12, linewidth = 0.4
    ) +
    geom_point(size = 1.6) +
    scale_y_continuous(breaks = seq_along(step_labels), labels = step_labels) +
    facet_wrap(vars(outcome_label), ncol = 1, scales = "free_x") +
    scale_colour_manual(values = c("OLS" = "#222222", "2SLS" = "#0072B2")) +
    labs(
      title = "UK-born wage changes and migration changes",
      x = "Coefficient",
      y = NULL,
      colour = NULL,
      caption = wrap_caption(paste(
        "Outcome: quarter-to-quarter change in UK-born mean log hourly pay.",
        "Regressors: quarter-to-quarter changes in migrants per UK-born person.",
        "Sample: 2021 Q1 onward. Instrument baseline: pooled 2017-2019 stocks.",
        "Each panel has its own scale. Bars are 95% intervals using each estimate's t degrees of freedom and residence-clustered standard errors.",
        "Step 5 is the reference model. Step 6 replaces year fixed effects with quarter fixed effects.",
        sprintf(
          "Step-5 first-stage F statistics: pooled %.1f, EU %.2f, non-EU %.1f.",
          step5_f$first_stage_f[step5_f$endogenous == "d_qoq_ratio_migrant"],
          step5_f$first_stage_f[step5_f$endogenous == "d_qoq_ratio_eu"],
          step5_f$first_stage_f[step5_f$endogenous == "d_qoq_ratio_non_eu"]
        ),
        "The joint EU interval is wide. These intervals are not a causal claim about Brexit."
      ))
    ) +
    results_theme(),
  "results", "main", "ols_2sls_coefficients.png",
  width = 9, height = 11
)

# Rebuild the step-5 sample with the same restrictions as step 04.
raw <- read_csv(data_file, show_col_types = FALSE)
needed <- c(
  outcome, "residence", "period", "year",
  "d_qoq_ratio_migrant", "shift_share_qoq_migrant",
  "d_qoq_ratio_eu", "d_qoq_ratio_non_eu",
  "shift_share_qoq_eu", "shift_share_qoq_non_eu",
  ethnicity, education, age
)
sample <- raw %>%
  filter(year > 2020L) %>%
  filter(if_all(all_of(needed), ~ !is.na(.))) %>%
  mutate(residence = as.character(residence), year = as.integer(year))
covid <- read_csv(covid_file, show_col_types = FALSE) %>%
  transmute(
    residence = as.character(residence),
    covid_asmr_2020 = as.numeric(covid_asmr_2020)
  )
n_before <- nrow(sample)
sample <- sample %>% left_join(covid, by = "residence")
if (nrow(sample) != n_before || any(!is.finite(sample$covid_asmr_2020))) {
  stop("COVID join changed the step-5 sample or left a residence unmatched.", call. = FALSE)
}
if (n_distinct(sample$residence) != 20L || min(sample$year) != 2021L) {
  stop("Step-5 sample must be all 20 residences from 2021 onward.", call. = FALSE)
}
covid_years <- sort(setdiff(unique(sample$year), 2021L))
covid_terms <- paste0("covid_asmr_2020_y", covid_years)
for (yr in covid_years) {
  sample[[paste0("covid_asmr_2020_y", yr)]] <- sample$covid_asmr_2020 * (sample$year == yr)
}
step5_controls <- c(ethnicity, covid_terms, education, age)

fit_step5 <- function(data) {
  fits <- list()
  for (design_name in names(designs)) {
    design <- designs[[design_name]]
    endogenous <- design$endogenous
    iv_text <- paste(
      paste(endogenous, collapse = " + "), "~",
      paste(design$instrument, collapse = " + ")
    )
    ols_rhs <- paste(c(endogenous, step5_controls), collapse = " + ")
    iv_rhs <- paste(step5_controls, collapse = " + ")
    ols <- feols(
      as.formula(paste(outcome, "~", ols_rhs, "| residence + year")),
      data, vcov = ~ residence
    )
    iv <- feols(
      as.formula(paste(outcome, "~", iv_rhs, "| residence + year |", iv_text)),
      data, vcov = ~ residence
    )
    if (nobs(ols) != nrow(data) || nobs(iv) != nrow(data)) {
      stop("Step 5 dropped rows for ", design_name, ".", call. = FALSE)
    }
    dropped <- unique(c(ols$collin.var, iv$collin.var))
    dropped <- dropped[dropped %in% covid_terms]
    if (length(dropped) > 0L) {
      stop("COVID controls were dropped as collinear.", call. = FALSE)
    }
    fits[[design_name]] <- list(ols = ols, iv = iv, endogenous = endogenous)
  }
  fits
}

pull_coef <- function(model, endogenous, estimator) {
  tab <- coeftable(model)
  bind_rows(lapply(endogenous, function(variable) {
    name <- if (estimator == "2SLS") paste0("fit_", variable) else variable
    if (!name %in% rownames(tab)) {
      stop("Refit is missing ", name, ".", call. = FALSE)
    }
    tibble(
      endogenous = variable,
      coefficient = unname(tab[name, "Estimate"]),
      std_error = unname(tab[name, "Std. Error"]),
      p_value = unname(tab[name, "Pr(>|t|)"]),
      t_df = as.numeric(degrees_freedom(model, type = "t"))[1]
    )
  }))
}

relative_gap <- function(actual, expected) {
  scale <- pmax(1, abs(actual), abs(expected))
  abs(actual - expected) / scale
}

full_fit <- fit_step5(sample)
refit_rows <- bind_rows(lapply(names(full_fit), function(design_name) {
  fit <- full_fit[[design_name]]
  bind_rows(
    pull_coef(fit$ols, fit$endogenous, "OLS") %>% mutate(estimator = "OLS"),
    pull_coef(fit$iv, fit$endogenous, "2SLS") %>% mutate(estimator = "2SLS")
  ) %>%
    mutate(design = design_name)
}))
saved_step5 <- coefs %>%
  filter(step == 5L) %>%
  select(design, estimator, endogenous, coefficient, std_error, p_value, t_df, n_obs, n_clusters, first_stage_f)
check <- refit_rows %>%
  inner_join(saved_step5, by = c("design", "estimator", "endogenous"), suffix = c("_refit", "_saved"))
if (nrow(check) != nrow(saved_step5) || nrow(check) != 6L) {
  stop("Step-5 refit does not match the saved coefficient rows.", call. = FALSE)
}
gap <- max(
  relative_gap(check$coefficient_refit, check$coefficient_saved),
  relative_gap(check$std_error_refit, check$std_error_saved),
  relative_gap(check$p_value_refit, check$p_value_saved),
  na.rm = TRUE
)
if (!is.finite(gap) || gap > 1e-8) {
  stop("Step-5 refit coefficients differ from pipeline/04_main_regressions.R.", call. = FALSE)
}
if (any(check$n_obs != nrow(sample)) || any(check$n_clusters != 20L)) {
  stop("Saved step 5 is not the refit sample.", call. = FALSE)
}
write_csv(
  check %>%
    transmute(
      design, estimator, endogenous,
      coefficient = coefficient_saved,
      refit_gap = relative_gap(coefficient_refit, coefficient_saved),
      n_obs, n_clusters
    ),
  output_file("regressions", "quarterly", "diagnostics", "step5_refit_check.csv")
)

partial_residuals <- function(data, column, controls) {
  rhs <- paste(controls, collapse = " + ")
  model <- feols(
    as.formula(paste(column, "~", rhs, "| residence + year")),
    data, vcov = ~ residence
  )
  if (nobs(model) != nrow(data)) {
    stop("Residualising ", column, " dropped rows.", call. = FALSE)
  }
  resid(model)
}

first_stage_slope <- function(data, endogenous, instruments, controls) {
  rhs <- paste(c(instruments, controls), collapse = " + ")
  model <- feols(
    as.formula(paste(endogenous, "~", rhs, "| residence + year")),
    data, vcov = ~ residence
  )
  slope <- unname(coef(model)[instruments[1]])
  if (!is.finite(slope)) {
    stop("First-stage slope is missing for ", endogenous, ".", call. = FALSE)
  }
  slope
}

# R02. Each cloud is residualised on the step-5 controls, year and residence
# fixed effects, and, in the joint model, the other instrument.
stage_specs <- list(
  list(
    design = "pooled", endogenous = "d_qoq_ratio_migrant",
    instrument = "shift_share_qoq_migrant", extra = character()
  ),
  list(
    design = "eu_non_eu", endogenous = "d_qoq_ratio_eu",
    instrument = "shift_share_qoq_eu", extra = "shift_share_qoq_non_eu"
  ),
  list(
    design = "eu_non_eu", endogenous = "d_qoq_ratio_non_eu",
    instrument = "shift_share_qoq_non_eu", extra = "shift_share_qoq_eu"
  )
)
stage_points <- list()
stage_stats <- list()
for (spec in stage_specs) {
  controls <- c(step5_controls, spec$extra)
  x <- partial_residuals(sample, spec$instrument, controls)
  y <- partial_residuals(sample, spec$endogenous, controls)
  line <- coef(lm(y ~ x))
  partial_slope <- unname(line[2])
  partial_intercept <- unname(line[1])
  fitted_slope <- first_stage_slope(
    sample, spec$endogenous, c(spec$instrument, spec$extra), step5_controls
  )
  if (relative_gap(partial_slope, fitted_slope) > 1e-6) {
    stop("Partial first-stage slope differs for ", spec$endogenous, ".", call. = FALSE)
  }
  saved_f <- saved_step5$first_stage_f[
    saved_step5$design == spec$design &
      saved_step5$estimator == "2SLS" &
      saved_step5$endogenous == spec$endogenous
  ]
  label <- unname(outcome_labels[spec$endogenous])
  stage_points[[length(stage_points) + 1L]] <- tibble(
    instrument = x, migration = y, panel = label,
    slope = fitted_slope, intercept = partial_intercept
  )
  stage_stats[[length(stage_stats) + 1L]] <- tibble(
    design = spec$design,
    endogenous = spec$endogenous,
    instrument = spec$instrument,
    other_instrument = if (length(spec$extra) == 0L) NA_character_ else spec$extra,
    first_stage_slope = fitted_slope,
    first_stage_f = saved_f,
    n_obs = nrow(sample),
    n_clusters = 20L
  )
}
stage_points <- bind_rows(stage_points)
stage_stats <- bind_rows(stage_stats)
write_csv(
  stage_stats,
  output_file("results", "first_stage", "step5_first_stage_diagnostics.csv")
)
f_caption <- paste(
  sprintf(
    "%s fixest ivf1 F = %.2f",
    stage_stats$endogenous, stage_stats$first_stage_f
  ),
  collapse = "; "
)
save_results(
  ggplot(stage_points, aes(instrument, migration)) +
    geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey75") +
    geom_vline(xintercept = 0, linewidth = 0.25, colour = "grey75") +
    geom_point(alpha = 0.45, size = 1.3, colour = "#0072B2") +
    geom_abline(
      aes(intercept = intercept, slope = slope),
      linewidth = 0.6, colour = "#222222"
    ) +
    facet_wrap(vars(panel), scales = "free") +
    labs(
      title = "Step-5 first stage, residualised",
      x = "Instrument residual",
      y = "Migration-change residual",
      caption = wrap_caption(paste(
        "Both axes remove residence and year fixed effects, ethnicity, COVID x year, education, and age.",
        "In the joint model, each panel also removes the other instrument.",
        "The line is that conditional first-stage slope, not an unconditional scatter.",
        "The reported F is fixest's ivf1 statistic for the excluded instrument in that first stage.",
        "With 20 residence clusters it is not, by itself, evidence that the instrument is strong.",
        f_caption
      ))
    ) +
    results_theme(),
  "results", "first_stage", "step5_partial_regression.png",
  width = 10, height = 5.8
)

# R03. Conditional OLS association. The other migration regressor is removed
# in the joint panels. This is not the 2SLS estimate.
ols_lookup <- saved_step5 %>% filter(estimator == "OLS")
scatter_specs <- list(
  list(
    endogenous = "d_qoq_ratio_migrant", extra = character(),
    panel = "Pooled model"
  ),
  list(
    endogenous = "d_qoq_ratio_eu", extra = "d_qoq_ratio_non_eu",
    panel = "Joint model: EU, conditional on non-EU"
  ),
  list(
    endogenous = "d_qoq_ratio_non_eu", extra = "d_qoq_ratio_eu",
    panel = "Joint model: non-EU, conditional on EU"
  )
)
scatter_rows <- list()
for (spec in scatter_specs) {
  controls <- c(step5_controls, spec$extra)
  x <- partial_residuals(sample, spec$endogenous, controls)
  y <- partial_residuals(sample, outcome, controls)
  line <- coef(lm(y ~ x))
  slope <- unname(line[2])
  intercept <- unname(line[1])
  saved_slope <- ols_lookup$coefficient[ols_lookup$endogenous == spec$endogenous]
  if (length(saved_slope) != 1L || relative_gap(slope, saved_slope) > 1e-6) {
    stop("Residualised OLS slope differs for ", spec$endogenous, ".", call. = FALSE)
  }
  bins <- tibble(x = x, y = y, bin = ntile(x, 20L)) %>%
    group_by(bin) %>%
    summarise(
      migration = mean(x),
      wage = mean(y),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(panel = spec$panel, slope = saved_slope, intercept = intercept)
  scatter_rows[[length(scatter_rows) + 1L]] <- bins
}
scatter_rows <- bind_rows(scatter_rows)
if (any(scatter_rows$n < 1L) || n_distinct(scatter_rows$panel) != 3L) {
  stop("Binned scatter is missing a panel.", call. = FALSE)
}
save_results(
  ggplot(scatter_rows, aes(migration, wage)) +
    geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey75") +
    geom_vline(xintercept = 0, linewidth = 0.25, colour = "grey75") +
    geom_point(size = 2, colour = "#0072B2") +
    geom_abline(
      aes(intercept = intercept, slope = slope),
      linewidth = 0.6, colour = "#222222"
    ) +
    facet_wrap(vars(panel), scales = "free") +
    labs(
      title = "Step-5 OLS association, residualised and binned",
      x = "Migration-change residual, bin mean",
      y = "UK-born wage-change residual, bin mean",
      caption = wrap_caption(paste(
        sprintf("Each point is the mean of about %d estimation-sample residence-quarters.", scatter_rows$n[1]),
        "Residuals remove residence and year fixed effects, ethnicity, COVID x year, education, and age.",
        "The joint panels also remove the other migration change.",
        "The line is the step-5 OLS coefficient. It is the conditional association, not the 2SLS estimate.",
        "Binning changes the display only. The line is fitted to the residence-quarters, not to the bins."
      ))
    ) +
    results_theme(),
  "results", "main", "step5_residualised_binscatter.png",
  width = 10, height = 5.8
)

# R04. Quarterly and annual estimates stay on separate scales.
if (!file.exists(annual_coef_path)) {
  stop(
    "Cannot find ", annual_coef_path,
    ". Run pipeline/04_main_regressions_annual.R before the quarterly-versus-annual figure.",
    call. = FALSE
  )
}
annual_coefs <- read_csv(annual_coef_path, show_col_types = FALSE)
if (!"t_df" %in% names(annual_coefs) || any(!is.finite(annual_coefs$t_df))) {
  stop("Annual coefficient file has no usable t degrees of freedom.", call. = FALSE)
}
migration_group <- function(variable) {
  case_when(
    grepl("non_eu", variable) ~ "non_eu",
    grepl("_eu$", variable) ~ "eu",
    grepl("migrant", variable) ~ "migrant",
    TRUE ~ NA_character_
  )
}
quarterly_shared <- coefs %>%
  filter(step <= 5L) %>%
  mutate(group = migration_group(endogenous), horizon = "Quarterly, one-quarter change")
annual_shared <- annual_coefs %>%
  filter(step <= 5L) %>%
  mutate(group = migration_group(endogenous), horizon = "Annual, one-year change")
if (anyNA(quarterly_shared$group) || anyNA(annual_shared$group)) {
  stop("A coefficient could not be paired across horizons.", call. = FALSE)
}
paired <- bind_rows(quarterly_shared, annual_shared) %>%
  mutate(
    critical = qt(0.975, t_df),
    low = coefficient - critical * std_error,
    high = coefficient + critical * std_error,
    step_label = factor(step_labels[step + 1L], levels = step_labels[1:6]),
    outcome_label = factor(
      recode(
        group,
        migrant = "Pooled model",
        eu = "Joint model: EU",
        `non_eu` = "Joint model: non-EU"
      ),
      levels = c("Pooled model", "Joint model: EU", "Joint model: non-EU")
    ),
    estimator = factor(estimator, levels = c("OLS", "2SLS")),
    step_pos = as.numeric(step_label) + if_else(estimator == "OLS", -0.18, 0.18),
    horizon = factor(
      horizon,
      levels = c("Quarterly, one-quarter change", "Annual, one-year change")
    ),
    panel = factor(
      paste(horizon, outcome_label, sep = " | "),
      levels = c(
        "Quarterly, one-quarter change | Pooled model",
        "Quarterly, one-quarter change | Joint model: EU",
        "Quarterly, one-quarter change | Joint model: non-EU",
        "Annual, one-year change | Pooled model",
        "Annual, one-year change | Joint model: EU",
        "Annual, one-year change | Joint model: non-EU"
      )
    )
  )
if (anyNA(paired$panel)) {
  stop("A quarterly-versus-annual panel label was not ordered.", call. = FALSE)
}
pair_count <- paired %>%
  count(horizon, step, design, estimator, group)
if (any(pair_count$n != 1L) || n_distinct(paired$horizon) != 2L) {
  stop("Quarterly and annual steps 0-5 do not pair one coefficient each.", call. = FALSE)
}
save_results(
  ggplot(paired, aes(coefficient, step_pos, colour = estimator)) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
    geom_errorbar(
      aes(xmin = low, xmax = high),
      orientation = "y", width = 0.1, linewidth = 0.35
    ) +
    geom_point(size = 1.4) +
    scale_y_continuous(
      breaks = seq_along(step_labels[1:6]),
      labels = step_labels[1:6]
    ) +
    facet_wrap(vars(panel), ncol = 3, scales = "free_x") +
    scale_colour_manual(values = c("OLS" = "#222222", "2SLS" = "#0072B2")) +
    labs(
      title = "Quarterly and annual estimates, steps 0–5",
      x = "Coefficient", y = NULL, colour = NULL,
      caption = wrap_caption(paste(
        "Each panel has its own coefficient scale. The top row is the one-quarter change and the bottom row is the one-year change.",
        "Columns are the pooled model, the joint EU coefficient, and the joint non-EU coefficient.",
        "In the joint EU quarterly panel the 2SLS interval sets the scale.",
        "The annual pooled panel's scale is set by the step-3 2SLS interval.",
        sprintf(
          "At step 5 the annual pooled OLS coefficient is %.2f (standard error %.2f) and the 2SLS first-stage F is %.3f.",
          paired$coefficient[paired$horizon == "Annual, one-year change" &
            paired$group == "migrant" & paired$step == 5L & paired$estimator == "OLS"],
          paired$std_error[paired$horizon == "Annual, one-year change" &
            paired$group == "migrant" & paired$step == 5L & paired$estimator == "OLS"],
          paired$first_stage_f[paired$horizon == "Annual, one-year change" &
            paired$group == "migrant" & paired$step == 5L & paired$estimator == "2SLS"]
        ),
        "The annual outcome is a one-year change in the annual mean. The migration regressors and 2017-2019 instruments use the same horizon as the outcome.",
        "Both samples start in 2021. Quarterly step 6 has no annual counterpart and is omitted here.",
        "Intervals use each row's t degrees of freedom and residence-clustered standard errors."
      ))
    ) +
    results_theme() +
    theme(strip.text = element_text(size = 8), axis.text.y = element_text(size = 7)),
  "results", "robustness", "quarterly_vs_annual_coefficients.png",
  width = 11, height = 9
)

# R06. Drop one residence's rows. Keep the stored 2017-2019 instruments.
residences <- sort(unique(sample$residence))
if (length(residences) != 20L) {
  stop("Leave-one-region-out expected 20 residences.", call. = FALSE)
}
loo_rows <- list()
for (dropped in residences) {
  held <- sample %>% filter(residence != dropped)
  if (n_distinct(held$residence) != 19L || min(held$year) != 2021L) {
    stop("Leaving out ", dropped, " changed the remaining year span.", call. = FALSE)
  }
  if (sum(sample$residence == dropped) != nrow(sample) - nrow(held)) {
    stop("Row count after dropping ", dropped, " is inconsistent.", call. = FALSE)
  }
  held_fit <- fit_step5(held)
  for (design_name in names(held_fit)) {
    fit <- held_fit[[design_name]]
    fs <- fitstat(fit$iv, "ivf", simplify = FALSE)
    for (estimator in c("OLS", "2SLS")) {
      model <- if (estimator == "OLS") fit$ols else fit$iv
      f_values <- if (estimator == "OLS") {
        rep(NA_real_, length(fit$endogenous))
      } else {
        vapply(fit$endogenous, function(variable) {
          unname(fs[[paste0("ivf1::", variable)]]$stat)
        }, numeric(1))
      }
      pulled <- pull_coef(model, fit$endogenous, estimator) %>%
        mutate(
          dropped_residence = dropped,
          design = design_name,
          estimator = estimator,
          n_obs = nobs(model),
          n_clusters = n_distinct(held$residence),
          first_stage_f = f_values
        )
      loo_rows[[length(loo_rows) + 1L]] <- pulled
    }
  }
}
loo <- bind_rows(loo_rows)
if (nrow(loo) != 20L * 6L) {
  stop("Expected 120 leave-one-region-out coefficient rows, got ", nrow(loo), ".", call. = FALSE)
}
if (any(loo$n_clusters != 19L)) {
  stop("A leave-one-region-out model does not have 19 clusters.", call. = FALSE)
}
loo <- loo %>%
  mutate(
    critical = qt(0.975, t_df),
    low = coefficient - critical * std_error,
    high = coefficient + critical * std_error,
    outcome_label = factor(
      unname(outcome_labels[endogenous]),
      levels = c("Pooled model", "Joint model: EU", "Joint model: non-EU")
    ),
    estimator = factor(estimator, levels = c("OLS", "2SLS")),
    panel = factor(
      paste(estimator, outcome_label, sep = " | "),
      levels = c(
        "OLS | Pooled model",
        "OLS | Joint model: EU",
        "OLS | Joint model: non-EU",
        "2SLS | Pooled model",
        "2SLS | Joint model: EU",
        "2SLS | Joint model: non-EU"
      )
    ),
    residence_label = if_else(
      dropped_residence %in% c("Inner London", "Outer London"),
      paste0(dropped_residence, " *"),
      dropped_residence
    )
  )
reference <- saved_step5 %>%
  mutate(
    outcome_label = factor(
      unname(outcome_labels[endogenous]),
      levels = c("Pooled model", "Joint model: EU", "Joint model: non-EU")
    ),
    estimator = factor(estimator, levels = c("OLS", "2SLS")),
    panel = factor(
      paste(estimator, outcome_label, sep = " | "),
      levels = levels(loo$panel)
    )
  )
if (anyNA(loo$panel) || anyNA(reference$panel)) {
  stop("A leave-one-region-out panel label was not ordered.", call. = FALSE)
}
exposure_order <- raw %>%
  distinct(residence, baseline_ratio_eu) %>%
  arrange(baseline_ratio_eu)
loo <- loo %>%
  mutate(residence_label = factor(
    residence_label,
    levels = c(
      exposure_order$residence[!exposure_order$residence %in% c("Inner London", "Outer London")],
      "Outer London *",
      "Inner London *"
    )
  ))
if (anyNA(loo$residence_label)) {
  stop("A leave-one-region-out residence label was not ordered.", call. = FALSE)
}
loo <- loo %>%
  group_by(outcome_label, estimator) %>%
  mutate(width = high - low, display = width <= 30 * median(width)) %>%
  ungroup()
loo_hidden <- loo %>% filter(!display)
loo_shown <- loo %>%
  mutate(
    coefficient = if_else(display, coefficient, NA_real_),
    low = if_else(display, low, NA_real_),
    high = if_else(display, high, NA_real_)
  )
hidden_note <- if (nrow(loo_hidden) == 0L) {
  ""
} else {
  paste(
    "Some 2SLS intervals are too wide for this axis and are listed instead of drawn:",
    paste(
      sprintf(
        "%s, %s, coefficient %.3g, interval %.3g to %.3g",
        loo_hidden$dropped_residence, loo_hidden$outcome_label,
        loo_hidden$coefficient, loo_hidden$low, loo_hidden$high
      ),
      collapse = "; "
    ),
    "."
  )
}
write_csv(
  loo %>%
    transmute(
      dropped_residence, design, estimator, endogenous, coefficient, std_error,
      t_df, low, high, n_obs, n_clusters, first_stage_f,
      instrument_rule = "Stored 2017-2019 shift-share held fixed; only the residence's rows were removed"
    ),
  output_file(
    "regressions", "quarterly", "diagnostics", "leave_one_region_out_step5.csv"
  )
)
save_results(
  ggplot(loo_shown, aes(coefficient, residence_label, colour = estimator, group = estimator)) +
    geom_vline(
      data = reference,
      aes(xintercept = coefficient, linetype = estimator),
      inherit.aes = FALSE,
      linewidth = 0.35, colour = "grey35"
    ) +
    geom_errorbar(
      aes(xmin = low, xmax = high),
      orientation = "y",
      position = position_dodge(width = 0.6),
      width = 0.25, linewidth = 0.3
    ) +
    geom_point(position = position_dodge(width = 0.6), size = 1.4) +
    facet_wrap(vars(panel), ncol = 3, scales = "free_x", axes = "all_y") +
    scale_colour_manual(values = c("OLS" = "#222222", "2SLS" = "#0072B2")) +
    scale_linetype_manual(values = c("OLS" = "solid", "2SLS" = "dashed")) +
    labs(
      title = "Step 5, leaving out one residence",
      x = "Coefficient",
      y = NULL,
      colour = NULL,
      linetype = "Full sample",
      caption = wrap_caption(paste(
        "Each point re-estimates step 5 without that residence. The stored 2017-2019 instruments are unchanged,",
        "so the other residences keep the excluded residence inside their leave-one-out national shocks.",
        "Lines are the full-sample step-5 estimates. Intervals use the 19-cluster t degrees of freedom.",
        "Starred labels are Inner London and Outer London. The 20 estimates are one sensitivity check, not 20 independent results.",
        "The top row is OLS and the bottom row is 2SLS. Each panel has its own coefficient scale.",
        hidden_note
      ))
    ) +
    results_theme() +
    theme(axis.text.y = element_text(size = 5.5), strip.text = element_text(size = 7.5)),
  "results", "robustness", "leave_one_region_out_step5.png",
  width = 13, height = 10
)

message("Results figures written from the post-2021 step-5 specification.")
