# Descriptive charts for the residence-quarter regression file.
#
# Levels of the migrant, EU, and non-EU ratios, their quarter-to-quarter
# changes, the quarter-to-quarter percent change in the UK-born stock,
# the quarter-to-quarter change in UK-born mean log hourly pay,
# and the quarter-to-quarter changes in the seven UK-born controls.
# The window is the whole file, 2006 Q1 through 2026 Q2.
#
# Each residence is a panel in one figure. Ratio levels, and the EU and
# non-EU quarter-to-quarter changes, give every residence its own vertical
# scale. The other quarter-to-quarter residence charts share one vertical
# scale, so a change in one area lines up with the same change in another.
# National ratios are the same in every area, so they are drawn once. They
# use the sum of area counts, not the average of the area ratios.
#
# The ratios and means are already weighted in the regression file. These
# charts do not weight residences again.
#
# The dashed line is 1 January 2021, the end of the Brexit transition
# period. Points are dated at the middle month of the quarter, so the line
# falls between 2020 Q4 and 2021 Q1.
#
# Eight area-quarters move UK-born mean log pay by more than 0.2. The
# largest are Merseyside 2022 Q4 and 2023 Q1, about -0.66 and 0.69. A
# shared axis that includes them flattens the other panels, so that chart
# is limited to +/- 0.2. Those quarters stay in the data and are named in
# the caption.
#
# The residence charts above cover the whole file. The later charts
# describe the 2017-2019 baseline and the post-2021 estimation window.
# They read the regression file and the COVID mortality file. They do not
# read coefficients or fitted models.

source("setup.R")

# URESMC code order, matching the residence labels in data_cleaners.R.
residence_levels <- c(
  "Tyne & Wear", "Rest of Northern region", "South Yorkshire",
  "West Yorkshire", "Rest of Yorks & Humberside", "East Midlands",
  "East Anglia", "Inner London", "Outer London", "Rest of South East",
  "South West", "West Midlands (met county)", "Rest of West Midlands",
  "Greater Manchester", "Merseyside", "Rest of North West", "Wales",
  "Strathclyde", "Rest of Scotland", "Northern Ireland"
)

series_colours <- c(
  "Migrant" = "#222222",
  "EU" = "#0072B2",
  "Non-EU" = "#D55E00"
)

transition <- as.Date("2021-01-01")
transition_caption <- paste(
  "Dashed line is 1 January 2021, the end of the Brexit transition period.",
  "Quarters are dated at the middle month. Sample is 2006 Q1 to 2026 Q2."
)
national_caption <- paste(
  transition_caption,
  "The national ratio sums area counts. It is not the average of the area ratios.",
  sep = "\n"
)
wage_axis_limit <- 0.2

data_file <- here("data", "processed", "residence_quarter_regression.csv")
if (!file.exists(data_file)) {
  stop(
    "Cannot find ", data_file, ". Run pipeline/02_construct_variables.R first.",
    call. = FALSE
  )
}

quarter_midpoint <- function(year, quarter) {
  if (any(!quarter %in% 1:4)) {
    stop("Quarter must be 1, 2, 3, or 4.", call. = FALSE)
  }
  month <- c(2L, 5L, 8L, 11L)[quarter]
  as.Date(sprintf("%d-%02d-15", year, month))
}

reg <- read_csv(data_file, show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    quarter = as.integer(quarter)
  )

unknown <- setdiff(unique(reg$residence), residence_levels)
missing_residence <- setdiff(residence_levels, unique(reg$residence))
if (length(unknown) > 0L || length(missing_residence) > 0L) {
  stop(
    "Residence names do not match the URESMC list.",
    call. = FALSE
  )
}

reg <- reg %>%
  mutate(
    date = quarter_midpoint(year, quarter),
    residence = factor(residence, levels = residence_levels)
  )
if (anyNA(reg$date) || anyNA(reg$residence)) {
  stop("Date or residence is missing.", call. = FALSE)
}
if (anyDuplicated(reg[c("residence", "year", "quarter")]) > 0L) {
  stop("Duplicate residence-year-quarter rows.", call. = FALSE)
}
if (min(reg$year) != 2006L || max(reg$year) < 2026L) {
  stop("Expected the file to run from 2006 through 2026.", call. = FALSE)
}

# Percent change in the UK-born stock, the denominator of the migrant
# ratio. The previous quarter is matched on the calendar, as in
# difference_from(). A person change is not comparable across residences:
# the largest stock is about ten times the smallest. This column is for
# the chart. It is not written back to the regression file.
native_index <- reg$year * 4L + (reg$quarter - 1L) - 1L
native_keys <- tibble(
  residence = as.character(reg$residence),
  .row = seq_len(nrow(reg)),
  lag_year = native_index %/% 4L,
  lag_quarter = as.integer(native_index %% 4L) + 1L
)
native_past <- tibble(
  residence = as.character(reg$residence),
  year = reg$year,
  quarter = reg$quarter,
  native_lag = reg$native
)
native_joined <- left_join(
  native_keys,
  native_past,
  by = c("residence", "lag_year" = "year", "lag_quarter" = "quarter")
)
native_joined <- native_joined %>% arrange(.row)
if (nrow(native_joined) != nrow(reg) || any(native_joined$.row != seq_len(nrow(reg)))) {
  stop("UK-born lag join changed the rows.", call. = FALSE)
}
bad_lag <- is.finite(native_joined$native_lag) & native_joined$native_lag <= 0
if (any(bad_lag) || any(!is.finite(reg$native) | reg$native <= 0)) {
  stop("UK-born stock is missing or not positive.", call. = FALSE)
}
reg$d_qoq_native_pct <- 100 *
  (reg$native - native_joined$native_lag) / native_joined$native_lag

# A quarter-to-quarter change is missing in 2006 Q1 because there is no
# earlier quarter. A gap later than that would be joined if those rows
# were dropped, so stop instead of plotting across it.
assert_leading_na <- function(data, columns) {
  n_residence <- n_distinct(data$residence)
  for (column in columns) {
    missing <- data %>% filter(is.na(.data[[column]]))
    late <- missing %>% filter(!(year == 2006L & quarter == 1L))
    if (nrow(late) > 0L) {
      stop(column, " is missing after 2006 Q1.", call. = FALSE)
    }
    if (nrow(missing) != n_residence) {
      stop(column, " should be missing once per residence in 2006 Q1.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

change_columns <- c(
  "d_qoq_ratio_migrant", "d_qoq_ratio_eu", "d_qoq_ratio_non_eu",
  "d_qoq_national_ratio_migrant", "d_qoq_national_ratio_eu",
  "d_qoq_national_ratio_non_eu",
  "d_qoq_mean_log_hourpay_uk_born",
  "d_qoq_native_pct",
  "d_qoq_mean_age_uk_born", "d_qoq_share_uk_degree", "d_qoq_share_uk_low_qual",
  "d_qoq_share_uk_asian", "d_qoq_share_uk_black", "d_qoq_share_uk_chinese",
  "d_qoq_share_uk_eth_other"
)
assert_leading_na(reg, change_columns)

national_check <- reg %>%
  group_by(year, quarter) %>%
  summarise(
    n = n_distinct(national_ratio_migrant),
    .groups = "drop"
  )
if (any(national_check$n != 1L)) {
  stop("National migrant ratio varies within a quarter.", call. = FALSE)
}

national <- reg %>%
  distinct(
    year, quarter, date,
    national_ratio_migrant, national_ratio_eu, national_ratio_non_eu,
    d_qoq_national_ratio_migrant, d_qoq_national_ratio_eu,
    d_qoq_national_ratio_non_eu,
    d_yoy_national_ratio_migrant, d_yoy_national_ratio_eu,
    d_yoy_national_ratio_non_eu
  )

x_limits <- range(reg$date)
# Interior years only. Labels on 2006 and 2026 sit on the panel edge and
# collide with the neighbouring panel.
year_breaks <- as.Date(sprintf("%d-02-15", c(2010, 2016, 2022)))

base_theme <- function() {
  theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1.2, "cm"),
      strip.text = element_text(size = 7.5),
      axis.text = element_text(size = 7),
      panel.grid.minor = element_blank(),
      panel.spacing.x = unit(0.7, "lines"),
      panel.spacing.y = unit(0.45, "lines"),
      plot.caption = element_text(hjust = 0, size = 8, lineheight = 1.05),
      plot.title = element_text(size = 12)
    )
}

x_scale <- function() {
  scale_x_date(
    limits = x_limits,
    breaks = year_breaks,
    date_labels = "%Y",
    expand = expansion(mult = 0.02)
  )
}

mark_transition <- function() {
  geom_vline(
    xintercept = transition,
    linewidth = 0.3,
    linetype = "dashed",
    colour = "grey40",
    inherit.aes = FALSE
  )
}

zero_line <- function() {
  geom_hline(
    yintercept = 0,
    linewidth = 0.25,
    colour = "grey75",
    inherit.aes = FALSE
  )
}

wrap_caption <- function(text, width) {
  parts <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- unlist(lapply(parts, function(part) strwrap(part, width = width)))
  paste(lines, collapse = "\n")
}

# Grouped series share one legend. Residence panels use the URESMC order.
plot_groups <- function(data, title, y_label, caption, facet,
                         caption_width = 145, scales = "fixed") {
  p <- ggplot(data, aes(date, value, colour = series)) +
    zero_line() +
    mark_transition() +
    geom_line(linewidth = 0.4, na.rm = TRUE) +
    scale_colour_manual(values = series_colours, breaks = names(series_colours)) +
    x_scale() +
    labs(
      title = title, x = NULL, y = y_label, colour = NULL,
      caption = wrap_caption(caption, caption_width)
    ) +
    base_theme()
  if (facet) {
    p <- p + facet_wrap(
      vars(residence),
      ncol = 5,
      scales = scales,
      labeller = labeller(residence = label_wrap_gen(18))
    )
  }
  p
}

plot_one <- function(data, column, title, y_label, caption, ylim = NULL) {
  p <- ggplot(data, aes(date, .data[[column]])) +
    zero_line() +
    mark_transition() +
    geom_line(linewidth = 0.35, colour = "#1f4e79", na.rm = TRUE) +
    facet_wrap(
      vars(residence),
      ncol = 5,
      labeller = labeller(residence = label_wrap_gen(18))
    ) +
    x_scale() +
    labs(
      title = title, x = NULL, y = y_label,
      caption = wrap_caption(caption, 145)
    ) +
    base_theme()
  if (!is.null(ylim)) {
    p <- p + coord_cartesian(ylim = ylim)
  }
  p
}

to_long <- function(data, columns, labels) {
  data %>%
    select(any_of(c("residence", "date")), all_of(columns)) %>%
    pivot_longer(
      all_of(columns),
      names_to = "series",
      values_to = "value"
    ) %>%
    mutate(series = factor(unname(labels[series]), levels = names(series_colours)))
}

wage_caption <- function(data) {
  outside <- data %>%
    filter(is.finite(d_qoq_mean_log_hourpay_uk_born)) %>%
    filter(abs(d_qoq_mean_log_hourpay_uk_born) > wage_axis_limit) %>%
    arrange(year, quarter, residence) %>%
    mutate(
      item = sprintf(
        "%s %d Q%d (%.2f)",
        residence, year, quarter, d_qoq_mean_log_hourpay_uk_born
      )
    )
  if (nrow(outside) == 0L) {
    return(transition_caption)
  }
  # Break only between area-quarters, so a residence name stays on one line.
  lead <- paste0(
    "Vertical scale is limited to +/- ",
    sprintf("%.2f", wage_axis_limit),
    ". Quarters outside it: "
  )
  lines <- character()
  current <- lead
  for (item in outside$item) {
    piece <- if (endsWith(current, ": ")) item else paste0("; ", item)
    if (nchar(current) + nchar(piece) > 140L && !endsWith(current, ": ")) {
      lines <- c(lines, current)
      current <- item
    } else {
      current <- paste0(current, piece)
    }
  }
  current <- paste0(current, ".")
  paste(c(transition_caption, lines, current), collapse = "\n")
}

n_figures <- 0L

save_figure <- function(plot, filename, width, height, subdir) {
  n_figures <<- n_figures + 1L
  path <- output_file("descriptive", "quarterly", subdir, filename)
  ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  message("Wrote ", path)
  invisible(path)
}

ratio_labels <- c(
  ratio_migrant = "Migrant",
  ratio_eu = "EU",
  ratio_non_eu = "Non-EU"
)
national_labels <- c(
  national_ratio_migrant = "Migrant",
  national_ratio_eu = "EU",
  national_ratio_non_eu = "Non-EU"
)
national_change_labels <- c(
  d_qoq_national_ratio_migrant = "Migrant",
  d_qoq_national_ratio_eu = "EU",
  d_qoq_national_ratio_non_eu = "Non-EU"
)

facet_width <- 13
facet_height <- 8.5

save_figure(
  plot_groups(
    to_long(reg, names(ratio_labels), ratio_labels),
    "Migrant ratio by residence",
    "Ratio to UK-born",
    paste(
      transition_caption,
      "Each residence has its own vertical scale."
    ),
    facet = TRUE,
    scales = "free_y"
  ),
  "ratio_levels_by_residence.png",
  facet_width,
  facet_height,
  "migration/levels"
)

save_figure(
  plot_groups(
    to_long(national, names(national_labels), national_labels),
    "National migrant ratio",
    "Ratio to UK-born",
    national_caption,
    facet = FALSE,
    caption_width = 105
  ),
  "national_ratio_levels.png",
  9,
  5.1,
  "migration/levels"
)

save_figure(
  plot_one(
    reg,
    "d_qoq_ratio_migrant",
    "Quarter-to-quarter change in the migrant ratio",
    "Change in ratio",
    transition_caption
  ),
  "d_qoq_ratio_migrant_by_residence.png",
  facet_width,
  facet_height,
  "migration/changes"
)

save_figure(
  plot_one(
    reg,
    "d_qoq_native_pct",
    "Quarter-to-quarter percent change in the UK-born stock",
    "Percent change",
    paste(
      transition_caption,
      "The UK-born stock is the denominator of the migrant ratio.",
      "Change is 100 times this quarter minus the previous quarter, divided by the previous quarter.",
      "Residences share one vertical scale."
    )
  ),
  "d_qoq_native_pct_by_residence.png",
  facet_width,
  9,
  "migration/changes"
)

# Within each origin. The previous row in the long data is the other
# origin, so lag() there is not this change.
eu_non_eu_changes <- c(
  d_qoq_ratio_eu = "EU",
  d_qoq_ratio_non_eu = "Non-EU"
)
save_figure(
  plot_groups(
    to_long(reg, names(eu_non_eu_changes), eu_non_eu_changes),
    "Quarter-to-quarter change in the EU and non-EU ratios",
    "Change in ratio",
    paste(
      transition_caption,
      "Each residence has its own vertical scale."
    ),
    facet = TRUE,
    scales = "free_y"
  ),
  "d_qoq_ratio_eu_non_eu_by_residence.png",
  facet_width,
  facet_height,
  "migration/changes"
)

save_figure(
  plot_groups(
    to_long(national, names(national_change_labels), national_change_labels),
    "Quarter-to-quarter change in the national migrant ratio",
    "Change in ratio",
    national_caption,
    facet = FALSE,
    caption_width = 105
  ),
  "national_d_qoq_ratios.png",
  9,
  5.1,
  "migration/changes"
)

save_figure(
  plot_one(
    reg,
    "d_qoq_mean_log_hourpay_uk_born",
    "Quarter-to-quarter change in UK-born mean log hourly pay",
    "Change in mean log hourly pay",
    wage_caption(reg),
    ylim = c(-wage_axis_limit, wage_axis_limit)
  ),
  "d_qoq_mean_log_hourpay_uk_born_by_residence.png",
  facet_width,
  9,
  "wages"
)

controls <- list(
  list(
    column = "d_qoq_mean_age_uk_born",
    file = "d_qoq_mean_age_uk_born_by_residence.png",
    title = "Quarter-to-quarter change in UK-born mean age",
    y = "Change in years"
  ),
  list(
    column = "d_qoq_share_uk_degree",
    file = "d_qoq_share_uk_degree_by_residence.png",
    title = "Quarter-to-quarter change in the UK-born degree share",
    y = "Change in share"
  ),
  list(
    column = "d_qoq_share_uk_low_qual",
    file = "d_qoq_share_uk_low_qual_by_residence.png",
    title = "Quarter-to-quarter change in the UK-born low-qualification share",
    y = "Change in share"
  ),
  list(
    column = "d_qoq_share_uk_asian",
    file = "d_qoq_share_uk_asian_by_residence.png",
    title = "Quarter-to-quarter change in the UK-born Asian share",
    y = "Change in share"
  ),
  list(
    column = "d_qoq_share_uk_black",
    file = "d_qoq_share_uk_black_by_residence.png",
    title = "Quarter-to-quarter change in the UK-born black share",
    y = "Change in share"
  ),
  list(
    column = "d_qoq_share_uk_chinese",
    file = "d_qoq_share_uk_chinese_by_residence.png",
    title = "Quarter-to-quarter change in the UK-born Chinese share",
    y = "Change in share"
  ),
  list(
    column = "d_qoq_share_uk_eth_other",
    file = "d_qoq_share_uk_eth_other_by_residence.png",
    title = "Quarter-to-quarter change in the UK-born mixed and other share",
    y = "Change in share"
  )
)

for (spec in controls) {
  save_figure(
    plot_one(reg, spec$column, spec$title, spec$y, transition_caption),
    spec$file,
    facet_width,
    facet_height,
    "controls"
  )
}

# The post-2021 charts use the same complete-case columns as
# pipeline/04_main_regressions.R, including a finite 2020 COVID rate.
# Rows before 2021 stay in the historical charts above.
estimation_columns <- c(
  "d_qoq_mean_log_hourpay_uk_born", "period",
  "d_qoq_ratio_migrant", "shift_share_qoq_migrant",
  "d_qoq_ratio_eu", "d_qoq_ratio_non_eu",
  "shift_share_qoq_eu", "shift_share_qoq_non_eu",
  "d_qoq_share_uk_asian", "d_qoq_share_uk_black",
  "d_qoq_share_uk_chinese", "d_qoq_share_uk_eth_other",
  "d_qoq_share_uk_degree", "d_qoq_share_uk_low_qual",
  "d_qoq_mean_age_uk_born"
)
covid_file <- here("data", "processed", "ons_covid_by_area_20.csv")
if (!file.exists(covid_file)) {
  stop(
    "Cannot find ", covid_file,
    ". The post-2021 descriptive sample uses the same COVID file as the regression.",
    call. = FALSE
  )
}
covid <- read_csv(covid_file, show_col_types = FALSE) %>%
  transmute(
    residence = as.character(residence),
    covid_asmr_2020 = as.numeric(covid_asmr_2020)
  )
if (anyDuplicated(covid$residence) > 0L || any(!is.finite(covid$covid_asmr_2020))) {
  stop("COVID file must have one finite 2020 rate per residence.", call. = FALSE)
}
est <- reg %>%
  mutate(residence_label = as.character(residence)) %>%
  filter(year > 2020L) %>%
  left_join(covid, by = c("residence_label" = "residence"))
if (nrow(est) != sum(reg$year > 2020L)) {
  stop("COVID join changed the post-2021 row count.", call. = FALSE)
}
if (any(!is.finite(est$covid_asmr_2020))) {
  stop("A post-2021 residence has no 2020 COVID rate.", call. = FALSE)
}
est <- est %>% filter(if_all(all_of(estimation_columns), ~ !is.na(.)))
if (n_distinct(est$residence) != 20L || min(est$year) != 2021L) {
  stop("Post-2021 descriptive sample must contain all 20 residences from 2021.", call. = FALSE)
}

# Lagged wage-sample size. The quarter-to-quarter wage change uses the
# current quarter and the previous one, so the scatter uses the smaller count.
reg <- reg %>%
  arrange(residence, year, quarter) %>%
  group_by(residence) %>%
  mutate(
    n_pay_lag = lag(n_pay_uk_born),
    lag_year = lag(year),
    lag_quarter = lag(quarter)
  ) %>%
  ungroup()
expected_lag <- reg %>%
  filter(!(year == 2006L & quarter == 1L)) %>%
  mutate(
    previous_year = if_else(quarter == 1L, year - 1L, year),
    previous_quarter = if_else(quarter == 1L, 4L, quarter - 1L)
  )
if (any(expected_lag$lag_year != expected_lag$previous_year |
        expected_lag$lag_quarter != expected_lag$previous_quarter)) {
  stop("Wage-sample lag is not the previous calendar quarter.", call. = FALSE)
}
reg <- reg %>%
  mutate(n_pay_pair = pmin(n_pay_uk_born, n_pay_lag))
est <- est %>%
  left_join(
    reg %>% transmute(
      residence, year, quarter, n_pay_lag, n_pay_pair
    ),
    by = c("residence", "year", "quarter")
  )
if (any(!is.finite(est$n_pay_pair))) {
  stop("A post-2021 wage change has no adjacent wage-sample count.", call. = FALSE)
}

baseline <- reg %>%
  group_by(residence) %>%
  summarise(
    baseline_ratio_migrant = unique(baseline_ratio_migrant),
    baseline_ratio_eu = unique(baseline_ratio_eu),
    baseline_ratio_non_eu = unique(baseline_ratio_non_eu),
    n_values = n_distinct(baseline_ratio_eu),
    .groups = "drop"
  )
if (nrow(baseline) != 20L || any(baseline$n_values != 1L)) {
  stop("2017-2019 EU baseline is not one value per residence.", call. = FALSE)
}
eu_median <- median(baseline$baseline_ratio_eu)
if (any(baseline$baseline_ratio_eu == eu_median)) {
  stop("2017-2019 EU baseline ties the median, so the 10/10 split is undefined.", call. = FALSE)
}
baseline <- baseline %>%
  mutate(
    eu_exposure_group = if_else(
      baseline_ratio_eu > eu_median, "Above median", "Below median"
    )
  )
if (any(table(baseline$eu_exposure_group) != 10L)) {
  stop("EU exposure split is not 10 residences on each side of the median.", call. = FALSE)
}
write_csv(
  baseline %>%
    arrange(desc(baseline_ratio_eu)) %>%
    transmute(
      residence = as.character(residence),
      baseline_ratio_migrant, baseline_ratio_eu, baseline_ratio_non_eu,
      eu_exposure_group,
      median_baseline_ratio_eu = eu_median,
      baseline_window = "2017-2019 pooled counts",
      split_rule = "Above the median 2017-2019 EU ratio versus below it"
    ),
  output_file("descriptive", "quarterly", "tables", "baseline_exposure_2017_2019.csv")
)

# D01. One bar per residence. The height is the stored pooled ratio.
save_figure(
  ggplot(
    baseline,
    aes(reorder(residence, baseline_ratio_eu), baseline_ratio_eu)
  ) +
    geom_col(fill = "#0072B2", width = 0.75) +
    coord_flip() +
    labs(
      title = "EU migrant exposure, 2017–2019",
      x = NULL,
      y = "EU migrants per UK-born person",
      caption = wrap_caption(paste(
        "Each bar is the residence's 2017–2019 pooled EU count divided by its pooled UK-born count.",
        "It is the baseline_ratio_eu column, not the average of quarterly ratios.",
        "This is the baseline in the shift-share instrument. It is fixed for every later quarter.",
        "The ratio is migrants per UK-born person, not a share of the total population."
      ), 100)
    ) +
    base_theme() +
    theme(legend.position = "none"),
  "baseline_eu_2017_2019.png",
  8,
  7.2,
  "migration/exposure"
)

# D02. Same denominator and the same residence order as D01.
baseline_long <- baseline %>%
  transmute(
    residence,
    EU = baseline_ratio_eu,
    `Non-EU` = baseline_ratio_non_eu
  ) %>%
  pivot_longer(-residence, names_to = "series", values_to = "ratio") %>%
  mutate(
    residence = factor(
      residence,
      levels = levels(reorder(baseline$residence, baseline$baseline_ratio_eu))
    ),
    series = factor(series, levels = c("EU", "Non-EU"))
  )
save_figure(
  ggplot(baseline_long, aes(residence, ratio, fill = series)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("EU" = "#0072B2", "Non-EU" = "#D55E00")) +
    labs(
      title = "EU and non-EU exposure, 2017–2019",
      x = NULL,
      y = "Migrants per UK-born person",
      fill = NULL,
      caption = wrap_caption(paste(
        "Both ratios use the residence's pooled 2017–2019 UK-born count as the denominator.",
        "Residences are ordered by the EU ratio, from lowest at the bottom to highest at the top.",
        "The 2010–2015 stock is not shown."
      ), 100)
    ) +
    base_theme(),
  "baseline_eu_non_eu_2017_2019.png",
  8,
  7.2,
  "migration/exposure"
)

# D03. 2025 is the last complete calendar year. 2026 has Q1 and Q2 only.
pool_year <- function(data, year_id, numerator) {
  piece <- data %>%
    filter(year == year_id, quarter %in% 1:4) %>%
    group_by(residence) %>%
    summarise(
      n_q = n(),
      numerator = sum(.data[[numerator]]),
      native = sum(native),
      .groups = "drop"
    )
  if (nrow(piece) != 20L || any(piece$n_q != 4L) || any(!is.finite(piece$native) | piece$native <= 0)) {
    stop("Cannot pool ", year_id, " ", numerator, " counts for every residence.", call. = FALSE)
  }
  piece %>%
    transmute(
      residence,
      ratio = numerator / native
    )
}
exposure_change <- function(numerator, baseline_column, series_name) {
  start <- pool_year(reg, 2021L, numerator) %>% rename(ratio_2021 = ratio)
  end <- pool_year(reg, 2025L, numerator) %>% rename(ratio_2025 = ratio)
  start %>%
    inner_join(end, by = "residence") %>%
    inner_join(
      baseline %>% transmute(residence, baseline = .data[[baseline_column]]),
      by = "residence"
    ) %>%
    mutate(
      change = ratio_2025 - ratio_2021,
      series = series_name,
      residence_label = as.character(residence)
    )
}
exposure_points <- bind_rows(
  exposure_change("eu", "baseline_ratio_eu", "EU"),
  exposure_change("non_eu", "baseline_ratio_non_eu", "Non-EU")
)
if (nrow(exposure_points) != 40L || any(!is.finite(exposure_points$change))) {
  stop("Post-2021 exposure changes are incomplete.", call. = FALSE)
}
exposure_fit <- exposure_points %>%
  group_by(series) %>%
  summarise(
    n = n(),
    correlation = cor(baseline, change),
    slope = coef(lm(change ~ baseline))[2],
    .groups = "drop"
  )
write_csv(
  exposure_points %>%
    transmute(
      series, residence = residence_label, baseline, ratio_2021, ratio_2025, change
    ),
  output_file("descriptive", "quarterly", "tables", "baseline_vs_2021_2025_change.csv")
)
exposure_caption <- paste(
  "Each point is one residence. The vertical axis is the 2025 pooled migrant-to-UK-born ratio minus the 2021 pooled ratio.",
  "A pooled ratio sums the four quarterly counts and divides by the summed UK-born count. It is not the average of the four quarterly ratios.",
  "2026 is omitted because the file has only Q1 and Q2.",
  "The line is a descriptive least-squares fit.",
  sprintf(
    "EU correlation %.2f. Non-EU correlation %.2f. Twenty residences in each panel.",
    exposure_fit$correlation[exposure_fit$series == "EU"],
    exposure_fit$correlation[exposure_fit$series == "Non-EU"]
  ),
  "The association is not a causal effect of the 2017–2019 baseline.",
  "Labels mark Inner London, Outer London, the extreme values, and residences separated from the others.",
  "Every residence is named in baseline_vs_2021_2025_change.csv."
)
# Labels use distances on each series' own 0-1 scales. A fixed list would
# collide again if the ranking changed.
label_exposure_points <- function(points, extra_labels = character()) {
  points %>%
    group_by(series) %>%
    mutate(
      x_scaled = (baseline - min(baseline)) / (max(baseline) - min(baseline)),
      y_scaled = (change - min(change)) / (max(change) - min(change))
    ) %>%
    group_modify(function(piece, key) {
      nearest <- vapply(seq_len(nrow(piece)), function(i) {
        distance <- sqrt(
          (piece$x_scaled[i] - piece$x_scaled)^2 +
            (piece$y_scaled[i] - piece$y_scaled)^2
        )
        distance[i] <- Inf
        min(distance)
      }, numeric(1))
      piece %>%
        mutate(
          nearest = nearest,
          label_this = residence_label %in% c(
            "Inner London", "Outer London", extra_labels
          ) |
            baseline %in% c(min(baseline), max(baseline)) |
            change %in% c(min(change), max(change)) |
            nearest > 0.22,
          hjust = if_else(x_scaled > 0.72, 1.08, -0.08),
          vjust = if_else(y_scaled > 0.78, 1.45, -0.55)
        )
    }) %>%
    ungroup() %>%
    filter(label_this)
}
exposure_labels <- label_exposure_points(exposure_points)
save_figure(
  ggplot(exposure_points, aes(baseline, change, colour = series)) +
    geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey75") +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, colour = "grey20") +
    geom_text(
      data = exposure_labels,
      aes(label = residence_label, hjust = hjust, vjust = vjust),
      size = 2.5, colour = "grey15", show.legend = FALSE
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.16))) +
    scale_y_continuous(expand = expansion(mult = c(0.16, 0.14))) +
    facet_wrap(vars(series), scales = "free") +
    scale_colour_manual(values = c("EU" = "#0072B2", "Non-EU" = "#D55E00"), guide = "none") +
    labs(
      title = "2017–2019 exposure and the change from 2021 to 2025",
      x = "2017–2019 migrants per UK-born person",
      y = "Change in the ratio, 2025 minus 2021",
      caption = wrap_caption(exposure_caption, 110)
    ) +
    base_theme(),
  "baseline_vs_2021_2025_change.png",
  10,
  6.4,
  "migration/exposure"
)

# D03b. Same axes for all migrants. migrant is EU-born plus non-EU-born.
# Both baselines divide by the residence's pooled UK-born count, so each
# migrant point is the sum of that residence's EU and non-EU points.
migrant_points <- exposure_change("migrant", "baseline_ratio_migrant", "Migrant")
if (nrow(migrant_points) != 20L || any(!is.finite(migrant_points$change))) {
  stop("Post-2021 migrant exposure changes are incomplete.", call. = FALSE)
}
component_sum <- exposure_points %>%
  group_by(residence) %>%
  summarise(
    baseline_sum = sum(baseline),
    change_sum = sum(change),
    .groups = "drop"
  )
migrant_check <- migrant_points %>%
  inner_join(component_sum, by = "residence")
baseline_scale <- pmax(
  1, abs(migrant_check$baseline), abs(migrant_check$baseline_sum)
)
change_scale <- pmax(
  1, abs(migrant_check$change), abs(migrant_check$change_sum)
)
if (nrow(migrant_check) != 20L ||
    any(abs(migrant_check$baseline - migrant_check$baseline_sum) > 1e-8 * baseline_scale) ||
    any(abs(migrant_check$change - migrant_check$change_sum) > 1e-8 * change_scale)) {
  stop(
    "Migrant baseline or 2025-minus-2021 change is not the EU plus non-EU value.",
    call. = FALSE
  )
}
migrant_fit <- migrant_points %>%
  summarise(
    n = n(),
    correlation = cor(baseline, change)
  )
if (migrant_fit$n != 20L || !is.finite(migrant_fit$correlation)) {
  stop("Migrant exposure correlation is not defined for 20 residences.", call. = FALSE)
}
write_csv(
  migrant_points %>%
    transmute(
      series, residence = residence_label, baseline, ratio_2021, ratio_2025, change
    ),
  output_file(
    "descriptive", "quarterly", "tables",
    "baseline_migrant_vs_2021_2025_change.csv"
  )
)
migrant_caption <- paste(
  "Each point is one residence. The vertical axis is the 2025 pooled migrant-to-UK-born ratio minus the 2021 pooled ratio.",
  "A pooled ratio sums the four quarterly counts and divides by the summed UK-born count. It is not the average of the four quarterly ratios.",
  "The numerator is EU-born plus non-EU-born.",
  "2026 is omitted because the file has only Q1 and Q2.",
  "The line is a descriptive least-squares fit.",
  sprintf("Correlation %.2f. Twenty residences.", migrant_fit$correlation),
  "The association is not a causal effect of the 2017–2019 baseline.",
  "Labels mark Inner London, Outer London, West Midlands (met county), the extreme values, and residences separated from the others.",
  "Every residence is named in baseline_migrant_vs_2021_2025_change.csv."
)
# West Midlands sits between the main group and London. On the 0-1 scale
# its nearest neighbour is inside 0.22, so the shared rule would leave it unnamed.
migrant_labels <- label_exposure_points(
  migrant_points,
  extra_labels = "West Midlands (met county)"
)
save_figure(
  ggplot(migrant_points, aes(baseline, change)) +
    geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey75") +
    geom_point(size = 2, colour = series_colours[["Migrant"]]) +
    geom_smooth(
      method = "lm", formula = y ~ x, se = FALSE,
      linewidth = 0.6, colour = "grey20"
    ) +
    geom_text(
      data = migrant_labels,
      aes(label = residence_label, hjust = hjust, vjust = vjust),
      size = 2.5, colour = "grey15", show.legend = FALSE
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.16))) +
    scale_y_continuous(expand = expansion(mult = c(0.16, 0.14))) +
    labs(
      title = "2017–2019 migrant exposure and the change from 2021 to 2025",
      x = "2017–2019 migrants per UK-born person",
      y = "Change in the migrant ratio, 2025 minus 2021",
      caption = wrap_caption(migrant_caption, 100)
    ) +
    base_theme(),
  "baseline_migrant_vs_2021_2025_change.png",
  7.5,
  6.8,
  "migration/exposure"
)

# D04. Group membership stays at the 2017-2019 median for every quarter.
grouped <- reg %>%
  left_join(
    baseline %>% transmute(residence, eu_exposure_group),
    by = "residence"
  )
if (anyNA(grouped$eu_exposure_group)) {
  stop("An exposure group was not joined.", call. = FALSE)
}
eu_group <- grouped %>%
  group_by(date, eu_exposure_group) %>%
  summarise(
    n = n(),
    native = sum(native),
    value = sum(eu) / sum(native),
    .groups = "drop"
  ) %>%
  mutate(panel = "EU migrants per UK-born person")
wage_group <- grouped %>%
  group_by(date, eu_exposure_group) %>%
  summarise(
    n = n(),
    n_wage = sum(is.finite(mean_log_hourpay_uk_born)),
    value = mean(mean_log_hourpay_uk_born),
    .groups = "drop"
  ) %>%
  mutate(panel = "UK-born mean log hourly pay, equal-region mean")
if (any(eu_group$n != 10L) || any(!is.finite(eu_group$native) | eu_group$native <= 0)) {
  stop("EU group ratio is not the sum of counts over 10 residences.", call. = FALSE)
}
if (any(wage_group$n != 10L) || any(wage_group$n_wage != 10L)) {
  stop("Wage group mean is not the unweighted mean of 10 residences.", call. = FALSE)
}
group_series <- bind_rows(
  eu_group %>% select(date, eu_exposure_group, value, panel),
  wage_group %>% select(date, eu_exposure_group, value, panel)
)
save_figure(
  ggplot(group_series, aes(date, value, colour = eu_exposure_group)) +
    annotate(
      "rect",
      xmin = as.Date("2021-01-01"), xmax = max(reg$date) + 15,
      ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.45
    ) +
    geom_vline(
      xintercept = transition, linewidth = 0.3, linetype = "dashed", colour = "grey40"
    ) +
    geom_line(linewidth = 0.6) +
    facet_wrap(vars(panel), ncol = 1, scales = "free_y") +
    scale_colour_manual(values = c("Above median" = "#0072B2", "Below median" = "#222222")) +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years", expand = expansion(mult = 0.02)) +
    labs(
      title = "High and low EU-exposure residences",
      x = NULL, y = NULL, colour = "2017–2019 EU ratio",
      caption = wrap_caption(paste(
        "Groups are fixed. Above median and below median each contain 10 residences.",
        sprintf("The median 2017–2019 EU ratio is %.3f migrants per UK-born person.", eu_median),
        "Inner London and Outer London are both above the median.",
        "The EU line sums the 10 residences' EU counts and divides by the sum of their UK-born counts.",
        "The wage line is the unweighted mean of the 10 regional UK-born mean log wages.",
        "The wage sample's income-weight total is not in the file, so this is not a population-weighted wage.",
        "The grey band is 2021 Q1 onward. The dashed line is 1 January 2021.",
        "These lines are not a difference-in-differences estimate."
      ), 115)
    ) +
    base_theme(),
  "high_low_eu_exposure_trends.png",
  9,
  7.6,
  "diagnostics"
)

# D05. Full estimation-sample distribution. Nothing is dropped or winsorised.
change_long <- est %>%
  transmute(
    residence_label, year, quarter,
    Migrant = d_qoq_ratio_migrant,
    EU = d_qoq_ratio_eu,
    `Non-EU` = d_qoq_ratio_non_eu
  ) %>%
  pivot_longer(-c(residence_label, year, quarter), names_to = "series", values_to = "change") %>%
  mutate(series = factor(series, levels = c("Migrant", "EU", "Non-EU")))
change_span <- range(change_long$change)
change_width <- diff(change_span) / 40
largest_changes <- change_long %>%
  group_by(series) %>%
  slice_max(order_by = abs(change), n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(item = sprintf(
    "%s %s %d Q%d (%.3f)", series, residence_label, year, quarter, change
  ))
save_figure(
  ggplot(change_long, aes(change, fill = series)) +
    geom_histogram(
      binwidth = change_width, boundary = 0, colour = "white", linewidth = 0.15
    ) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey30") +
    facet_wrap(vars(series), ncol = 1) +
    scale_fill_manual(
      values = c("Migrant" = "#222222", "EU" = "#0072B2", "Non-EU" = "#D55E00"),
      guide = "none"
    ) +
    labs(
      title = "Quarter-to-quarter migration-ratio changes, 2021 onward",
      x = "Change in migrants per UK-born person",
      y = "Residence-quarters",
      caption = wrap_caption(paste(
        sprintf(
          "Estimation sample: %d residence-quarters, %d residences, %d Q%d to %d Q%d.",
          nrow(est), n_distinct(est$residence),
          min(est$year), min(est$quarter[est$year == min(est$year)]),
          max(est$year), max(est$quarter[est$year == max(est$year)])
        ),
        "The sample keeps the regression's complete cases and a finite 2020 COVID rate. No observation is removed for size.",
        "The three panels share the horizontal scale. Bars are counts, not a smoothed density.",
        "Largest absolute changes:",
        paste(largest_changes$item, collapse = "; "),
        "."
      ), 115)
    ) +
    base_theme(),
  "post2021_ratio_change_distributions.png",
  9,
  8,
  "migration/changes"
)

# D06. The zoomed panel hides nothing from the data: both panels use every estimate.
wage_changes <- est %>%
  transmute(
    residence_label, year, quarter,
    change = d_qoq_mean_log_hourpay_uk_born
  )
mersey <- wage_changes %>%
  filter(residence_label == "Merseyside", year %in% c(2022L, 2023L), quarter %in% c(1L, 4L))
wage_note <- paste(
  "Largest absolute changes:",
  paste(
    sprintf("%s %d Q%d (%.3f)", wage_changes$residence_label, wage_changes$year,
            wage_changes$quarter, wage_changes$change)[
      order(-abs(wage_changes$change))
    ][1:5],
    collapse = "; "
  ),
  ". Merseyside around the previously noted quarters:",
  paste(
    sprintf("%d Q%d (%.3f)", mersey$year, mersey$quarter, mersey$change),
    collapse = "; "
  ),
  "."
)
wage_hist <- function(title, limits, axis_note) {
  p <- ggplot(wage_changes, aes(change)) +
    geom_histogram(
      binwidth = diff(range(wage_changes$change)) / 40,
      boundary = 0, fill = "#1f4e79", colour = "white", linewidth = 0.15
    ) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey30") +
    labs(
      title = title,
      x = "Change in UK-born mean log hourly pay",
      y = "Residence-quarters",
      caption = wrap_caption(paste(wage_note, axis_note), 105)
    ) +
    base_theme()
  if (!is.null(limits)) {
    p <- p + coord_cartesian(xlim = limits)
  }
  p
}
save_figure(
  wage_hist(
    "UK-born wage changes, 2021 onward",
    NULL,
    "This figure shows the full range. A separate figure limits the axis to +/- 0.2 and does not remove those quarters."
  ),
  "post2021_wage_change_distribution.png",
  9,
  5.4,
  "wages"
)
save_figure(
  wage_hist(
    "UK-born wage changes, 2021 onward, axis limited to +/- 0.2",
    c(-wage_axis_limit, wage_axis_limit),
    "This figure limits the axis to +/- 0.2. Those quarters remain in the data and in the full-range figure."
  ),
  "post2021_wage_change_distribution_axis_limited.png",
  9,
  5.4,
  "wages"
)

# D07. n_pay_uk_born is the unweighted respondent count, not the income weight.
sample_order <- reg %>%
  distinct(residence, baseline_ratio_eu) %>%
  arrange(baseline_ratio_eu)
sample_heat <- reg %>%
  mutate(residence = factor(residence, levels = sample_order$residence))
save_figure(
  ggplot(sample_heat, aes(date, residence, fill = n_pay_uk_born)) +
    geom_tile(height = 0.9, width = 80) +
    geom_vline(xintercept = transition, linewidth = 0.3, colour = "white") +
    scale_fill_gradient(low = "#deebf7", high = "#08519c", name = "Respondents") +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    labs(
      title = "UK-born wage sample size",
      x = NULL, y = NULL,
      caption = wrap_caption(paste(
        "Each cell is one residence-quarter. The fill is n_pay_uk_born, the unweighted count of UK-born employees in the wage mean.",
        "It is not the income weight and it is not the UK-born resident stock.",
        "Residences are ordered by the 2017–2019 EU ratio, lowest at the bottom.",
        "The white line is 1 January 2021. No cell is dropped."
      ), 110)
    ) +
    base_theme() +
    theme(legend.position = "right"),
  "wage_sample_counts.png",
  11,
  6.5,
  "diagnostics"
)
save_figure(
  ggplot(est, aes(n_pay_pair, abs(d_qoq_mean_log_hourpay_uk_born))) +
    geom_point(alpha = 0.65, colour = "#1f4e79", size = 1.6) +
    labs(
      title = "Wage-change size and the adjacent sample counts",
      x = "Minimum UK-born wage respondents in the current and previous quarter",
      y = "Absolute quarter-to-quarter change in mean log hourly pay",
      caption = wrap_caption(paste(
        sprintf("Points are the %d post-2021 estimation-sample residence-quarters.", nrow(est)),
        "The horizontal axis is the smaller of the two unweighted counts behind the two wage means.",
        "The 2021 Q1 change uses the 2020 Q4 count as well as the 2021 Q1 count.",
        "Large changes are plotted. None are removed."
      ), 105)
    ) +
    base_theme(),
  "wage_change_vs_sample_size.png",
  8.5,
  5.6,
  "diagnostics"
)

# D08. Missing levels, unavailable quarters, and small-but-valid samples.
observed_quarters <- reg %>% distinct(year, quarter, date)
full_grid <- tidyr::expand_grid(
  residence = unique(reg$residence),
  year = sort(unique(reg$year)),
  quarter = 1:4
) %>%
  filter(!(year == 2026L & quarter > 2L)) %>%
  left_join(observed_quarters, by = c("year", "quarter")) %>%
  left_join(
    reg %>% select(
      residence, year, quarter, n_pay_uk_born,
      mean_log_hourpay_uk_born, native, eu, non_eu
    ),
    by = c("residence", "year", "quarter")
  )
small_cutoff <- as.numeric(stats::quantile(reg$n_pay_uk_born, 0.10))
coverage <- full_grid %>%
  mutate(
    status = case_when(
      is.na(date) ~ "Quarter unavailable",
      !is.finite(mean_log_hourpay_uk_born) ~ "Missing wage",
      !is.finite(native) | !is.finite(eu) | !is.finite(non_eu) ~ "Missing migration",
      n_pay_uk_born <= small_cutoff ~ "Small valid sample",
      TRUE ~ "Valid sample"
    ),
    date = if_else(is.na(date), quarter_midpoint(year, quarter), date),
    residence = factor(residence, levels = sample_order$residence)
  )
status_levels <- c(
  "Valid sample", "Small valid sample", "Missing wage",
  "Missing migration", "Quarter unavailable"
)
status_colours <- c(
  "Valid sample" = "#009E73",
  "Small valid sample" = "#56B4E9",
  "Missing wage" = "#D55E00",
  "Missing migration" = "#E69F00",
  "Quarter unavailable" = "#999999"
)
status_counts <- coverage %>% count(status)
missing_n <- sum(coverage$status %in% c(
  "Missing wage", "Missing migration", "Quarter unavailable"
))
missing_note <- if (missing_n == 0L) {
  paste(
    "Missing wage, missing migration, and a quarter absent from the file are separate statuses.",
    "None of those three occurs inside 2006 Q1–2026 Q2."
  )
} else {
  sprintf(
    "Missing wage, missing migration, or a quarter absent from the file: %d residence-quarters.",
    missing_n
  )
}
write_csv(
  status_counts,
  output_file("descriptive", "quarterly", "tables", "coverage_status_counts.csv")
)
save_figure(
  ggplot(coverage, aes(date, residence, fill = status)) +
    geom_tile(height = 0.9, width = 80) +
    geom_vline(xintercept = transition, linewidth = 0.3, colour = "black") +
    scale_fill_manual(
      values = status_colours,
      breaks = status_levels[status_levels %in% coverage$status],
      drop = FALSE
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
    labs(
      title = "Wage and migration coverage",
      x = NULL, y = NULL, fill = NULL,
      caption = wrap_caption(paste(
        "The grid is every residence from 2006 Q1 through 2026 Q2. 2026 Q3 and Q4 are outside the file and are not drawn.",
        sprintf(
          "Small valid sample means the wage mean is present and n_pay_uk_born is at or below the 10th percentile, %d respondents.",
          as.integer(small_cutoff)
        ),
        paste(
          sprintf("%s: %d", status_counts$status, status_counts$n),
          collapse = ". "
        ),
        ".",
        missing_note,
        "The respondent count is unweighted. The black line is 1 January 2021."
      ), 115)
    ) +
    base_theme() +
    theme(legend.position = "bottom"),
  "coverage_status.png",
  11,
  6.8,
  "diagnostics"
)

# D09. Annual ratios from quarterly counts. Incomplete 2026 is omitted.
heatmap_years <- reg %>%
  filter(year <= 2025L) %>%
  group_by(residence, year) %>%
  summarise(
    n_q = n(),
    native = sum(native),
    eu_ratio = sum(eu) / sum(native),
    .groups = "drop"
  )
if (any(heatmap_years$n_q != 4L) || any(!is.finite(heatmap_years$eu_ratio))) {
  stop("Annual EU heatmap is missing a complete residence-year.", call. = FALSE)
}
heatmap_years <- heatmap_years %>%
  mutate(residence = factor(residence, levels = sample_order$residence))
save_figure(
  ggplot(heatmap_years, aes(year, residence, fill = eu_ratio)) +
    geom_tile(height = 0.9, width = 0.9) +
    geom_vline(xintercept = 2020.5, linewidth = 0.4, colour = "black") +
    scale_fill_gradient(low = "#f7fbff", high = "#0072B2", name = "EU ratio") +
    scale_x_continuous(breaks = seq(2006, 2024, by = 4), expand = expansion(add = 0.4)) +
    labs(
      title = "EU migrant ratio by residence and year",
      x = NULL, y = NULL,
      caption = wrap_caption(paste(
        "Each cell sums the year's four quarterly EU counts and divides by the summed UK-born count.",
        "Residences are ordered by the 2017–2019 EU baseline, lowest at the bottom.",
        "The colour scale is the same for every year. The black line is between 2020 and 2021.",
        "2026 is omitted because Q3 and Q4 are not in the file."
      ), 110)
    ) +
    base_theme() +
    theme(legend.position = "right"),
  "eu_ratio_heatmap_annual.png",
  10,
  6.5,
  "migration/levels"
)

# D10. Separate panels so a one-quarter change is not drawn on a four-quarter scale.
if (any(!is.finite(est$d_yoy_ratio_eu) | !is.finite(est$d_yoy_ratio_non_eu) |
        !is.finite(est$d_qoq_ratio_eu) | !is.finite(est$d_qoq_ratio_non_eu))) {
  stop("A post-2021 EU or non-EU change is missing.", call. = FALSE)
}
horizon_long <- bind_rows(
  est %>% transmute(
    panel = "Quarter-to-quarter distribution",
    date, series = "EU", value = d_qoq_ratio_eu
  ),
  est %>% transmute(
    panel = "Quarter-to-quarter distribution",
    date, series = "Non-EU", value = d_qoq_ratio_non_eu
  ),
  est %>% transmute(
    panel = "Year-on-year distribution",
    date, series = "EU", value = d_yoy_ratio_eu
  ),
  est %>% transmute(
    panel = "Year-on-year distribution",
    date, series = "Non-EU", value = d_yoy_ratio_non_eu
  )
) %>%
  mutate(panel = factor(
    panel,
    levels = c("Quarter-to-quarter distribution", "Year-on-year distribution")
  ))
national_horizon <- bind_rows(
  national %>%
    filter(year > 2020L) %>%
    transmute(
      panel = "National quarter-to-quarter change",
      date, series = "EU", value = d_qoq_national_ratio_eu
    ),
  national %>%
    filter(year > 2020L) %>%
    transmute(
      panel = "National quarter-to-quarter change",
      date, series = "Non-EU", value = d_qoq_national_ratio_non_eu
    ),
  national %>%
    filter(year > 2020L, is.finite(d_yoy_national_ratio_eu)) %>%
    transmute(
      panel = "National year-on-year change",
      date, series = "EU", value = d_yoy_national_ratio_eu
    ),
  national %>%
    filter(year > 2020L, is.finite(d_yoy_national_ratio_non_eu)) %>%
    transmute(
      panel = "National year-on-year change",
      date, series = "Non-EU", value = d_yoy_national_ratio_non_eu
    )
)
if (any(national$year > 2020L & (
  !is.finite(national$d_yoy_national_ratio_eu) |
  !is.finite(national$d_yoy_national_ratio_non_eu)
))) {
  stop("A post-2021 national year-on-year ratio change is missing.", call. = FALSE)
}
distribution_plot <- ggplot(horizon_long, aes(value, fill = series)) +
  geom_histogram(bins = 30, colour = "white", linewidth = 0.1) +
  geom_vline(xintercept = 0, linewidth = 0.25, colour = "grey30") +
  facet_wrap(vars(panel, series), scales = "free", ncol = 2) +
  scale_fill_manual(
    values = c("EU" = "#0072B2", "Non-EU" = "#D55E00"),
    guide = "none"
  ) +
  labs(
    title = "Quarter-to-quarter and year-on-year migration changes",
    x = "Change in migrants per UK-born person",
    y = "Residence-quarters",
    caption = wrap_caption(paste(
      "Each panel has its own scale. EU and non-EU are not overlaid.",
      "Year-on-year is the change from the same quarter one year earlier, including the 2020 level for a 2021 change.",
      "The estimation sample is unchanged. These are different horizons, so their dispersions are not directly comparable."
    ), 105)
  ) +
  base_theme()
national_plot <- ggplot(national_horizon, aes(date, value, colour = series)) +
  geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey75") +
  geom_line(linewidth = 0.5) +
  facet_wrap(vars(panel), scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c("EU" = "#0072B2", "Non-EU" = "#D55E00")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(x = NULL, y = "Change in the national ratio", colour = NULL) +
  base_theme()
save_figure(
  distribution_plot,
  "qoq_vs_yoy_distributions_post2021.png",
  9,
  7.2,
  "migration/changes"
)
save_figure(
  national_plot +
    labs(
      title = "National EU and non-EU changes, 2021 onward",
      caption = wrap_caption(paste(
        "The year-on-year change compares a quarter with the same quarter one year earlier.",
        "The 2021 Q1 year-on-year change uses the 2020 Q1 level, which is outside the estimation sample.",
        "Quarter-to-quarter and year-on-year changes are different horizons. Their vertical scales are separate,",
        "and a larger year-on-year standard deviation would not by itself mean the quarterly series is noisier."
      ), 110)
    ),
  "qoq_vs_yoy_national_post2021.png",
  9,
  7.2,
  "migration/changes"
)

if (n_figures != 28L) {
  stop("Expected 28 quarterly charts, wrote ", n_figures, ".", call. = FALSE)
}
message("Wrote ", n_figures, " quarterly descriptive charts.")
