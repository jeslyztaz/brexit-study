# Descriptive charts for the residence-quarter regression file.
#
# Levels of the migrant, EU, and non-EU ratios, their quarter-to-quarter
# changes, the quarter-to-quarter change in UK-born mean log hourly pay,
# and the quarter-to-quarter changes in the seven UK-born controls.
# The window is the whole file, 2006 Q1 through 2026 Q2.
#
# Each residence is a panel in one figure. The ratio levels give every
# residence its own vertical scale. The quarter-to-quarter charts still
# share a vertical scale within each figure, so a change in one area
# lines up with the same change in another. National ratios are the same
# in every area, so they are drawn once. They use the sum of area counts,
# not the average of the area ratios.
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
    d_qoq_national_ratio_non_eu
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

fig_dir <- here("outputs", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

save_figure <- function(plot, filename, width, height) {
  path <- file.path(fig_dir, filename)
  ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 200,
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
  facet_height
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
  5.1
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
  facet_height
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
  facet_height
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
  5.1
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
  9
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
    facet_height
  )
}

message("Wrote 13 charts to ", fig_dir, ".")
