# Descriptive charts for the annual robustness check.
#
# Same series as pipeline/03_descriptive_analysis.R, on the residence-year
# file. Years are dated at 1 July. A one-year change is missing in the
# first calendar year because there is no previous year. The quarterly
# charts and their directory are not written by this script.
#
# The ratios and means are already weighted. These charts do not weight
# residences again. The national ratio sums area counts.

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

  series_colours <- c(
    "Migrant" = "#222222",
    "EU" = "#0072B2",
    "Non-EU" = "#D55E00"
  )

  data_file <- here("data", "processed", "residence_annual_regression.csv")
  coverage_file <- here("outputs", "annual", "annual_year_coverage.csv")
  if (!file.exists(data_file)) {
    stop(
      "Cannot find ", data_file,
      ". Run pipeline/02_construct_annual_variables.R first.",
      call. = FALSE
    )
  }
  if (!file.exists(coverage_file)) {
    stop(
      "Cannot find ", coverage_file,
      ". Run pipeline/01_build_annual_panel.R first.",
      call. = FALSE
    )
  }

  reg <- read_csv(data_file, show_col_types = FALSE) %>%
    mutate(year = as.integer(year))
  if ("quarter" %in% names(reg)) {
    stop("Annual regression file has a quarter column.", call. = FALSE)
  }
  coverage <- read_csv(coverage_file, show_col_types = FALSE)
  included <- as.integer(coverage$year[coverage$included])
  if (!setequal(included, unique(reg$year))) {
    stop("Regression years do not match the complete calendar years.", call. = FALSE)
  }

  unknown <- setdiff(unique(reg$residence), residence_levels)
  missing_residence <- setdiff(residence_levels, unique(reg$residence))
  if (length(unknown) > 0L || length(missing_residence) > 0L) {
    stop("Residence names do not match the URESMC list.", call. = FALSE)
  }

  reg <- reg %>%
    mutate(
      date = as.Date(sprintf("%d-07-01", year)),
      residence = factor(residence, levels = residence_levels)
    )
  if (anyNA(reg$date) || anyNA(reg$residence)) {
    stop("Date or residence is missing.", call. = FALSE)
  }
  if (anyDuplicated(reg[c("residence", "year")]) > 0L) {
    stop("Duplicate residence-year rows.", call. = FALSE)
  }
  year_span <- reg %>%
    group_by(residence) %>%
    summarise(
      n = n(),
      first = min(year),
      last = max(year),
      .groups = "drop"
    )
  if (any(year_span$n != year_span$last - year_span$first + 1L) ||
      n_distinct(year_span$first) != 1L || n_distinct(year_span$last) != 1L) {
    stop("Residence-years are not a complete consecutive calendar span.", call. = FALSE)
  }

  first_year <- min(reg$year)
  last_year <- max(reg$year)
  sample_caption <- paste0(
    "Annual robustness check. The quarterly analysis is the main specification. ",
    "Dashed line is 1 January 2021, the end of the Brexit transition period. ",
    "Years are dated at 1 July. Sample is ", first_year, " to ", last_year, "."
  )
  national_caption <- paste(
    sample_caption,
    "The national ratio sums area counts. It is not the average of the area ratios.",
    sep = "\n"
  )
  wage_axis_limit <- 0.2

  # A one-year change is missing in the first year. A later gap is not
  # filled from the previous row, so a missing value after that year stops
  # the charts instead of being drawn across.
  assert_leading_na <- function(data, columns) {
    n_residence <- n_distinct(data$residence)
    for (column in columns) {
      missing <- data %>% filter(is.na(.data[[column]]))
      late <- missing %>% filter(year != first_year)
      if (nrow(late) > 0L) {
        stop(column, " is missing after ", first_year, ".", call. = FALSE)
      }
      if (nrow(missing) != n_residence) {
        stop(
          column, " should be missing once per residence in ", first_year, ".",
          call. = FALSE
        )
      }
      second <- data %>% filter(year == first_year + 1L)
      if (nrow(second) == 0L || all(is.na(second[[column]]))) {
        stop(column, " is missing throughout ", first_year + 1L, ".", call. = FALSE)
      }
    }
    invisible(TRUE)
  }

  change_columns <- c(
    "d_annual_ratio_migrant", "d_annual_ratio_eu", "d_annual_ratio_non_eu",
    "d_annual_national_ratio_migrant", "d_annual_national_ratio_eu",
    "d_annual_national_ratio_non_eu",
    "d_annual_mean_log_hourpay_uk_born",
    "d_annual_mean_age_uk_born", "d_annual_share_uk_degree",
    "d_annual_share_uk_low_qual",
    "d_annual_share_uk_asian", "d_annual_share_uk_black",
    "d_annual_share_uk_chinese", "d_annual_share_uk_eth_other"
  )
  assert_leading_na(reg, change_columns)

  # The stored Jaeger analogue is the previous year's one-year change, so
  # it needs two earlier years. It is not plotted and not used in the
  # annual regression.
  lag_columns <- c(
    "d_annual_ratio_migrant_l1", "d_annual_ratio_eu_l1", "d_annual_ratio_non_eu_l1",
    "shift_share_annual_migrant_l1", "shift_share_annual_eu_l1",
    "shift_share_annual_non_eu_l1"
  )
  n_residence <- n_distinct(reg$residence)
  for (column in lag_columns) {
    if (!column %in% names(reg)) {
      stop("Annual file is missing ", column, ".", call. = FALSE)
    }
    missing <- reg %>% filter(is.na(.data[[column]]))
    late <- missing %>% filter(!year %in% c(first_year, first_year + 1L))
    if (nrow(late) > 0L) {
      stop(column, " is missing after ", first_year + 1L, ".", call. = FALSE)
    }
    early <- missing %>% filter(year %in% c(first_year, first_year + 1L))
    if (nrow(early) != 2L * n_residence) {
      stop(
        column, " should be missing for every residence in ",
        first_year, " and ", first_year + 1L, ".",
        call. = FALSE
      )
    }
  }

  national_check <- reg %>%
    group_by(year) %>%
    summarise(n = n_distinct(national_ratio_migrant), .groups = "drop")
  if (any(national_check$n != 1L)) {
    stop("National migrant ratio varies within a year.", call. = FALSE)
  }

  national <- reg %>%
    distinct(
      year, date,
      national_ratio_migrant, national_ratio_eu, national_ratio_non_eu,
      d_annual_national_ratio_migrant, d_annual_national_ratio_eu,
      d_annual_national_ratio_non_eu
    )
  if (anyDuplicated(national$year) > 0L) {
    stop("National series is not one row per year.", call. = FALSE)
  }

  x_limits <- range(reg$date)
  year_breaks <- as.Date(sprintf("%d-07-01", c(2010, 2016, 2022)))
  year_breaks <- year_breaks[year_breaks >= x_limits[[1]] & year_breaks <= x_limits[[2]]]

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
      xintercept = as.Date("2021-01-01"),
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
      filter(is.finite(d_annual_mean_log_hourpay_uk_born)) %>%
      filter(abs(d_annual_mean_log_hourpay_uk_born) > wage_axis_limit) %>%
      arrange(year, residence) %>%
      mutate(
        item = sprintf(
          "%s %d (%.2f)",
          residence, year, d_annual_mean_log_hourpay_uk_born
        )
      )
    if (nrow(outside) == 0L) {
      return(sample_caption)
    }
    lead <- paste0(
      "Vertical scale is limited to +/- ",
      sprintf("%.2f", wage_axis_limit),
      ". Years outside it: "
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
    paste(c(sample_caption, lines, current), collapse = "\n")
  }

  fig_dir <- here("outputs", "annual", "figures")
  assert_annual_output(fig_dir)
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  n_saved <- 0L

  save_figure <- function(plot, filename, width, height) {
    path <- file.path(fig_dir, filename)
    assert_annual_output(path)
    ggsave(
      filename = path,
      plot = plot,
      width = width,
      height = height,
      units = "in",
      dpi = 200,
      bg = "white"
    )
    n_saved <<- n_saved + 1L
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
    d_annual_national_ratio_migrant = "Migrant",
    d_annual_national_ratio_eu = "EU",
    d_annual_national_ratio_non_eu = "Non-EU"
  )

  facet_width <- 13
  facet_height <- 8.5

  save_figure(
    plot_groups(
      to_long(reg, names(ratio_labels), ratio_labels),
      "Migrant ratio by residence, annual",
      "Ratio to UK-born",
      paste(sample_caption, "Each residence has its own vertical scale."),
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
      "National migrant ratio, annual",
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
      "d_annual_ratio_migrant",
      "One-year change in the migrant ratio",
      "Change in ratio",
      sample_caption
    ),
    "d_annual_ratio_migrant_by_residence.png",
    facet_width,
    facet_height
  )

  # Within each origin. The previous row in the long data is the other
  # origin, so lag() there is not this change.
  eu_non_eu_changes <- c(
    d_annual_ratio_eu = "EU",
    d_annual_ratio_non_eu = "Non-EU"
  )
  save_figure(
    plot_groups(
      to_long(reg, names(eu_non_eu_changes), eu_non_eu_changes),
      "One-year change in the EU and non-EU ratios",
      "Change in ratio",
      paste(sample_caption, "Each residence has its own vertical scale."),
      facet = TRUE,
      scales = "free_y"
    ),
    "d_annual_ratio_eu_non_eu_by_residence.png",
    facet_width,
    facet_height
  )

  save_figure(
    plot_groups(
      to_long(national, names(national_change_labels), national_change_labels),
      "One-year change in the national migrant ratio",
      "Change in ratio",
      national_caption,
      facet = FALSE,
      caption_width = 105
    ),
    "national_d_annual_ratios.png",
    9,
    5.1
  )

  save_figure(
    plot_one(
      reg,
      "d_annual_mean_log_hourpay_uk_born",
      "One-year change in UK-born mean log hourly pay",
      "Change in mean log hourly pay",
      wage_caption(reg),
      ylim = c(-wage_axis_limit, wage_axis_limit)
    ),
    "d_annual_mean_log_hourpay_uk_born_by_residence.png",
    facet_width,
    9
  )

  controls <- list(
    list(
      column = "d_annual_mean_age_uk_born",
      file = "d_annual_mean_age_uk_born_by_residence.png",
      title = "One-year change in UK-born mean age",
      y = "Change in years"
    ),
    list(
      column = "d_annual_share_uk_degree",
      file = "d_annual_share_uk_degree_by_residence.png",
      title = "One-year change in the UK-born degree share",
      y = "Change in share"
    ),
    list(
      column = "d_annual_share_uk_low_qual",
      file = "d_annual_share_uk_low_qual_by_residence.png",
      title = "One-year change in the UK-born low-qualification share",
      y = "Change in share"
    ),
    list(
      column = "d_annual_share_uk_asian",
      file = "d_annual_share_uk_asian_by_residence.png",
      title = "One-year change in the UK-born Asian share",
      y = "Change in share"
    ),
    list(
      column = "d_annual_share_uk_black",
      file = "d_annual_share_uk_black_by_residence.png",
      title = "One-year change in the UK-born black share",
      y = "Change in share"
    ),
    list(
      column = "d_annual_share_uk_chinese",
      file = "d_annual_share_uk_chinese_by_residence.png",
      title = "One-year change in the UK-born Chinese share",
      y = "Change in share"
    ),
    list(
      column = "d_annual_share_uk_eth_other",
      file = "d_annual_share_uk_eth_other_by_residence.png",
      title = "One-year change in the UK-born mixed and other share",
      y = "Change in share"
    )
  )

  for (spec in controls) {
    save_figure(
      plot_one(reg, spec$column, spec$title, spec$y, sample_caption),
      spec$file,
      facet_width,
      facet_height
    )
  }
  if (n_saved != 13L) {
    stop("Expected 13 annual charts, wrote ", n_saved, ".", call. = FALSE)
  }

  stat_columns <- c(
    "ratio_migrant", "ratio_eu", "ratio_non_eu",
    "national_ratio_migrant", "national_ratio_eu", "national_ratio_non_eu",
    "d_annual_ratio_migrant", "d_annual_ratio_eu", "d_annual_ratio_non_eu",
    "d_annual_ratio_migrant_l1", "d_annual_ratio_eu_l1", "d_annual_ratio_non_eu_l1",
    "d_annual_national_ratio_migrant", "d_annual_national_ratio_eu",
    "d_annual_national_ratio_non_eu",
    "baseline_ratio_migrant", "baseline_ratio_eu", "baseline_ratio_non_eu",
    "baseline_ratio_migrant_2010_2015", "baseline_ratio_eu_2010_2015",
    "baseline_ratio_non_eu_2010_2015",
    "baseline_national_ratio_migrant", "baseline_national_ratio_eu",
    "baseline_national_ratio_non_eu",
    "shift_share_annual_migrant", "shift_share_annual_eu",
    "shift_share_annual_non_eu",
    "shift_share_annual_migrant_l1", "shift_share_annual_eu_l1",
    "shift_share_annual_non_eu_l1",
    "shift_share_annual_migrant_2010_2015", "shift_share_annual_eu_2010_2015",
    "shift_share_annual_non_eu_2010_2015",
    "mean_log_hourpay_uk_born", "d_annual_mean_log_hourpay_uk_born",
    "mean_age_uk_born", "share_uk_degree", "share_uk_low_qual",
    "share_uk_asian", "share_uk_black", "share_uk_chinese", "share_uk_eth_other",
    "d_annual_mean_age_uk_born", "d_annual_share_uk_degree",
    "d_annual_share_uk_low_qual",
    "d_annual_share_uk_asian", "d_annual_share_uk_black",
    "d_annual_share_uk_chinese", "d_annual_share_uk_eth_other",
    "n_pay_uk_born"
  )
  missing_stats <- setdiff(stat_columns, names(reg))
  if (length(missing_stats) > 0L) {
    stop(
      "Descriptive file is missing ",
      paste(missing_stats, collapse = ", "), ".",
      call. = FALSE
    )
  }

  summarise_sample <- function(data, sample_name) {
    bind_rows(lapply(stat_columns, function(column) {
      x <- data[[column]]
      x <- x[is.finite(x)]
      tibble(
        sample = sample_name,
        variable = column,
        n = length(x),
        mean = if (length(x) > 0L) mean(x) else NA_real_,
        sd = if (length(x) > 1L) stats::sd(x) else NA_real_,
        min = if (length(x) > 0L) min(x) else NA_real_,
        p50 = if (length(x) > 0L) stats::median(x) else NA_real_,
        max = if (length(x) > 0L) max(x) else NA_real_
      )
    }))
  }

  stats <- bind_rows(
    summarise_sample(reg, "all_complete_years"),
    summarise_sample(reg %>% filter(year > 2015), "year_gt_2015")
  )
  stats_path <- here("outputs", "annual", "descriptive_stats.csv")
  national_path <- here("outputs", "annual", "national_ratios_by_year.csv")
  assert_annual_output(stats_path)
  assert_annual_output(national_path)
  write_csv(stats, stats_path)
  write_csv(
    national %>%
      arrange(year) %>%
      select(
        year,
        national_ratio_migrant, national_ratio_eu, national_ratio_non_eu,
        d_annual_national_ratio_migrant, d_annual_national_ratio_eu,
        d_annual_national_ratio_non_eu
      ),
    national_path
  )
  message("Wrote 13 charts to ", fig_dir, ".")
  message("Wrote ", stats_path, " and ", national_path, ".")
})
