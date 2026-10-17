# Residence-by-quarter panel from the quarterly person files in
# data/quarterly_lfs_raw.
#
# One file is one calendar quarter. The quarter comes from the filename
# (jm, aj, js, od), not from QRTR or from the reference week. Every row in
# that file gets the same year and quarter. Ages 16-64. Person weight for
# labour-market and composition statistics, income weight for pay.
# The row is a URESMC residence area, not a Government Office Region.

residence_labels <- c(
  "Tyne & Wear", "Rest of Northern region", "South Yorkshire",
  "West Yorkshire", "Rest of Yorks & Humberside", "East Midlands",
  "East Anglia", "Inner London", "Outer London", "Rest of South East",
  "South West", "West Midlands (met county)", "Rest of West Midlands",
  "Greater Manchester", "Merseyside", "Rest of North West", "Wales",
  "Strathclyde", "Rest of Scotland", "Northern Ireland"
)

quarter_numbers <- c(jm = 1L, aj = 2L, js = 3L, od = 4L)

core_months <- list(
  "1" = 1:3,
  "2" = 4:6,
  "3" = 7:9,
  "4" = 10:12
)

qual_shares <- c(
  share_qual_degree = "Degree or equivalent",
  share_qual_higher_ed = "Higher education",
  share_qual_alevel = "GCE A level or equivalent",
  share_qual_gcse = "GCSE grades A*-C or equivalent",
  share_qual_other = "Other qualifications",
  share_qual_none = "No qualification",
  share_qual_dont_know = "Don't know"
)

statr_shares <- c(
  share_employee = "Employee",
  share_self_employed = "Self-employed",
  share_gov_scheme = "Government scheme",
  share_unpaid_family = "Unpaid family worker"
)

sector_shares <- c(
  share_private_firm = "Private firm or business or Ltd",
  share_other_org = "Other kind of organisation"
)

manager_shares <- c(
  share_manager = "Manager",
  share_foreman = "Foreman or supervisor",
  share_not_manager = "Not manager or supervisor"
)

samegross_shares <- c(
  share_samegross_yes = "Yes",
  share_samegross_no = "No",
  share_samegross_no_usual = "No usual amount"
)

# Three birthplace shares. UK, EU, and everyone else with a known country.
birth_shares <- c(
  share_birth_uk = "UK",
  share_birth_eu = "EU",
  share_birth_non_eu = "non-EU"
)

# Labels on CRYOX7_EUL_Main. Used only to check that column when it is
# the birthplace source. EU here is value 2, "European Union (excluding UK)".
birth_main_labels <- c(
  "UK", "European Union (excluding UK)", "Other Europe",
  "Asia", "Rest of the World"
)

eth_shares <- c(
  share_eth_white = "white",
  share_eth_mixed = "mixed",
  share_eth_asian = "asian",
  share_eth_chinese = "chinese",
  share_eth_black = "black",
  share_eth_other = "other"
)

soc2010_labels <- c(
  "Managers, directors and senior officials",
  "Professional occupations",
  "Associate professional and technical occupations",
  "Administrative and secretarial occupations",
  "Skilled trades occupations",
  "Caring, leisure and other service occupations",
  "Sales and customer service occupations",
  "Process, plant and machine operatives",
  "Elementary occupations"
)

soc2000_labels <- c(
  "Managers and senior officials",
  "Professional occupations",
  "Associate professional and technical occupations",
  "Administrative and secretarial occupations",
  "Skilled trades occupations",
  "Personal service occupations",
  "Sales and customer service occupations",
  "Process, plant and machine operatives",
  "Elementary occupations"
)

# A person is in the denominator when x, the weight, and `among` are all
# usable. `among` is the extra sample for that statistic (in employment,
# employees, and so on). TRUE means no extra sample.
wmean <- function(x, w, among = TRUE) {
  if (length(among) == 1L) among <- rep(among, length(x))
  ok <- !is.na(x) & !is.na(w) & w > 0 & among %in% TRUE
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

# Smallest value at which the cumulative weight reaches half the total.
wmedian <- function(x, w, among = TRUE) {
  if (length(among) == 1L) among <- rep(among, length(x))
  ok <- !is.na(x) & !is.na(w) & w > 0 & among %in% TRUE
  if (!any(ok)) return(NA_real_)
  x <- x[ok]
  w <- w[ok]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

wshare <- function(x, w, level, among = TRUE) {
  if (length(among) == 1L) among <- rep(among, length(x))
  ok <- !is.na(x) & !is.na(w) & w > 0 & among %in% TRUE
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * (x[ok] == level)) / sum(w[ok])
}

# Unlike wshare, a FALSE or missing flag stays in the denominator. Used for
# arrival lags, where people with no arrival year are part of the population.
wprop <- function(flag, w) {
  ok <- !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * (flag[ok] %in% TRUE)) / sum(w[ok])
}

share_columns <- function(x, w, named_levels, among = TRUE) {
  lapply(named_levels, function(level) wshare(x, w, level, among = among))
}

col <- function(df, name) {
  if (!name %in% names(df)) return(rep(NA, nrow(df)))
  df[[name]]
}

lfs_source_name <- function(path) {
  file.path(basename(dirname(dirname(path))), basename(path))
}

ukda_id <- function(path) {
  hit <- regmatches(path, regexec("UKDA-([0-9]+)-tab", path))[[1]]
  if (length(hit) != 2L) return(NA_integer_)
  as.integer(hit[2])
}

parse_lfs_quarter <- function(path) {
  base <- basename(path)
  hit <- regmatches(
    base,
    regexec("^lfsp_([a-z]{2})([0-9]{2})_", base, ignore.case = TRUE)
  )[[1]]
  if (length(hit) != 3L) return(NULL)
  code <- tolower(hit[2])
  yy <- as.integer(hit[3])
  # Quarterly LFS microdata start in 1992.
  year <- if (yy >= 92L) 1900L + yy else 2000L + yy
  list(quarter_code = code, year = year)
}

# Names only. These person files have about 800 columns; opening them with
# a full table reader just to see the header dominates a multi-quarter build.
lfs_header <- function(path) {
  line <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  nms <- strsplit(line, "\t", fixed = TRUE)[[1]]
  if (length(nms) > 0L) nms[[1]] <- sub("^\ufeff", "", nms[[1]])
  nms
}

highest_numbered_column <- function(nms, prefix) {
  hits <- grep(paste0("^", prefix, "[0-9]+$"), nms, value = TRUE)
  if (length(hits) == 0L) return(NULL)
  vintage <- as.integer(sub(paste0("^", prefix), "", hits))
  hits[[which.max(vintage)]]
}

# Columns the residence-quarter panel actually aggregates. The first name
# present in the file wins, same rule as shrink_lfs_cs().
panel_keep <- c(
  "YEAR", "MONTH", "SEX", "AGE", "HOURPAY", "HRRATE", "SAMEGROSS",
  "USUALHRS", "ACTUALHRS", "WRKING", "STATR", "UNDEMP", "ACTIVITY",
  "WELSH", "SCOTTISH", "OTHERS", "NRTHIRISH", "ENGLISH", "BRITISH",
  "ETHNICITY", "QUALGRP", "RESIDENCE", "OCCUGRP", "PUBLICR", "SECTOR",
  "MANAGER", "BIRTH_CODE", "BIRTH_MAIN", "BIRTH12",
  "ARRIVAL_FIRST", "ARRIVAL_LAST"
)

panel_source_columns <- function(header, pwt_name, piwt_name) {
  chosen <- vapply(panel_keep, function(nm) {
    candidates <- lfs_cs_candidates[[nm]]
    hit <- candidates[candidates %in% header]
    if (length(hit) == 0L) NA_character_ else hit[[1]]
  }, character(1))
  unique(c(pwt_name, piwt_name, chosen[!is.na(chosen)]))
}

# Calendar-quarter person files only. Two files for the same quarter: keep
# the higher person-weight vintage, then the higher UKDA study number.
# from_year drops earlier files before they are opened.
list_lfs_quarter_files <- function(root, from_year = NULL) {
  if (!is.null(from_year)) {
    from_year <- as.integer(from_year)
    if (length(from_year) != 1L || is.na(from_year)) {
      stop("from_year must be one year.", call. = FALSE)
    }
  }

  paths <- sort(list.files(
    root,
    pattern = "^lfsp_.*\\.tab$",
    recursive = TRUE,
    full.names = TRUE
  ))
  if (length(paths) == 0L) {
    stop("No lfsp_ person files found in ", root, call. = FALSE)
  }

  parsed <- lapply(paths, parse_lfs_quarter)
  unparsed <- paths[vapply(parsed, is.null, logical(1))]
  codes <- vapply(parsed, function(x) if (is.null(x)) NA_character_ else x$quarter_code, character(1))
  years <- vapply(parsed, function(x) if (is.null(x)) NA_integer_ else x$year, integer(1))
  in_window <- if (is.null(from_year)) rep(TRUE, length(paths)) else !is.na(years) & years >= from_year
  bad <- unique(c(unparsed, paths[in_window & !codes %in% names(quarter_numbers)]))
  if (length(bad) > 0L) {
    stop(
      "These files are not calendar quarters (jm, aj, js, od):\n",
      paste(lfs_source_name(bad), collapse = "\n"),
      call. = FALSE
    )
  }

  paths <- paths[in_window & codes %in% names(quarter_numbers)]
  if (length(paths) == 0L) {
    stop("No calendar-quarter files from ", from_year, " onward in ", root, call. = FALSE)
  }

  rows <- lapply(paths, function(path) {
    header <- lfs_header(path)
    pwt <- highest_numbered_column(header, "PWT")
    piwt <- highest_numbered_column(header, "PIWT")
    if (is.null(pwt)) {
      stop(lfs_source_name(path), " has no person weight (PWT##).", call. = FALSE)
    }
    if (is.null(piwt)) {
      stop(lfs_source_name(path), " has no income weight (PIWT##).", call. = FALSE)
    }
    q <- parse_lfs_quarter(path)
    tibble(
      path = path,
      year = q$year,
      quarter = unname(quarter_numbers[[q$quarter_code]]),
      quarter_code = q$quarter_code,
      pwt_name = pwt,
      pwt_vintage = as.integer(sub("^PWT", "", pwt)),
      piwt_name = piwt,
      columns = list(panel_source_columns(header, pwt, piwt)),
      ukda_id = ukda_id(path)
    )
  })
  keep_one_file_per_quarter(bind_rows(rows))
}

keep_one_file_per_quarter <- function(files) {
  keys <- paste(files$year, files$quarter)
  keep <- integer(0)
  for (key in unique(keys)) {
    i <- which(keys == key)
    if (length(i) == 1L) {
      keep <- c(keep, i)
      next
    }
    best <- i[files$pwt_vintage[i] == max(files$pwt_vintage[i])]
    best_id <- suppressWarnings(max(files$ukda_id[best], na.rm = TRUE))
    if (!is.finite(best_id)) {
      stop(
        "Same quarter and weight vintage, and no UKDA study number:\n",
        paste(files$path[best], collapse = "\n"),
        call. = FALSE
      )
    }
    chosen <- best[!is.na(files$ukda_id[best]) & files$ukda_id[best] == best_id]
    if (length(chosen) != 1L) {
      stop(
        "Same quarter, weight vintage, and UKDA study number:\n",
        paste(files$path[best], collapse = "\n"),
        call. = FALSE
      )
    }
    dropped <- setdiff(i, chosen)
    message(
      "Quarter ", files$year[chosen], " Q", files$quarter[chosen],
      " keeps ", lfs_source_name(files$path[chosen]),
      " and drops ", paste(lfs_source_name(files$path[dropped]), collapse = ", ")
    )
    keep <- c(keep, chosen)
  }
  files[keep, ] %>% arrange(year, quarter)
}

clean_missing_codes <- function(x) {
  if (is.character(x)) x <- suppressWarnings(as.numeric(trimws(x)))
  # Whole-number weights are read as integers. Weighted sums of those
  # overflow R's integer range, so the working copy is double.
  x <- as.numeric(x)
  x[x %in% c(-9, -8, -6)] <- NA_real_
  x
}

read_lfs_microdata <- function(path, pwt_name, piwt_name, columns) {
  missing <- setdiff(c(pwt_name, piwt_name, "URESMC"), columns)
  if (length(missing) > 0L) {
    stop(
      lfs_source_name(path), " is missing ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # fread skips the hundreds of unused columns. Reading them as text and
  # converting in R was most of the runtime.
  raw <- data.table::fread(
    path,
    sep = "\t",
    select = columns,
    na.strings = c("", "NA"),
    showProgress = FALSE,
    data.table = FALSE
  )
  int_cols <- vapply(raw, is.integer, logical(1))
  if (any(int_cols)) raw[int_cols] <- lapply(raw[int_cols], as.numeric)
  raw <- as_tibble(raw)

  income <- clean_missing_codes(raw[[piwt_name]])
  lfs <- raw %>%
    shrink_lfs_cs(weight = pwt_name) %>%
    decode_lfs_cs()
  if (nrow(lfs) != length(income)) {
    stop(lfs_source_name(path), ": row count changed while shrinking.", call. = FALSE)
  }
  lfs$INCOME_WEIGHT <- income
  lfs
}

# The filename year is the survey year. OD files can contain a few early
# January interviews, and JM files a few April interviews, so the check is
# the modal year and at least 80% of reference months in the three core
# months, not every row.
check_survey_quarter <- function(lfs, year, quarter, file) {
  years <- lfs$YEAR[!is.na(lfs$YEAR)]
  if (length(years) == 0L) {
    stop(file, " has no reference year.", call. = FALSE)
  }
  tab <- table(years)
  if (sum(tab == max(tab)) != 1L) {
    stop(file, " has no single modal reference year.", call. = FALSE)
  }
  modal <- as.integer(names(tab)[which.max(tab)])
  if (modal != year) {
    stop(
      file, ": modal reference year is ", modal, ", filename year is ", year,
      call. = FALSE
    )
  }

  months <- lfs$MONTH[!is.na(lfs$MONTH)]
  if (length(months) == 0L) {
    stop(file, " has no reference month.", call. = FALSE)
  }
  share <- mean(months %in% core_months[[as.character(quarter)]])
  if (share < 0.8) {
    stop(
      file, ": only ", round(100 * share, 1),
      "% of reference months fall in quarter ", quarter, ".",
      call. = FALSE
    )
  }
}

# `$` on a tibble warns when the column is absent. Qualification and
# birthplace are absent in some vintages, and that absence is expected.
pull_col <- function(data, name) {
  if (!name %in% names(data)) return(NULL)
  data[[name]]
}

stop_if_codes <- function(x, allowed, variable, file) {
  if (is.null(x)) return(invisible(NULL))
  bad <- unique(x[!is.na(x) & !x %in% allowed])
  if (length(bad) == 0L) return(invisible(NULL))
  stop(
    file, ": ", variable, " has codes outside the expected list: ",
    paste(bad, collapse = ", "),
    call. = FALSE
  )
}

stop_unless_levels <- function(x, expected, variable, file) {
  if (is.null(x)) return(invisible(NULL))
  if (!identical(levels(x), expected)) {
    stop(
      file, ": ", variable, " labels are ",
      paste(levels(x), collapse = " | "),
      call. = FALSE
    )
  }
}

stop_if_unlabelled <- function(lfs, code, label, file) {
  if (code %in% names(lfs) && !label %in% names(lfs)) {
    stop(file, ": ", code, " could not be labelled.", call. = FALSE)
  }
}

source_of <- function(lfs, name) {
  src <- attr(lfs, "source_variables")
  if (is.null(src) || !name %in% names(src) || is.na(src[[name]])) {
    return(NA_character_)
  }
  unname(src[[name]])
}

check_labels <- function(lfs, file) {
  required <- c(
    "AGE", "WEIGHT", "YEAR", "MONTH", "RESIDENCE_LABEL",
    "SEX_LABEL", "ACTIVITY_LABEL", "STATR_LABEL", "HOURPAY", "HRRATE",
    "INCOME_WEIGHT"
  )
  missing <- setdiff(required, names(lfs))
  if (length(missing) > 0L) {
    stop(file, " is missing ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (is.null(attr(lfs, "source_variables"))) {
    stop(file, " lost the shrinker source-variable map.", call. = FALSE)
  }

  stop_unless_levels(pull_col(lfs, "RESIDENCE_LABEL"), residence_labels, "RESIDENCE", file)
  stop_unless_levels(pull_col(lfs, "SEX_LABEL"), c("Male", "Female"), "SEX", file)
  stop_unless_levels(
    pull_col(lfs, "ACTIVITY_LABEL"),
    c("In employment", "ILO unemployed", "Inactive", "Under 16"),
    "ACTIVITY", file
  )
  stop_unless_levels(pull_col(lfs, "STATR_LABEL"), unname(statr_shares), "STATR", file)
  stop_unless_levels(pull_col(lfs, "WRKING_LABEL"), c("Yes", "No"), "WRKING", file)
  stop_unless_levels(pull_col(lfs, "UNDEMP_LABEL"), c("Yes", "No"), "UNDEMP", file)
  stop_unless_levels(pull_col(lfs, "PUBLIC_LABEL"), c("Private", "Public"), "PUBLICR", file)
  stop_unless_levels(pull_col(lfs, "SECTOR_LABEL"), unname(sector_shares), "SECTOR", file)
  stop_unless_levels(pull_col(lfs, "MANAGER_LABEL"), unname(manager_shares), "MANAGER", file)
  stop_unless_levels(pull_col(lfs, "SAMEGROSS_LABEL"), unname(samegross_shares), "SAMEGROSS", file)
  stop_unless_levels(pull_col(lfs, "QUALGRP_LABEL"), unname(qual_shares), "QUALGRP", file)
  stop_unless_levels(pull_col(lfs, "BIRTH_MAIN_LABEL"), birth_main_labels, "BIRTH_MAIN", file)

  pairs <- list(
    c("WRKING", "WRKING_LABEL"), c("UNDEMP", "UNDEMP_LABEL"),
    c("PUBLICR", "PUBLIC_LABEL"), c("SECTOR", "SECTOR_LABEL"),
    c("MANAGER", "MANAGER_LABEL"), c("SAMEGROSS", "SAMEGROSS_LABEL"),
    c("QUALGRP", "QUALGRP_LABEL"), c("OCCUGRP", "OCCUGRP_LABEL"),
    c("ETHNICITY", "ETHNICITY_LABEL"), c("BIRTH_MAIN", "BIRTH_MAIN_LABEL")
  )
  for (pair in pairs) stop_if_unlabelled(lfs, pair[[1]], pair[[2]], file)

  if ("OCCUGRP" %in% names(lfs)) {
    scheme <- source_of(lfs, "OCCUGRP")
    expected <- if (identical(scheme, "SC2KMMJ")) {
      soc2000_labels
    } else if (scheme %in% c("SC10MMJ", "SC20MMJ")) {
      soc2010_labels
    } else {
      stop(file, ": unrecognised occupation source ", scheme, call. = FALSE)
    }
    stop_unless_levels(lfs$OCCUGRP_LABEL, expected, "OCCUGRP", file)
  }
}

stop_if_codes_present <- function(lfs, file) {
  stop_if_codes(pull_col(lfs, "SEX"), 1:2, "SEX", file)
  stop_if_codes(pull_col(lfs, "WRKING"), 1:2, "WRKING", file)
  stop_if_codes(pull_col(lfs, "UNDEMP"), 1:2, "UNDEMP", file)
  stop_if_codes(pull_col(lfs, "ACTIVITY"), 1:4, "ACTIVITY", file)
  stop_if_codes(pull_col(lfs, "STATR"), 1:4, "STATR", file)
  stop_if_codes(pull_col(lfs, "PUBLICR"), 1:2, "PUBLICR", file)
  stop_if_codes(pull_col(lfs, "SECTOR"), 1:2, "SECTOR", file)
  stop_if_codes(pull_col(lfs, "MANAGER"), 1:3, "MANAGER", file)
  stop_if_codes(pull_col(lfs, "SAMEGROSS"), 1:3, "SAMEGROSS", file)
  stop_if_codes(pull_col(lfs, "QUALGRP"), 1:7, "QUALGRP", file)
  stop_if_codes(pull_col(lfs, "OCCUGRP"), 1:9, "OCCUGRP", file)
  stop_if_codes(pull_col(lfs, "RESIDENCE"), 1:20, "RESIDENCE", file)
  stop_if_codes(pull_col(lfs, "BIRTH_MAIN"), 1:5, "BIRTH_MAIN", file)
  for (nm in c("WELSH", "SCOTTISH", "OTHERS", "NRTHIRISH", "ENGLISH", "BRITISH")) {
    stop_if_codes(pull_col(lfs, nm), c(0, 1), nm, file)
  }
}

# Six groups that line up across ETHUKEUL, ETHCEN6, and ETH01. Chinese is
# kept separate from Asian in all three code lists.
ethnicity_group <- function(code, origin, file) {
  lut <- if (origin == "ETHUKEUL") {
    c(
      "1" = "white", "2" = "mixed", "3" = "asian", "4" = "asian",
      "5" = "asian", "6" = "chinese", "7" = "asian", "8" = "black",
      "9" = "other"
    )
  } else {
    c(
      "1" = "white", "2" = "mixed", "3" = "asian", "4" = "black",
      "5" = "chinese", "6" = "other"
    )
  }
  group <- rep(NA_character_, length(code))
  known <- !is.na(code)
  if (!any(known)) return(group)
  mapped <- unname(lut[as.character(code[known])])
  bad <- unique(as.character(code[known])[is.na(mapped)])
  if (length(bad) > 0L) {
    stop(
      file, ": ", origin, " codes with no ethnicity group: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  group[known] <- mapped
  group
}

# EU-born on CRYOX7. This is the EU28 excluding the UK, which is what
# CRYOX7_EUL_Main value 2 means from 2017 Q3. Croatia is included.
# Spain is 911/912/913 on this code list, not ISO 724. Cyprus (European
# Union) is 901 and Cyprus not otherwise specified is 903. Cyprus
# (non-European Union), 902, is left out. So are Czechoslovakia (971),
# the USSR (972), and Yugoslavia (973).
eu_cryox7_codes <- c(
  40, 56, 100, 191, 196, 203, 208, 233, 246, 250, 276, 300, 348, 372,
  380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 724, 752,
  901, 903, 911, 912, 913
)

uk_cryox7_codes <- 921:926

# "UK", "EU", "non-EU", or NA. CRYOX7 where the end-user file still has
# it (through 2017 Q2). From 2017 Q3 the file has CRYOX7_EUL_Main instead:
# 1 UK, 2 EU, 3-5 non-EU.
birth_group <- function(lfs) {
  group <- rep(NA_character_, nrow(lfs))
  if ("BIRTH_CODE" %in% names(lfs)) {
    code <- lfs$BIRTH_CODE
    known <- !is.na(code)
    group[known & code %in% uk_cryox7_codes] <- "UK"
    group[known & code %in% eu_cryox7_codes] <- "EU"
    group[known & !code %in% c(uk_cryox7_codes, eu_cryox7_codes)] <- "non-EU"
  } else if ("BIRTH_MAIN" %in% names(lfs)) {
    code <- lfs$BIRTH_MAIN
    known <- !is.na(code)
    group[known & code == 1] <- "UK"
    group[known & code == 2] <- "EU"
    group[known & code %in% 3:5] <- "non-EU"
  }
  factor(group, levels = unname(birth_shares))
}

# TRUE when known to be UK-born, FALSE when known not to be, NA when unknown.
# birth_group covers vintages with CRYOX7 or CRYOX7_EUL_Main. BIRTH12 is
# only the fallback before those exist. From 2007, CRY01 and CRY12 code
# the UK as 921-926. The four 2006 files code it as 1-5. Republic of
# Ireland is 6 in 2006 and 372 later.
uk_born_flag <- function(lfs, file) {
  if ("birth3" %in% names(lfs) && any(!is.na(lfs$birth3))) {
    return(lfs$birth3 == "UK")
  }
  if ("BIRTH12" %in% names(lfs)) {
    uk <- rep(NA, nrow(lfs))
    known <- !is.na(lfs$BIRTH12)
    uk[known] <- lfs$BIRTH12[known] %in% c(1:5, 921, 922, 923, 924, 926)
    return(uk)
  }
  stop(
    file, " has no birthplace variable, so UK-born cannot be identified.",
    call. = FALSE
  )
}

add_derived <- function(lfs, file) {
  # 995 is a censored rate of £995 or more. 998 and 999 are non-response.
  if ("HRRATE" %in% names(lfs)) {
    lfs$HRRATE[lfs$HRRATE %in% c(998, 999)] <- NA_real_
  }
  if ("ETHNICITY" %in% names(lfs)) {
    origin <- source_of(lfs, "ETHNICITY")
    if (is.na(origin) || !origin %in% c("ETHUKEUL", "ETHCEN6", "ETH01")) {
      stop(file, ": unrecognised ethnicity source ", origin, call. = FALSE)
    }
    lfs$eth6 <- ethnicity_group(lfs$ETHNICITY, origin, file)
  } else {
    lfs$eth6 <- rep(NA_character_, nrow(lfs))
  }
  lfs$birth3 <- birth_group(lfs)
  lfs$not_uk <- !uk_born_flag(lfs, file)
  lfs
}

arrival_lag_shares <- function(df, column, w, prefix) {
  names_out <- sprintf("%s_%02d", prefix, 0:20)
  if (!column %in% names(df)) {
    return(setNames(rep(list(NA_real_), 21L), names_out))
  }
  arrived <- df[[column]]
  year <- df$YEAR
  stats <- lapply(0:20, function(k) {
    flag <- !is.na(arrived) & !is.na(year) &
      arrived >= 1900 & arrived <= year & (year - arrived) == k
    wprop(flag, w)
  })
  setNames(stats, names_out)
}

years_since <- function(df, column, w) {
  if (!column %in% names(df)) return(NA_real_)
  arrived <- df[[column]]
  valid <- df$not_uk %in% TRUE & !is.na(arrived) & !is.na(df$YEAR) &
    arrived >= 1900 & arrived <= df$YEAR
  wmean(df$YEAR - arrived, w, among = valid)
}

# df is one residence area, already limited to ages 16-64. Zero rows is a
# real area with nobody in the sample: counts are 0 and rates are missing.
residence_stats <- function(df, eth_origin, occ_scheme) {
  w <- col(df, "WEIGHT")
  iw <- col(df, "INCOME_WEIGHT")
  hourpay <- col(df, "HOURPAY")
  hrrate <- col(df, "HRRATE")
  person <- !is.na(w) & w > 0
  activity <- col(df, "ACTIVITY_LABEL")
  status <- col(df, "STATR_LABEL")
  in_emp <- activity == "In employment"
  employee <- status == "Employee"
  known_status <- activity %in% c("In employment", "ILO unemployed", "Inactive")
  active <- activity %in% c("In employment", "ILO unemployed")
  pay <- employee %in% TRUE & !is.na(hourpay) & hourpay > 0 &
    !is.na(iw) & iw > 0
  # Employees with a basic rate. Not restricted to HOURPAY > 0. 995 stays.
  rate <- employee %in% TRUE & !is.na(hrrate) & hrrate > 0 &
    !is.na(iw) & iw > 0
  log_hourpay <- rep(NA_real_, length(hourpay))
  log_hourpay[pay %in% TRUE] <- log(hourpay[pay %in% TRUE])

  eth_detail <- if (identical(eth_origin, "ETHUKEUL")) {
    share_columns(col(df, "ETHNICITY"), w, c(
      share_eth_indian = 3,
      share_eth_pakistani = 4,
      share_eth_bangladeshi = 5
    ))
  } else {
    list(
      share_eth_indian = NA_real_,
      share_eth_pakistani = NA_real_,
      share_eth_bangladeshi = NA_real_
    )
  }

  occ <- as.integer(col(df, "OCCUGRP_LABEL"))
  occ_stats <- setNames(
    lapply(1:9, function(k) wshare(occ, w, k, among = in_emp)),
    paste0("share_occ_", 1:9)
  )

  stats <- c(
    list(
      n = as.integer(sum(person)),
      pop = sum(w[person]),
      mean_age = wmean(col(df, "AGE"), w),
      median_age = wmedian(col(df, "AGE"), w),
      share_female = wshare(col(df, "SEX_LABEL"), w, "Female"),
      emp_rate = wshare(activity, w, "In employment", among = known_status),
      inact_rate = wshare(activity, w, "Inactive", among = known_status),
      share_unemployed = wshare(activity, w, "ILO unemployed", among = known_status),
      unemp_rate = wshare(activity, w, "ILO unemployed", among = active),
      share_wrking = wshare(col(df, "WRKING_LABEL"), w, "Yes"),
      share_undemp = wshare(col(df, "UNDEMP_LABEL"), w, "Yes"),
      mean_hourpay = wmean(hourpay, iw, among = pay),
      median_hourpay = wmedian(hourpay, iw, among = pay),
      mean_log_hourpay = wmean(log_hourpay, iw, among = pay),
      n_pay = as.integer(sum(pay %in% TRUE)),
      pop_pay = sum(iw[pay %in% TRUE]),
      mean_hrrate = wmean(hrrate, iw, among = rate),
      median_hrrate = wmedian(hrrate, iw, among = rate),
      mean_usualhrs = wmean(col(df, "USUALHRS"), w, among = in_emp),
      mean_actualhrs = wmean(col(df, "ACTUALHRS"), w, among = in_emp),
      share_public = wshare(col(df, "PUBLIC_LABEL"), w, "Public", among = employee),
      mean_years_since_first_arrival = years_since(df, "ARRIVAL_FIRST", w),
      mean_years_since_last_arrival = years_since(df, "ARRIVAL_LAST", w)
    ),
    share_columns(col(df, "SAMEGROSS_LABEL"), iw, samegross_shares),
    share_columns(status, w, statr_shares, among = in_emp),
    share_columns(col(df, "SECTOR_LABEL"), w, sector_shares, among = employee),
    share_columns(col(df, "MANAGER_LABEL"), w, manager_shares, among = in_emp),
    share_columns(col(df, "QUALGRP_LABEL"), w, qual_shares),
    list(occ_scheme = occ_scheme),
    occ_stats,
    share_columns(col(df, "eth6"), w, eth_shares),
    eth_detail,
    share_columns(col(df, "birth3"), w, birth_shares),
    list(
      share_welsh = wshare(col(df, "WELSH"), w, 1),
      share_scottish = wshare(col(df, "SCOTTISH"), w, 1),
      share_other_national_identity = wshare(col(df, "OTHERS"), w, 1),
      share_nrthirish = wshare(col(df, "NRTHIRISH"), w, 1),
      share_english = wshare(col(df, "ENGLISH"), w, 1),
      share_british = wshare(col(df, "BRITISH"), w, 1)
    ),
    arrival_lag_shares(df, "ARRIVAL_FIRST", w, "share_first_lag"),
    arrival_lag_shares(df, "ARRIVAL_LAST", w, "share_last_lag")
  )
  as_tibble(stats)
}

aggregate_residence_quarter <- function(lfs, info) {
  file <- lfs_source_name(info$path)
  eth_origin <- source_of(lfs, "ETHNICITY")
  occ_scheme <- source_of(lfs, "OCCUGRP")
  aged <- lfs[!is.na(lfs$AGE) & lfs$AGE >= 16 & lfs$AGE <= 64, , drop = FALSE]
  n_missing <- sum(is.na(aged$RESIDENCE_LABEL))
  if (n_missing > 0L) {
    message(
      file, ": ", n_missing,
      " people aged 16-64 have no residence and are left out."
    )
  }

  rows <- lapply(seq_along(residence_labels), function(i) {
    area <- residence_labels[[i]]
    idx <- which(as.character(aged$RESIDENCE_LABEL) == area)
    df <- aged[idx, , drop = FALSE]
    bind_cols(
      tibble(
        residence = area,
        residence_code = i,
        year = info$year,
        quarter = info$quarter,
        quarter_code = info$quarter_code,
        source_file = file,
        person_weight = info$pwt_name,
        income_weight = info$piwt_name
      ),
      residence_stats(df, eth_origin, occ_scheme)
    )
  })
  bind_rows(rows)
}

build_residence_quarter_panel <- function(root, from_year = NULL) {
  files <- list_lfs_quarter_files(root, from_year = from_year)
  panels <- vector("list", nrow(files))
  for (i in seq_len(nrow(files))) {
    info <- files[i, ]
    file <- lfs_source_name(info$path)
    message("Reading ", info$year, " Q", info$quarter, ": ", file)
    lfs <- read_lfs_microdata(
      info$path, info$pwt_name, info$piwt_name, info$columns[[1]]
    )
    check_survey_quarter(lfs, info$year, info$quarter, file)
    stop_if_codes_present(lfs, file)
    check_labels(lfs, file)
    lfs <- add_derived(lfs, file)
    panels[[i]] <- aggregate_residence_quarter(lfs, info)
  }
  bind_rows(panels) %>% arrange(year, quarter, residence_code)
}
