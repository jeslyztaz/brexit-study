# Quarterly person-level LFS (the files in data/quarterly_lfs_raw).
# Column names and code lists change across vintages. shrink_lfs_cs()
# keeps the column that exists in the file being cleaned, and
# decode_lfs_cs() labels it with that column's own code list.
#
# lfs_cs_candidates is also how the panel reader decides which columns
# to load. Within each entry the first name present in the file wins.
# shrink_lfs_cs() skips that name when every value is a missing code
# and a later name in the same table has a real value.

lfs_cs_candidates <- list(
  CASEID = "CASENOP",
  PERSNO = "PERSNO",
  WEIGHT = c("PWT24", "PWT22", "PWT18", "PWT14"),
  YEAR = "REFWKY",
  MONTH = "REFWKM",
  SEX = "SEX",
  AGE = "AGE",
  HOURPAY = "HOURPAY",
  HRRATE = "HRRATE",
  SAMEGROSS = "USGRS99",
  USUALHRS = "TTUSHR",
  ACTUALHRS = "TTACHR",
  WRKING = "WRKING",
  STATR = "STATR",
  UNDEMP = "UNDEMP",
  ACTIVITY = "ILODEFR",
  WELSH = c("NATIDW11", "NATIDW"),
  SCOTTISH = c("NATIDS11", "NATIDS"),
  OTHERS = c("NATIDO11", "NATIDO"),
  NRTHIRISH = c("NATIDNI", "NATIDI"),
  ENGLISH = c("NATIDE11", "NATIDE"),
  BRITISH = c("NATIDB11", "NATIDB"),
  ETHNICITY = c("ETHUKEUL", "ETHCEN6", "ETH01", "ETHUK14", "ETHUK8"),
  MAXQUALI = c("HIQUAL22", "HIQUAL15", "HIQUAL11", "HIQUAL8"),
  QUALGRP = c("HIQUL22D", "HIQUL15D", "HIQUL11D", "HIQUAL8D", "HIQUAL5D"),
  RESIDENCE = "URESMC",
  REGION = "GOVTOF2",
  OCCUPATION = c("SOC20M", "SOC10M", "SOC2KM"),
  OCCUGRP = c("SC20MMJ", "SC10MMJ", "SC2KMMJ"),
  PUBLICR = "PUBLICR",
  SECTOR = "SECTOR",
  MANAGER = "MANAGER",
  BIRTH_CODE = c("CRYOX7", "CRYOX"),
  BIRTH_MAIN = "CRYOX7_EUL_Main",
  BIRTH_SUB = "CRYOX7_EUL_Sub",
  BIRTH12 = c("CRY12", "CRY01"),
  ARRIVAL_FIRST = "CAMEYR",
  ARRIVAL_LAST = "CAMEYR2",
  ARRIVAL_MONTH = "CAMEMT"
)

lfs_cs_candidate_columns <- function() {
  unique(unlist(lfs_cs_candidates, use.names = FALSE))
}

shrink_lfs_cs <- function(data, weight = NULL) {
  # -9, -8, and -6 are non-response. 2011 Q1 still carries ETHCEN6, but
  # every value is -8; ETHUK14 in that file is filled.
  has_values <- function(nm) {
    x <- data[[nm]]
    if (is.character(x)) x <- suppressWarnings(as.numeric(trimws(x)))
    any(!is.na(x) & !x %in% c(-9, -8, -6))
  }
  choose <- function(candidates) {
    hit <- candidates[candidates %in% names(data)]
    if (length(hit) == 0) return(NA_character_)
    usable <- hit[vapply(hit, has_values, logical(1))]
    if (length(usable) == 0) hit[[1]] else usable[[1]]
  }

  if (is.null(weight)) {
    weight <- choose(lfs_cs_candidates$WEIGHT)
  }
  if (is.na(weight) || !weight %in% names(data)) {
    stop(
      "No person weight found. These quarterly files use PWT14, PWT18, PWT22, or PWT24."
    )
  }

  spec <- c(
    CASEID = choose(lfs_cs_candidates$CASEID),
    PERSNO = choose(lfs_cs_candidates$PERSNO),
    WEIGHT = weight,
    YEAR = choose(lfs_cs_candidates$YEAR),
    MONTH = choose(lfs_cs_candidates$MONTH),
    SEX = choose(lfs_cs_candidates$SEX),
    AGE = choose(lfs_cs_candidates$AGE),
    HOURPAY = choose(lfs_cs_candidates$HOURPAY),
    HRRATE = choose(lfs_cs_candidates$HRRATE),
    SAMEGROSS = choose(lfs_cs_candidates$SAMEGROSS),
    USUALHRS = choose(lfs_cs_candidates$USUALHRS),
    ACTUALHRS = choose(lfs_cs_candidates$ACTUALHRS),
    WRKING = choose(lfs_cs_candidates$WRKING),
    STATR = choose(lfs_cs_candidates$STATR),
    UNDEMP = choose(lfs_cs_candidates$UNDEMP),
    ACTIVITY = choose(lfs_cs_candidates$ACTIVITY),
    WELSH = choose(lfs_cs_candidates$WELSH),
    SCOTTISH = choose(lfs_cs_candidates$SCOTTISH),
    OTHERS = choose(lfs_cs_candidates$OTHERS),
    NRTHIRISH = choose(lfs_cs_candidates$NRTHIRISH),
    ENGLISH = choose(lfs_cs_candidates$ENGLISH),
    BRITISH = choose(lfs_cs_candidates$BRITISH),
    ETHNICITY = choose(lfs_cs_candidates$ETHNICITY),
    MAXQUALI = choose(lfs_cs_candidates$MAXQUALI),
    QUALGRP = choose(lfs_cs_candidates$QUALGRP),
    RESIDENCE = choose(lfs_cs_candidates$RESIDENCE),
    REGION = choose(lfs_cs_candidates$REGION),
    OCCUPATION = choose(lfs_cs_candidates$OCCUPATION),
    OCCUGRP = choose(lfs_cs_candidates$OCCUGRP),
    PUBLICR = choose(lfs_cs_candidates$PUBLICR),
    SECTOR = choose(lfs_cs_candidates$SECTOR),
    MANAGER = choose(lfs_cs_candidates$MANAGER),
    BIRTH_CODE = choose(lfs_cs_candidates$BIRTH_CODE),
    BIRTH_MAIN = choose(lfs_cs_candidates$BIRTH_MAIN),
    BIRTH_SUB = choose(lfs_cs_candidates$BIRTH_SUB),
    BIRTH12 = choose(lfs_cs_candidates$BIRTH12),
    ARRIVAL_FIRST = choose(lfs_cs_candidates$ARRIVAL_FIRST),
    ARRIVAL_LAST = choose(lfs_cs_candidates$ARRIVAL_LAST),
    ARRIVAL_MONTH = choose(lfs_cs_candidates$ARRIVAL_MONTH)
  )
  spec <- spec[!is.na(spec)]

  out <- data %>%
    select(all_of(unname(spec))) %>%
    rename(!!!setNames(unname(spec), names(spec))) %>%
    mutate(
      across(where(is.character), ~ {
        trimmed <- trimws(.x)
        num <- suppressWarnings(as.numeric(trimmed))
        if (all(is.na(num) | trimmed == "")) .x else num
      }),
      across(where(is.numeric), ~ na_if(.x, -9)),
      across(where(is.numeric), ~ na_if(.x, -8)),
      across(where(is.numeric), ~ na_if(.x, -6))
    )

  attr(out, "source_variables") <- spec
  attr(out, "weight_variable") <- unname(weight)
  attr(out, "qualification_variable") <- unname(spec["MAXQUALI"])
  attr(out, "occupation_variable") <- unname(spec["OCCUPATION"])
  out
}

decode_lfs_cs <- function(lfs) {
  label <- function(data, source, dest, levels, labels) {
    if (!source %in% names(data)) return(data)
    data[[dest]] <- factor(data[[source]], levels = levels, labels = labels)
    data
  }

  label_from_source <- function(data, column, dest, schemes) {
    if (!column %in% names(data)) return(data)
    src <- attr(data, "source_variables")
    origin <- if (is.null(src)) NA_character_ else unname(src[[column]])
    if (length(origin) != 1 || is.na(origin) || !origin %in% names(schemes)) {
      return(data)
    }
    scheme <- schemes[[origin]]
    data[[dest]] <- factor(data[[column]], levels = scheme$levels, labels = scheme$labels)
    data
  }

  qual_levels <- 1:7
  qual_labels <- c(
    "Degree or equivalent",
    "Higher education",
    "GCE A level or equivalent",
    "GCSE grades A*-C or equivalent",
    "Other qualifications",
    "No qualification",
    "Don't know"
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

  lfs %>%
    label("WRKING", "WRKING_LABEL", c(1, 2), c("Yes", "No")) %>%
    label("STATR", "STATR_LABEL", 1:4, c(
      "Employee", "Self-employed", "Government scheme", "Unpaid family worker"
    )) %>%
    label("SEX", "SEX_LABEL", c(1, 2), c("Male", "Female")) %>%
    label("ACTIVITY", "ACTIVITY_LABEL", 1:4, c(
      "In employment", "ILO unemployed", "Inactive", "Under 16"
    )) %>%
    label("RESIDENCE", "RESIDENCE_LABEL", 1:20, c(
      "Tyne & Wear", "Rest of Northern region", "South Yorkshire",
      "West Yorkshire", "Rest of Yorks & Humberside", "East Midlands",
      "East Anglia", "Inner London", "Outer London", "Rest of South East",
      "South West", "West Midlands (met county)", "Rest of West Midlands",
      "Greater Manchester", "Merseyside", "Rest of North West", "Wales",
      "Strathclyde", "Rest of Scotland", "Northern Ireland"
    )) %>%
    label("REGION", "REGION_LABEL", c(1, 2, 4:13), c(
      "North East", "North West", "Yorkshire and Humberside",
      "East Midlands", "West Midlands", "East of England", "London",
      "South East", "South West", "Wales", "Scotland", "Northern Ireland"
    )) %>%
    label("PUBLICR", "PUBLIC_LABEL", c(1, 2), c("Private", "Public")) %>%
    label("SECTOR", "SECTOR_LABEL", c(1, 2), c(
      "Private firm or business or Ltd", "Other kind of organisation"
    )) %>%
    label("MANAGER", "MANAGER_LABEL", 1:3, c(
      "Manager", "Foreman or supervisor", "Not manager or supervisor"
    )) %>%
    label("SAMEGROSS", "SAMEGROSS_LABEL", 1:3, c("Yes", "No", "No usual amount")) %>%
    label("UNDEMP", "UNDEMP_LABEL", c(1, 2), c("Yes", "No")) %>%
    label_from_source("ETHNICITY", "ETHNICITY_LABEL", list(
      ETHUKEUL = list(
        levels = 1:9,
        labels = c(
          "White", "Mixed/Multiple ethnic groups", "Indian", "Pakistani",
          "Bangladeshi", "Chinese", "Any other Asian background",
          "Black/African/Caribbean/Black British", "Other ethnic group"
        )
      ),
      ETHCEN6 = list(
        levels = 1:6,
        labels = c(
          "White", "Mixed", "Asian or Asian British",
          "Black or Black British", "Chinese", "Other"
        )
      ),
      ETH01 = list(
        levels = 1:6,
        labels = c(
          "White", "Mixed", "Asian or Asian British",
          "Black or Black British", "Chinese", "Other ethnic group"
        )
      ),
      ETHUK14 = list(
        levels = 1:14,
        labels = c(
          "White", "White and Black Caribbean", "White and Black African",
          "White and Asian", "Asian/Asian British", "Indian", "Pakistani",
          "Bangladeshi", "Chinese", "Any other Asian background", "African",
          "Caribbean", "Any other Black/African/Caribbean background",
          "Any other ethnic group (includes Gypsy, Traveller or Irish Traveller)"
        )
      ),
      ETHUK8 = list(
        levels = 1:8,
        labels = c(
          "White", "Mixed/Multiple ethnic groups", "Indian", "Pakistani",
          "Bangladeshi", "Chinese", "Black/African/Caribbean/Black British",
          "Other ethnic group"
        )
      )
    )) %>%
    label_from_source("QUALGRP", "QUALGRP_LABEL", list(
      HIQUL22D = list(levels = qual_levels, labels = qual_labels),
      HIQUL15D = list(levels = qual_levels, labels = qual_labels),
      HIQUL11D = list(levels = qual_levels, labels = qual_labels),
      HIQUAL8D = list(levels = qual_levels, labels = qual_labels),
      # 2006–2007. Same 1–7 groups; the dictionary text is shorter.
      HIQUAL5D = list(levels = qual_levels, labels = qual_labels)
    )) %>%
    label_from_source("OCCUGRP", "OCCUGRP_LABEL", list(
      SC20MMJ = list(levels = 1:9, labels = soc2010_labels),
      SC10MMJ = list(levels = 1:9, labels = soc2010_labels),
      SC2KMMJ = list(
        levels = 1:9,
        labels = c(
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
      )
    )) %>%
    label("BIRTH_MAIN", "BIRTH_MAIN_LABEL", 1:5, c(
      "UK", "European Union (excluding UK)", "Other Europe",
      "Asia", "Rest of the World"
    )) %>%
    label("BIRTH_SUB", "BIRTH_SUB_LABEL", 1:15, c(
      "UK", "European Union EU15", "European Union EU8", "European Union EU2",
      "European Union Other", "Other Europe", "Middle East and Central Asia",
      "East Asia", "South Asia", "South East Asia", "Sub-Saharan Africa",
      "North Africa", "North America", "Central and South America", "Oceania"
    )) %>%
    label_from_source("BIRTH12", "BIRTH12_LABEL", list(
      CRY12 = list(
        levels = c(356, 372, 586, 616, 921, 922, 923, 924, 926, 997),
        labels = c(
          "India", "Republic of Ireland", "Pakistan", "Poland",
          "England", "Northern Ireland", "Scotland", "Wales",
          "UK, Britain (Did not know country)", "Other"
        )
      ),
      CRY01 = list(
        levels = c(156, 344, 372, 921, 922, 923, 924, 926, 997),
        labels = c(
          "China", "Hong Kong", "Republic of Ireland",
          "England", "Northern Ireland", "Scotland", "Wales",
          "UK, Britain (Dont know country)", "Other"
        )
      )
    )) %>%
    label("ARRIVAL_MONTH", "ARRIVAL_MONTH_LABEL", 1:12, month.name)
}
