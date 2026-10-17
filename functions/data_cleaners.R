decode_lfs <- function(lfs) {
  lfs_decoded <- lfs %>%
    mutate(
      across(where(is.numeric), ~ na_if(., -9)),
      across(where(is.numeric), ~ na_if(., -8)),
      FLOW_LABEL = case_when(
        FLOW == 1 ~ "Aged 15 at both quarters",
	      FLOW == 2 ~ "Entrant to working-age between first and final quarter",
	      FLOW == 3 ~ "In employment at first quarter; in employment at final quarter (EE)",
	      FLOW == 4 ~ "In employment at first quarter; unemployed at final quarter (EU)",
	      FLOW == 5 ~ "In employment at first quarter; inactive at final quarter (EN)",
	      FLOW == 6 ~ "Unemployed at first quarter; in employment at final quarter (UE)",
	      FLOW == 7 ~ "Unemployed at first quarter; unemployed at final quarter (UU)",
	      FLOW == 8 ~ "Unemployed at first quarter; inactive at final quarter (UN)",
	      FLOW == 9 ~ "Inactive at first quarter; in employment at final quarter (NE)",
	      FLOW == 10 ~ "Inactive at first quarter; unemployed at final quarter (NU)",
	      FLOW == 11 ~ "Inactive at first quarter; inactive at final quarter (NN)",
	      FLOW == 12 ~ "Reached retirement age by final quarter"),
      ANFLOW_LABEL = case_when(
        anflow == 0 ~ "Entering",
        anflow == 1 ~ "In employment in all quarters (E)",
        anflow == 2 ~ "Unemployed in all quarters (U)",
        anflow == 3 ~ "Inactive in all quarters (N)",
        anflow == 4 ~ "Employed at first quarter; unemployed at final quarter (EU)",
        anflow == 5 ~ "Employed at first quarter; inactive at final quarter (EN)",
        anflow == 6 ~ "Unemployed at first quarter; inactive at final quarter (UN)",
        anflow == 7 ~ "Unemployed at first quarter; employed at final quarter (UE)",
        anflow == 8 ~ "Inactive at first quarter; employed at final quarter (NE)",
        anflow == 9 ~ "Inactive at first quarter; unemployed at final quarter (NU)",
        anflow == 10 ~ "Employed at first; unemployed; employed at final quarter (EUE)",
        anflow == 11 ~ "Employed at first; inactive; employed at final quarter (ENE)"),
      WRKING_LABEL = factor(SEX, levels = c(1, 2), labels = c("Yes", "No")),
      STATR_LABEL = case_when(
        STATR1 == 1 ~ "Employee",
        STATR1 == 2 ~ "Self-employed",
        STATR1 == 3 ~ "Government scheme",
        STATR1 == 4 ~ "Unpaid family worker"),
      SEX_LABEL = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
      RESIDENCE_LABEL = case_when(
        RESIDENCE == 1 ~ "Tyne & Wear",
        RESIDENCE == 2 ~ "Rest of Northern region",
        RESIDENCE == 3 ~ "South Yorkshire",
        RESIDENCE == 4 ~ "West Yorkshire",
        RESIDENCE == 5 ~ "Rest of Yorks & Humberside",
        RESIDENCE == 6 ~ "East Midlands",
        RESIDENCE == 7 ~ "East Anglia",
        RESIDENCE == 8 ~ "Inner London",
        RESIDENCE == 9 ~ "Outer London",
        RESIDENCE == 10 ~ "Rest of South East",
        RESIDENCE == 11 ~ "South West",
        RESIDENCE == 12 ~ "West Midlands (Met County)",
        RESIDENCE == 13 ~ "Rest of West Midlands",
        RESIDENCE == 14 ~ "Greater Manchester",
        RESIDENCE == 15 ~ "Merseyside",
        RESIDENCE == 16 ~ "Rest of North West",
        RESIDENCE == 17 ~ "Wales",
        RESIDENCE == 18 ~ "Strathclyde",
        RESIDENCE == 19 ~ "Rest of Scotland",
        RESIDENCE == 20 ~ "Northern Ireland"),
      PUBLIC_LABEL = factor(PUBLICR1, levels = c(1, 2), labels = c("Private", "Public")),
      SECTOR_LABEL = factor(SECTOR1, levels = c(1, 2), 
                            labels = c("Private firm or business or Ltd",
                                       "Other kind of organisation")),
      MANAGER_LABEL = case_when(
        MANAGER1 == 1 ~ "Manager",
        MANAGER1 == 2 ~ "Foreman or supervisor",
        MANAGER1 == 3 ~ "Not manager or supervisor"),
      SAMEGROSS_LABEL = case_when(
        SAMEGROSS == 1 ~ "Yes",
        SAMEGROSS == 2 ~ "No",
        SAMEGROSS == 3 ~ "No usual amount"),
      UNDEMP_LABEL = case_when(
        UNDEMP1 == 1 ~ "Yes",
        UNDEMP1 == 2 ~ "No"),
      ETHNICITY_LABEL = case_when(
        ETHNICITY == 1 ~ "White",
        ETHNICITY == 2 ~ "Mixed/Multiple",
        ETHNICITY == 3 ~ "Indian",
        ETHNICITY == 4 ~ "Pakistani",
        ETHNICITY == 5 ~ "Bangladeshi",
        ETHNICITY == 6 ~ "Chinese",
        ETHNICITY == 7 ~ "Any other Asian background",
        ETHNICITY == 8 ~ "Black/African/Caribbean/Black British",
        ETHNICITY == 9 ~ "Other"
      )
    )
  return(lfs_decoded)


shrink_data <- function(data, lgwtcol) {
  data_shrunk <- data %>%
    select(
      # identification
      PERSID, all_of(lgwtcol), FLOW,
      # wages
      HOURPAY1, HRRATE1, USGRS991, TTUSHR1, TTACHR1,
      # employment
      WRKING1, STATR1, anflow, UNDEMP1,
      # national identity
      NAIDW111, NAIDS111, NAIDO111, NATIDNI1, NAIDE111, NAIDB111,
      # ethnicity
      ETUKEUL2,
      # individual controls
      SEX, HIQUA151, URESMC1,
      # market controls
      SOC10M1, SC10MMJ1, PUBLICR1, SECTOR1, MANAGER1) %>%
    rename(
      SAMEGROSS = USGRS991, # same gross pay as previous periods
      USUALHRS = TTUSHR1,
      ACTUALHRS = TTACHR1, # total hours worked in actual week
      WELSH = NAIDW111,
      SCOTTISH = NAIDS111,
      OTHERS = NAIDO111,
      NRTHIRISH = NATIDNI1,
      ENGLISH = NAIDE111,
      BRITISH = NAIDB111,
      ETHNICITY = ETUKEUL2,
      MAXQUALI = HIQUA151,
      RESIDENCE = URESMC1,
      OCCUPATION = SOC10M1,
      OCCUGRP = SC10MMJ1
    )
  return(data_shrunk)
}