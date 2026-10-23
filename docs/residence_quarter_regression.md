# Residence-quarter regression data

`data/processed/residence_quarter_regression.csv` is the file to load into a regression. `pipeline/02_construct_variables.R` builds it from `data/processed/residence_quarter_panel_full.csv`. Those formulas are in `functions/construct_variables.R`. `pipeline/01_prep_data.R` builds the panel from the quarterly person files in `data/quarterly_lfs_raw`. The person-level recodes are in `functions/data_cleaners.R` and `functions/lfs_panel.R`.

One row is one URESMC residence area in one calendar quarter. Ages are 16–64. The current full panel, and therefore this file, runs from 2006 Q1 through 2026 Q2, twenty areas. No rows are dropped for the sample. A change is missing when the earlier quarter is not in the file.

## Person records

Each person file is one calendar quarter. The quarter comes from the filename: `jm` is Q1, `aj` is Q2, `js` is Q3, and `od` is Q4. The two-digit year in the filename is 19xx when it is 92 or above, and 20xx when it is below 92. Everyone in the file is given that year and quarter.

`REFWKY` is the reference year and `REFWKM` is the reference month. They check the filename quarter. The modal `REFWKY` has to be unique and equal the filename year. At least 80% of non-missing `REFWKM` values have to fall in that quarter's months: Q1 is January–March, Q2 April–June, Q3 July–September, and Q4 October–December. A person whose reference month sits outside those three months stays in the file's quarter.

Each quarter from 2006 Q1 through 2026 Q2 has one person file in that folder. If two files shared a quarter, the file with the higher `PWT` number would be kept. If those numbers were equal, the file with the higher UKDA study number in the `UKDA-*-tab` folder name would be kept.

A person is kept when `AGE` is from 16 to 64. A missing age is left out. `URESMC` codes 1–20 are the residence names, in order: Tyne & Wear, Rest of Northern region, South Yorkshire, West Yorkshire, Rest of Yorks & Humberside, East Midlands, East Anglia, Inner London, Outer London, Rest of South East, South West, West Midlands (met county), Rest of West Midlands, Greater Manchester, Merseyside, Rest of North West, Wales, Strathclyde, Rest of Scotland, Northern Ireland. A person aged 16–64 with a missing `URESMC` is left out. Each quarter still has one row for every area. An area with no positive person weight has `pop` of 0.

On the numeric columns that are read, -9, -8, and -6 are set to missing before any total, share, or mean. Those three codes are non-response.

The person weight is the `PWT` column with the highest number. The income weight is the `PIWT` column with the highest number. In this build those columns are `PWT14` and `PIWT14` from 2006 Q1 through 2011 Q2, `PWT18` and `PIWT18` from 2011 Q3 through 2018 Q4, `PWT24` and `PIWT18` in 2019, `PWT24` and `PIWT22` from 2020 Q1 through 2023 Q3, `PWT24` and `PIWT23` from 2023 Q4 through 2024 Q3, and `PWT24` and `PIWT24` from 2024 Q4. A weight is used only when it is non-missing and above 0. Birthplace, qualification, ethnicity, age, and `pop` use the person weight. Pay uses the income weight.

Birthplace, qualification, and ethnicity change column name across vintages. For each of those, the quarter uses the first listed name that is on the file and has at least one value other than -9, -8, and -6. If every present name is only those codes, the first present name is kept.

A share is the total weight of people in the category divided by the total weight of people in its denominator. A person is in the denominator when the weight is above 0, the classified value is non-missing, and any further restriction for that share is met. A missing value is in neither part. A mean is the weighted average on the same basis. With nobody in the denominator, the share or mean is missing.

`pop` is the sum of the person weights in the area. A missing birthplace stays inside `pop`.

## Birthplace

Birthplace is UK, EU, or non-EU. Unknown birthplace is in neither group. The EU group is the EU28 excluding the UK, including Croatia, and it does not change with the year of the survey. The country column is the first of `CRYOX7` and `CRYOX` that is on the file and has a value other than -9, -8, or -6. `CRYOX7_EUL_Main` is used only when neither country column has a usable value. A missing row on the chosen column stays missing. It is not filled from another birthplace column. `CRYO` is a different 2006 code list and is not used. `CRYO7` is the same code list as `CRYOX7` but is filled only for a subset of rows; it is compared with `CRYOX7` and is not the classification. In these files, `CRYOX` is the 2006 column, `CRYOX7` is the column from 2007 Q1 through 2017 Q2, and `CRYOX7_EUL_Main` is the column from 2017 Q3. `birthplace_source` records which of those three columns was used. `birthplace_scheme` records the code list.

On `CRYOX7_EUL_Main`, 1 is UK, 2 is EU, and 3–5 are non-EU. Those three non-UK codes are Other Europe, Asia, and Rest of the World. Value 2 is the ONS aggregate of EU15, EU8, EU2, and EU Other on `CRYOX7_EUL_Sub` (LFS User Guide Volume 4).

On `CRYOX`, code 1 is UK, Britain. The 2006 dictionary has no separate Channel Islands or Isle of Man code. Where the partial `CRYO` column does record them, they sit inside `CRYOX` code 1, so those births cannot be removed from UK-born. EU is codes 2–12, 14–17, 19, 21, 26–30. That is the EU27 where this list has its own code: the EU15 countries on the list, Cyprus (15), Gibraltar (16), Malta (19), and the 2004 and 2007 members that have their own code, including former Czechoslovakia (27). Gibraltar and Czechoslovakia are included because User Guide Volume 4 puts their `CRYOX7` counterparts, 292 and 971, in the EU aggregate, and both successor states of Czechoslovakia are in the EU27. Cyprus is one code on this list. Former Yugoslavia is 24 and the former USSR is 31. Those two mix EU and non-EU successor states and stay unclassified, so they are not added to non-EU. Czechia and Slovakia are inside code 27. The Baltic states are inside code 31, and Slovenia and Croatia are inside code 24, so 2006 cannot separate them. At sea or in the air (102) and stateless (103) also stay unclassified. Any other code from 1 to 103 is non-EU.

On `CRYOX7`, codes 921–926 are UK. The end-user column collapses England, Northern Ireland, Scotland, Wales, the Channel Islands, and the Isle of Man into 926, which is what Volume 3 says and what the files contain. `CRYO7` still separates them: 831 Guernsey, 832 Jersey, 833 Isle of Man, and 931 Channel Islands not otherwise specified are crown dependencies, and 921–925 are the constituent countries and Great Britain not otherwise specified. People coded 926 are UK-born, including the crown-dependency births that can no longer be separated. That matches `CRYOX7_EUL_Main` value 1. EU follows the Volume 4 aggregate, not ISO country numbers. Spain is 911 (except the Canary Islands), 912 (Canary Islands), and 913 (Spain not otherwise specified). There is no code 724. Cyprus (European Union) is 901, Cyprus (non-European Union) is 902, and Cyprus not otherwise specified is 903. There is no code 196. The EU codes are Austria 40, Belgium 56, Bulgaria 100, Croatia 191, Cyprus 901, 902, and 903, Czechia 203, Denmark 208, Estonia 233, Finland 246, Åland 248, France 250, Germany 276, Gibraltar 292, Greece 300, Hungary 348, Ireland 372, Italy 380, Latvia 428, Lithuania 440, Luxembourg 442, Malta 470, the Netherlands 528, Poland 616, Portugal 620, Romania 642, Slovakia 703, Slovenia 705, Spain 911, 912, and 913, Sweden 752, and Czechoslovakia not otherwise specified 971. A documented code outside that set and outside 921–926 is non-EU, except at sea (991) and in the air (992), which stay unclassified. That non-EU set includes the USSR not otherwise specified (972), Yugoslavia not otherwise specified (973), and Europe not otherwise specified (981): Volume 4 puts those residuals in Other Europe after the EU successor states have their own codes. A code with no UKDA label stops the build. French overseas departments are separate codes and Volume 4 does not put them in the EU aggregate, so they stay non-EU. Code 891 is Kosovo in the 2007–2011 dictionaries and Vojvodina in the later user guide; both are non-EU.

`share_birth_uk`, `share_birth_eu`, and `share_birth_non_eu` are person-weighted shares among residents with a known birthplace and a positive person weight.

## Qualification

The qualification column is the first usable name among `HIQUL22D`, `HIQUL15D`, `HIQUL11D`, `HIQUAL8D`, and `HIQUAL5D`. On these files that is `HIQUAL5D` from 2006 Q1 through 2007 Q4, `HIQUAL8D` from 2008 Q1 through 2010 Q4, `HIQUL11D` from 2011 Q1 through 2014 Q4, `HIQUL15D` from 2015 Q1 through 2021 Q4, and `HIQUL22D` from 2022 Q1. All five are read as codes 1–7 with the same groups. The printed dictionary text on `HIQUAL5D` is shorter; the codes are still these groups:

1. Degree or equivalent
2. Higher education
3. GCE A level or equivalent
4. GCSE grades A*-C or equivalent
5. Other qualifications
6. No qualification
7. Don't know

The panel's qualification shares are person-weighted. Code 7 is a classified value, so "Don't know" is in the denominator and the seven shares sum to 1. A qualification left missing by -9, -8, or -6 is outside that denominator. The UK-born shares use the same codes and the same person weight, and they also require a UK birthplace from the grouping above. The regression then drops "Don't know" from its own denominator. That step is under UK-born controls.

## Ethnicity

The ethnicity column is the first usable name among `ETHUKEUL`, `ETHCEN6`, `ETH01`, `ETHUK14`, and `ETHUK8`. A column that is present but filled only with -9, -8, or -6 is skipped, and the next name with a real value is used. From 2006 Q1 through 2010 Q4 the column used is `ETHCEN6`. `ETH01` is also on those files; `ETHCEN6` is earlier in the list and is the one kept. 2011 Q1 uses `ETHUK14`. On that file `ETHCEN6` has no value other than -9, -8, or -6, and `ETHUK8` is filled but comes later in the list. From 2011 Q2 the column is `ETHUKEUL`. Each code is placed in one of six groups: white, mixed, Asian, Chinese, black, or other. Chinese stays out of Asian.

`ETHCEN6`: 1 white, 2 mixed, 3 Asian or Asian British, 4 black or Black British, 5 Chinese, 6 other.

`ETH01`: 1 white, 2 mixed, 3 Asian or Asian British, 4 black or Black British, 5 Chinese, 6 other ethnic group.

`ETHUKEUL`: 1 white, 2 mixed, 3 Indian, 4 Pakistani, 5 Bangladeshi, 6 Chinese, 7 any other Asian background, 8 black, 9 other. Codes 3, 4, 5, and 7 are Asian.

`ETHUK8`: 1 white, 2 mixed, 3 Indian, 4 Pakistani, 5 Bangladeshi, 6 Chinese, 7 black, 8 other. Codes 3, 4, and 5 are Asian.

`ETHUK14`: 1 white. Codes 2–4 are mixed: White and Black Caribbean, White and Black African, and White and Asian. Codes 5, 6, 7, 8, and 10 are Asian: Asian or Asian British, Indian, Pakistani, Bangladeshi, and any other Asian background. Code 9 is Chinese. Codes 11–13 are black: African, Caribbean, and any other Black background. Code 14 is other: any other ethnic group, which on this list includes Gypsy, Traveller or Irish Traveller.

On `ETHCEN6` and `ETH01`, Asian is the single Asian or Asian British code. On the other three columns, Asian is the sum of the Indian, Pakistani, Bangladeshi, and other-Asian codes, and on `ETHUK14` it also includes code 5.

The panel's six ethnicity shares are person-weighted among residents with a known group, and they sum to 1. The UK-born shares use the same groups and also require a UK birthplace. The regression columns that keep Asian, black, Chinese, and mixed plus other are under UK-born controls.

## Keys

`residence` is the URESMC area name from the list above. `year` and `quarter` are the filename year and quarter. `period` is those two pasted together, such as `2016Q1`, for a quarter fixed effect.

## Counts

`pop` on the panel is the sum of the person weights of residents aged 16–64, including people whose birthplace is missing. Each birth share is the person-weighted share among residents with a known birthplace and a positive person weight.

`pop_uk_born`, `pop_eu_born`, and `pop_non_eu_born` are the sums of the person weights for residents whose birthplace is UK, EU, and non-EU. Missing birthplace is in none of the three. A group with nobody in it is 0, including an area with `pop` of 0. `pop_birthplace_unknown` is the person-weighted total with missing birthplace. `birthplace_missing_rate` is that total divided by `pop`, and it is missing when `pop` is 0. The three birthplace totals and `pop_birthplace_unknown` add up to `pop`.

`native`, `eu`, and `non_eu` are those three birthplace totals. `migrant` = `eu` + `non_eu`. The regression file keeps `native`, `eu`, `non_eu`, and `migrant`. The `pop_*` totals and `birthplace_missing_rate` stay on the panel.

## Ratios

`ratio_migrant` = `migrant` / `native`. `ratio_eu` = `eu` / `native`. `ratio_non_eu` = `non_eu` / `native`. The ratio is missing when `native` is missing or not positive. Where `share_birth_uk` is positive, `ratio_migrant` = (`share_birth_eu` + `share_birth_non_eu`) / `share_birth_uk`. The EU and non-EU ratios match their shares in the same way. The known-birthplace total sits in every share, so it cancels.

`national_ratio_migrant`, `national_ratio_eu`, and `national_ratio_non_eu` use the same formulas on the sums of the area counts in that quarter. They are not the average of the area ratios. Missing birthplace is not in those counts, so an area with more missing birthplace does not receive a larger weight in the national total.

## Changes

A quarter-to-quarter change, `d_qoq_`, is the column in this quarter minus the same column one quarter earlier. A year-on-year change, `d_yoy_`, subtracts the value four quarters earlier. The earlier quarter is the calendar quarter, not the previous row. If that quarter is absent, the change is missing.

Area columns are matched within `residence`. National columns are matched on the calendar only, so every area in a quarter gets the same national change.

`d_qoq_ratio_migrant`, `d_qoq_ratio_eu`, `d_qoq_ratio_non_eu`
`d_yoy_ratio_migrant`, `d_yoy_ratio_eu`, `d_yoy_ratio_non_eu`
`d_qoq_national_ratio_migrant`, `d_qoq_national_ratio_eu`, `d_qoq_national_ratio_non_eu`
`d_yoy_national_ratio_migrant`, `d_yoy_national_ratio_eu`, `d_yoy_national_ratio_non_eu`
`d_qoq_mean_log_hourpay`, `d_qoq_mean_log_hourpay_uk_born`
`d_yoy_mean_log_hourpay`, `d_yoy_mean_log_hourpay_uk_born`
`d_qoq_mean_age_uk_born`, `d_qoq_share_uk_degree`, `d_qoq_share_uk_low_qual`
`d_qoq_share_uk_asian`, `d_qoq_share_uk_black`, `d_qoq_share_uk_chinese`, `d_qoq_share_uk_eth_other`

`d_yoy_ratio_migrant_l4`, `d_yoy_ratio_eu_l4`, and `d_yoy_ratio_non_eu_l4` are the year-on-year ratio changes dated four quarters earlier. In quarter *t* that is the ratio at *t*−4 minus the ratio at *t*−8.

## Baselines

The baseline window is 2017 Q1 through 2019 Q4 inclusive. A quarter in that window is used only when `native`, `eu`, and `non_eu` are all finite. This window includes 2017 Q1–Q2, when birthplace is `CRYOX7`, and 2017 Q3 through 2019 Q4, when it is `CRYOX7_EUL_Main`.

`baseline_ratio_migrant` = sum of `migrant` / sum of `native` over that window, inside the residence. `baseline_ratio_eu` and `baseline_ratio_non_eu` use `eu` and `non_eu` as the numerator. This is the pooled stock, not the average of the quarterly ratios. The sums use the birthplace totals, so a quarter with more missing birthplace is not scaled up. The same value is written on every quarter for that residence, including quarters outside 2017–2019.

`baseline_national_ratio_migrant`, `baseline_national_ratio_eu`, and `baseline_national_ratio_non_eu` are the same pooled ratios with the sums taken over all twenty areas. Each one is a single number, repeated on every row.

The same construction is also stored for 2010 Q1 through 2015 Q4. Those columns end in `_2010_2015`: `baseline_ratio_migrant_2010_2015`, `baseline_ratio_eu_2010_2015`, `baseline_ratio_non_eu_2010_2015`, and the three `baseline_national_ratio_*_2010_2015` columns. A quarter in that window is used only when `native`, `eu`, and `non_eu` are all finite.

## Wages

`mean_log_hourpay` is copied from the panel. It is the income-weighted mean of log(`HOURPAY`) among people with `STATR` equal to 1, employee, `HOURPAY` above 0, and a positive income weight. `STATR` 2 is self-employed, 3 is a government scheme, and 4 is an unpaid family worker. The log is the natural log. This is the mean of the log, not the log of the mean.

`mean_log_hourpay_uk_born` is the same mean among employees who are also UK-born. Unknown birthplace is left out.

`n_pay` counts the employees in `mean_log_hourpay`: `STATR` employee, `HOURPAY` above 0, and a positive income weight. `n_pay_uk_born`, `n_pay_eu_born`, and `n_pay_non_eu_born` are that count inside each birthplace group. Unknown birthplace is in `n_pay` only. The regression file keeps `n_pay_uk_born`. It is not a sample filter and it is not a regressor. A cell with nobody in the UK-born wage sample has `n_pay_uk_born` of 0 and a missing `mean_log_hourpay_uk_born`.

## Shift-share

For each area and quarter, the leave-one-out national ratio keeps every area except that one. For migrants it is (national migrant count − the area's `migrant`) / (national native count − the area's `native`). EU and non-EU use their own numerators and the same native denominator. The national counts are the sums of the area counts.

The unsuffixed shift-shares use the 2017–2019 baseline. `shift_share_qoq_migrant` = `baseline_ratio_migrant` × the quarter-to-quarter change in that leave-one-out migrant ratio. `shift_share_yoy_migrant` uses the year-on-year change. The EU columns use `baseline_ratio_eu` and the leave-one-out EU ratio. The non-EU columns use `baseline_ratio_non_eu` and the leave-one-out non-EU ratio.

`shift_share_qoq_eu`, `shift_share_qoq_non_eu`, `shift_share_yoy_eu`, `shift_share_yoy_non_eu`

`shift_share_yoy_migrant_l4`, `shift_share_yoy_eu_l4`, and `shift_share_yoy_non_eu_l4` are those year-on-year shift-shares dated four quarters earlier. The leave-one-out ratios themselves are not in the file.

`shift_share_qoq_migrant_2010_2015` = `baseline_ratio_migrant_2010_2015` × the same quarter-to-quarter leave-one-out change. The EU, non-EU, and year-on-year columns follow the same pattern: `shift_share_qoq_eu_2010_2015`, `shift_share_qoq_non_eu_2010_2015`, `shift_share_yoy_migrant_2010_2015`, `shift_share_yoy_eu_2010_2015`, and `shift_share_yoy_non_eu_2010_2015`. `shift_share_yoy_migrant_2010_2015_l4`, `shift_share_yoy_eu_2010_2015_l4`, and `shift_share_yoy_non_eu_2010_2015_l4` are those year-on-year shift-shares dated four quarters earlier.

## UK-born controls

The levels are contemporaneous. They are person-weighted, ages 16–64, and restricted to UK-born residents. Unknown birthplace is not in the denominator.

`mean_age_uk_born` is copied from the panel: the person-weighted mean of `AGE` among UK-born residents in the 16–64 sample. `AGE` is not recoded inside that range.

The qualification controls start from the panel's UK-born qualification shares, built from codes 1–7 above. Those shares are among UK-born residents with a non-missing qualification, so "Don't know" is inside their denominator and the seven shares sum to 1. `share_uk_degree` divides the degree share by the sum of the shares for degree, higher education, A-level, GCSE, other qualifications, and no qualification. "Don't know" is left out of that sum, which puts it out of the denominator. The degree label is "Degree or equivalent".

`share_uk_low_qual` uses the same denominator. Its numerator is the sum of "GCSE grades A*-C or equivalent", "Other qualifications", and "No qualification". Higher education and A-level are not columns.

`d_qoq_mean_age_uk_born`, `d_qoq_share_uk_degree`, and `d_qoq_share_uk_low_qual` are those three controls minus their values one calendar quarter earlier in the same residence. The level of each control remains in the file.

The ethnicity controls start from the panel's six UK-born ethnicity shares. The UKDA codes behind those groups are listed under Ethnicity. Those shares are among UK-born residents with a known ethnicity group, and they sum to 1. Chinese is kept out of Asian. Where the LFS lists them separately, Asian is Indian, Pakistani, Bangladeshi, and other Asian, and on `ETHUK14` it also includes the Asian or Asian British code.

`share_uk_asian`, `share_uk_black`, and `share_uk_chinese` divide the matching panel share by the sum of the six ethnicity shares. When that sum is 1, each equals the panel share. `share_uk_eth_other` divides the sum of the mixed share and the residual other share by the same total. White is not a column.

`d_qoq_share_uk_asian`, `d_qoq_share_uk_black`, `d_qoq_share_uk_chinese`, and `d_qoq_share_uk_eth_other` are those four shares minus their values one calendar quarter earlier in the same residence. The level of each share remains in the file.

## Regression

The estimated equations are in `outputs/regressions/quarterly/tables/`. The outcome is `d_qoq_mean_log_hourpay_uk_born`. One design uses `d_qoq_ratio_migrant`, and the 2SLS table instruments it with `shift_share_qoq_migrant`. The other puts `d_qoq_ratio_eu` and `d_qoq_ratio_non_eu` in one equation, instrumented by `shift_share_qoq_eu` and `shift_share_qoq_non_eu`. Each design is estimated by OLS and 2SLS. Standard errors are clustered by residence.

The sample printed in these tables is 2021 Q1 through 2026 Q2. Every column has 440 observations and 20 residence clusters. Rows from 2006 Q1 through 2020 Q4 stay on this file. The 2017–2019 baseline is outside the estimation sample and enters the 2SLS tables through `shift_share_qoq_*`. `d_yoy_*_l4` and `shift_share_yoy_*_l4` stay on this file and do not appear in the tables.

The text tables add controls in this order. The fixed-effect block shows which fixed effects are on. The header `year FE` is the column label in the text table; that column has both residence and year fixed effects.

0. `none`. Residence, year, and period fixed effects are off.
1. `year FE`. Residence and year fixed effects are on. Period fixed effects are off.
2. `+ ethnicity`. Adds `d_qoq_share_uk_asian`, `d_qoq_share_uk_black`, `d_qoq_share_uk_chinese`, and `d_qoq_share_uk_eth_other`.
3. `+ covid`. Adds `covid_asmr_2020_y2022` through `covid_asmr_2020_y2026`. Each is the 2020 age-standardised mortality rate from `data/processed/ons_covid_by_area_20.csv`, interacted with that year. 2021 is the omitted year. These interactions are built for the regression and are not columns of this file.
4. `+ education`. Adds `d_qoq_share_uk_degree` and `d_qoq_share_uk_low_qual`.
5. `+ age`. Adds `d_qoq_mean_age_uk_born`. Residence and year fixed effects stay on. This is the reference column for the step-5 figures under `outputs/results/`.
6. `period FE`. The same regressors as `+ age`. Residence fixed effects stay on, year fixed effects go off, and period fixed effects go on. `period` is the calendar quarter, such as `2021Q1`.

The ethnicity, education, and age rows are the quarter-to-quarter changes. Their levels remain on this file. `hausman_p` is the Wu–Hausman test, joint in the EU and non-EU equation. The EU and non-EU 2SLS table prints the first-stage F as `first_stage_f_eu` and `first_stage_f_non_eu`.

The quarterly panel has a separate `covid_asmr` column: the 2020 rate in 2020, the 2021 rate in 2021, and 0 in every other year. That column is not on this regression file. The COVID rows in the tables are the 2020 rate interacted with year.
