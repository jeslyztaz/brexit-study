# Annual residence-year regression data

This file is the annual robustness check. The quarterly analysis in `docs/residence_quarter_regression.md` is the main specification. Person-level rules — age 16–64, URESMC residence, birthplace, missing codes, employment, wages, qualifications, and ethnicity — are defined there and are not repeated here. `functions/lfs_panel_annual.R` calls the same quarterly aggregation after pooling the year's quarters. `functions/construct_variables_annual.R` uses the same ratio, baseline, and leave-one-out definitions as `functions/construct_variables.R`.

`pipeline/01_build_annual_panel.R` reads the quarterly cross-sectional person files in `data/quarterly_lfs_raw`, the same files as `pipeline/01_prep_data.R`. It does not use the longitudinal LFS. `pipeline/02_construct_annual_variables.R` writes `data/processed/residence_annual_regression.csv`. The regressions are in `pipeline/04_main_regressions_annual.R`.

## Residence-year rows

One row is one URESMC residence in one calendar year. The year is January–December: Q1, Q2, Q3, and Q4. A year is used only when all four quarterly files are present. A year with no file, or with fewer than four quarters, is listed in `outputs/annual/annual_year_coverage.csv` and is not given rows. Those years are not treated as complete. In the extract used to build this check, 2026 has Q1 and Q2 only, so 2026 is excluded. The quarterly regression still includes 2026 Q1–Q2.

The LFS rotates addresses, and a respondent can be interviewed in more than one quarter. This build does not link people across quarters and does not use person fixed effects. Each interview stays in its own quarter. The annual figures are pooled cross-sectional labour-market statistics, not individual transitions.

## Population stocks

`pop`, `pop_uk_born`, `pop_eu_born`, `pop_non_eu_born`, `pop_birthplace_unknown`, and `pop_pay` are averages of the four quarterly weighted stocks. The quarterly stock is the sum of that quarter's weights. The annual figure divides each quarterly weight by four and then uses the same sum, which is the arithmetic mean of the four quarterly stocks. The four stocks are not added together.

Unknown birthplace stays in `pop` and out of the three birthplace totals. The three totals and `pop_birthplace_unknown` add up to `pop`, as in the quarterly panel.

`n` and `n_pay_uk_born` are sums of the quarterly person counts. They count person-quarters, not unique people, and they are not divided by four. A person interviewed twice contributes two to `n`. Dividing `n` by four is an average quarterly cell size, not a count of distinct respondents.

## Means, shares, and wages

A mean or share pools the eligible person-quarters and their weights. It is not the unweighted average of the four quarterly means or shares. Dividing every weight by four does not change a ratio, so the annual share equals the ratio of the summed quarterly numerator weights to the summed quarterly denominator weights.

Birth shares use the known-birthplace person weight. `share_birth_uk` is the annual UK-born stock divided by the sum of the UK-born, EU-born, and non-EU-born stocks. Qualification and ethnicity shares use the same denominators as the quarterly panel, pooled across the four quarters.

`mean_log_hourpay` and `mean_log_hourpay_uk_born` use the income weight, not the person weight. The definition is otherwise the quarterly one: natural log of `HOURPAY`, `STATR` employee, `HOURPAY` above 0, and a positive income weight. The UK-born mean also requires a UK birthplace. The annual mean pools those eligible interviews. It is not the average of the four quarterly means, and a weighted median is not the average of the quarterly medians.

A respondent who appears in two quarters contributes to both quarterly stocks, and therefore to the annual average stock through both quarters. That person also contributes two weighted observations to a pooled mean. The annual population is still an average of stocks, so the second interview does not double-count the person on top of a summed population.

## Changes inside a calendar year

Quarterly weights can change vintage inside a year. The annual stock averages the quarterly estimates as published. It does not put the four quarters on one weight vintage.

Birthplace and ethnicity columns can also change inside a year. Each quarter keeps the classification already used by the quarterly pipeline. The annual stocks average those quarterly stocks. People are not reclassified onto one scheme. `birthplace_source`, `birthplace_scheme`, `eth_origin_by_quarter`, and the matching by-quarter columns record the mix. Indian, Pakistani, and Bangladeshi shares are computed inside each quarter, because `ETHCEN6` and `ETH01` code 3 is one Asian group and is not treated as Indian, and are then pooled with the person weights. The six-group ethnicity shares use the groups already harmonised in each quarter.

If the occupation scheme changes inside the year, `share_occ_1` through `share_occ_9` are missing. Those shares are not regression controls.

## Ratios, baselines, and leave-one-out

`native`, `eu`, `non_eu`, and `migrant` are the annual birthplace stocks. `ratio_migrant`, `ratio_eu`, and `ratio_non_eu` divide by `native`. The national ratios use the sums of the twenty area stocks in that year. They are not the average of the area ratios, and they are not the average of the four quarterly national ratios. Because each annual stock is the quarterly average, the ratio of annual sums equals the ratio of the summed quarterly stocks. The division by four cancels.

The 2017–2019 baseline is the sum of annual migrant stocks over the sum of annual native stocks inside that window, by residence. The 2010–2015 baseline is the same construction. These are pooled stocks, not averages of ratios. They equal the quarterly pooled baselines when every year in the window has four quarters. Years after the window are not in the sum. `baseline_national_ratio_*` pools all twenty areas. The `_2010_2015` columns repeat that construction for 2010–2015.

The leave-one-out national ratio is (national stock − the residence stock) / (national native stock − the residence native stock), using the annual stocks. It is not the national ratio minus the residence ratio, and it is not the average of the four quarterly leave-one-out ratios.

## One-year changes and the quarterly lags

A change is matched on the calendar year within residence. If the previous year is absent, the change is missing. It is not taken from the previous row.

`d_annual_*` is the annual level minus the previous calendar year's level. That is the counterpart of the quarterly quarter-to-quarter change, `d_qoq_*`. The main annual regression uses this contemporaneous change, because `pipeline/04_main_regressions.R` uses `d_qoq_*`.

The quarterly file also stores `d_yoy_*_l4`, the year-on-year change dated four quarters earlier. In quarter *t* that is the ratio at *t*−4 minus the ratio at *t*−8, which is the previous year's one-year change. On the annual file that object is `d_annual_*_l1`, equal to `d_annual_*` in year *t*−1, or level[*t*−1] − level[*t*−2]. It is not the contemporaneous annual change. `shift_share_annual_*_l1` is the same lag of the annual shift-share. Neither `_l1` column is in the annual regression, just as `d_yoy_*_l4` is not in the quarterly regression.

At an annual frequency, a one-quarter change and a four-quarter change are not two different operations. The annual file does not create a second copy of `d_annual_*` under a `d_yoy_` name. `d_annual_*` is also not the average of the quarterly quarter-to-quarter changes.

`shift_share_annual_migrant` is `baseline_ratio_migrant` times the one-year change in the leave-one-out national migrant ratio. EU and non-EU use their own baseline ratios and their own leave-one-out ratios. `shift_share_annual_*_2010_2015` uses the 2010–2015 baseline and the same leave-one-out change. The quarterly regression's instrument is `shift_share_qoq_*`, so the annual regression's instrument is `shift_share_annual_*`.

## Regression

`pipeline/04_main_regressions_annual.R` repeats the quarterly specification. The outcome is `d_annual_mean_log_hourpay_uk_born`. One design uses `d_annual_ratio_migrant` and `shift_share_annual_migrant`. The other puts `d_annual_ratio_eu` and `d_annual_ratio_non_eu` in one equation, with `shift_share_annual_eu` and `shift_share_annual_non_eu`. Each is estimated by OLS and 2SLS.

Controls enter in the same order: none, residence and time fixed effects, UK-born ethnicity, UK-born education, then UK-born mean age. The time fixed effect is the calendar year, in `period`. It replaces the quarterly `period` such as `2016Q1`. It is a year fixed effect, not a linear trend. Standard errors are clustered by residence. The sample is `year > 2015` on the complete-year file, with the same complete-case columns as the quarterly regression. The 2017–2019 baseline overlaps that sample, as it does in the quarterly specification.

`d_annual_ratio_*_l1` is not added. No new control, instrument, or estimator is introduced. If a model is collinear or the first stage is weak, the output says so and the specification is left as it is. `hausman_p` is the Wu–Hausman test, joint in the EU and non-EU equation. The first-stage F is the same fixest statistic as in the quarterly table.

The quarterly sample includes 2026 Q1–Q2 because those quarters exist. The annual sample stops at the last complete year, so it does not include an incomplete 2026. That is the coverage rule, not a different `year > 2015` rule.

`covid_asmr` is joined from the same ONS file as the quarterly panel. Years other than 2020 and 2021 are filled with 0. It is not a regressor.

## Outputs

Annual files are written under paths that the quarterly scripts do not use.

- `data/processed/residence_annual_panel.csv`
- `data/processed/residence_annual_regression.csv`
- `outputs/annual/annual_year_coverage.csv`
- `outputs/annual/aggregation_checks.csv`
- `outputs/annual/descriptive_stats.csv`
- `outputs/annual/national_ratios_by_year.csv`
- `outputs/annual/figures/`
- `outputs/annual/main_annual_coefficients.csv`
- `outputs/annual/estimation_notes.csv`
- `outputs/annual/tables/`
- `outputs/annual/quarterly_vs_annual_coefficients.csv`
- `outputs/annual/quarterly_vs_annual_coefficients.txt`
- `outputs/annual/validation_against_quarterly.csv`

`outputs/annual/quarterly_vs_annual_coefficients.csv` places the quarterly coefficient, standard error, p-value, sample size, cluster count, first-stage F, and Wu–Hausman p-value next to the annual ones. The quarterly column is the main specification. The annual column is the robustness check.
