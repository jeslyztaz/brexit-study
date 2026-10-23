# Outputs

Figures and tables are grouped by purpose. Quarterly and annual descriptive charts are separate. Regression tables are separate from descriptive statistics. Figures that need fitted models are under `results/`.

`main.R` runs the steps in this order:

1. `pipeline/01_prep_data.R` and `pipeline/02_construct_variables.R`
2. `pipeline/03_descriptive_analysis.R`
3. `pipeline/04_main_regressions.R`
4. `pipeline/01_build_annual_panel.R`, `pipeline/02_construct_annual_variables.R`, `pipeline/03_descriptive_analysis_annual.R`, and `pipeline/04_main_regressions_annual.R`
5. `pipeline/05_compare_quarterly_annual.R`
6. `pipeline/06_results_analysis.R`

Step 03 reads the constructed regression file. It does not read coefficients, fitted models, or files written by step 04. Step 06 reads the coefficient files and refits the reference specification, step 5, for the partial-regression and leave-one-region-out figures. That refit is checked against the coefficient file before those figures are drawn.

The reference specification is residence and year fixed effects, UK-born ethnicity, the 2020 COVID mortality rate interacted with year, UK-born education, and UK-born age. The sample is 2021 Q1 onward. The instrument uses the pooled 2017–2019 baseline. Quarterly step 6, which replaces year fixed effects with quarter fixed effects, is shown in the main coefficient plot and has no annual counterpart.

The annual pipeline reads the raw Labour Force Survey files again. It does not rewrite quarterly outputs.

## Where to look

| Location | Contents |
| --- | --- |
| `descriptive/quarterly/` | Quarterly charts and the small tables behind them |
| `descriptive/annual/` | Annual charts and descriptive statistics |
| `regressions/quarterly/` | Main quarterly coefficients, text tables, and regression diagnostics |
| `regressions/annual/` | Annual coefficients, text tables, and the quarterly-versus-annual coefficient table |
| `results/main/` | Coefficient plot and residualised binned scatter |
| `results/first_stage/` | Step-5 first-stage figure and its diagnostic table |
| `results/robustness/` | Quarterly-versus-annual coefficients and leave-one-region-out |
| `metadata/` | Year coverage, aggregation checks, validation, and figures that were intentionally not produced |

Paths are created by `output_file()` in `setup.R`. PNG files are written at 300 dpi.

The earlier copies under `outputs/figures/`, `outputs/tables/`, `outputs/annual/`, and `outputs/main_qoq_coefficients.csv` have been removed. Each had a replacement in this tree. The old coefficient files matched the current estimates; the current files add the `t_df` column used for the confidence intervals.

## Figures not produced

`metadata/figure_omissions.csv` records three figures that are not estimated:

- Sample-window re-estimates. A window that drops 2021 cannot keep 2021 as the omitted COVID year.
- A four-quarter step-5 model. The composition controls exist only as quarter-to-quarter changes.
- A second specification-stability plot. The main coefficient plot already shows steps 0–6.

The 2017–2019 baseline bar chart and the annual EU heatmap are written once, from the quarterly descriptive script, when the annual stocks match those quarterly pooled ratios.

## Figure inventory

| File | Description | Script | Input | Regression required | Location |
| --- | --- | --- | --- | --- | --- |
| `ratio_levels_by_residence.png` | Migrant, EU, and non-EU ratio levels by residence | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/levels/` |
| `national_ratio_levels.png` | National migrant, EU, and non-EU ratio levels | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/levels/` |
| `eu_ratio_heatmap_annual.png` | EU ratio by residence and complete year, ordered by the 2017–2019 baseline | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/levels/` |
| `d_qoq_ratio_migrant_by_residence.png` | Quarter-to-quarter migrant-ratio changes | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/changes/` |
| `d_qoq_ratio_eu_non_eu_by_residence.png` | Quarter-to-quarter EU and non-EU ratio changes | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/changes/` |
| `national_d_qoq_ratios.png` | National quarter-to-quarter ratio changes | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/changes/` |
| `post2021_ratio_change_distributions.png` | Post-2021 distributions of quarter-to-quarter ratio changes | `03_descriptive_analysis.R` | Quarterly regression file and COVID file | No | `descriptive/quarterly/migration/changes/` |
| `qoq_vs_yoy_distributions_post2021.png` | Post-2021 quarter-to-quarter and year-on-year ratio-change distributions | `03_descriptive_analysis.R` | Quarterly regression file and COVID file | No | `descriptive/quarterly/migration/changes/` |
| `qoq_vs_yoy_national_post2021.png` | Post-2021 national quarter-to-quarter and year-on-year ratio changes | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/changes/` |
| `baseline_eu_2017_2019.png` | 2017–2019 EU baseline by residence | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/exposure/` |
| `baseline_eu_non_eu_2017_2019.png` | 2017–2019 EU and non-EU baselines | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/exposure/` |
| `baseline_vs_2021_2025_change.png` | 2017–2019 baseline against the 2025-minus-2021 pooled ratio change | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/exposure/` |
| `baseline_migrant_vs_2021_2025_change.png` | 2017–2019 migrant baseline against the 2025-minus-2021 pooled migrant-ratio change | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/migration/exposure/` |
| `d_qoq_mean_log_hourpay_uk_born_by_residence.png` | Quarter-to-quarter UK-born wage changes, axis limited to ±0.2 | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/wages/` |
| `post2021_wage_change_distribution.png` | Full post-2021 distribution of UK-born wage changes | `03_descriptive_analysis.R` | Quarterly regression file and COVID file | No | `descriptive/quarterly/wages/` |
| `post2021_wage_change_distribution_axis_limited.png` | Same distribution with the axis limited to ±0.2 | `03_descriptive_analysis.R` | Quarterly regression file and COVID file | No | `descriptive/quarterly/wages/` |
| `d_qoq_mean_age_uk_born_by_residence.png` | Quarter-to-quarter UK-born age change | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/controls/` |
| `d_qoq_share_uk_degree_by_residence.png` | Quarter-to-quarter UK-born degree-share change | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/controls/` |
| `d_qoq_share_uk_low_qual_by_residence.png` | Quarter-to-quarter UK-born low-qualification-share change | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/controls/` |
| `d_qoq_share_uk_asian_by_residence.png` | Quarter-to-quarter UK-born Asian-share change | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/controls/` |
| `d_qoq_share_uk_black_by_residence.png` | Quarter-to-quarter UK-born black-share change | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/controls/` |
| `d_qoq_share_uk_chinese_by_residence.png` | Quarter-to-quarter UK-born Chinese-share change | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/controls/` |
| `d_qoq_share_uk_eth_other_by_residence.png` | Quarter-to-quarter UK-born mixed-and-other-share change | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/controls/` |
| `high_low_eu_exposure_trends.png` | Fixed 2017–2019 median groups: pooled EU ratio and equal-region mean log wage | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/diagnostics/` |
| `wage_sample_counts.png` | Unweighted UK-born wage respondent counts | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/diagnostics/` |
| `wage_change_vs_sample_size.png` | Absolute post-2021 wage change against the smaller adjacent respondent count | `03_descriptive_analysis.R` | Quarterly regression file and COVID file | No | `descriptive/quarterly/diagnostics/` |
| `coverage_status.png` | Valid, small, missing, and unavailable residence-quarters | `03_descriptive_analysis.R` | Quarterly regression file | No | `descriptive/quarterly/diagnostics/` |
| `ratio_levels_by_residence.png` | Annual migrant, EU, and non-EU ratio levels | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/migration/levels/` |
| `national_ratio_levels.png` | Annual national ratio levels | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/migration/levels/` |
| `d_annual_ratio_migrant_by_residence.png` | One-year migrant-ratio changes | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/migration/changes/` |
| `d_annual_ratio_eu_non_eu_by_residence.png` | One-year EU and non-EU ratio changes | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/migration/changes/` |
| `national_d_annual_ratios.png` | National one-year ratio changes | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/migration/changes/` |
| `post2021_ratio_change_distributions.png` | Post-2021 distributions of one-year ratio changes | `03_descriptive_analysis_annual.R` | Annual regression file and COVID file | No | `descriptive/annual/migration/changes/` |
| `d_annual_mean_log_hourpay_uk_born_by_residence.png` | One-year UK-born wage changes, axis limited to ±0.2 | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/wages/` |
| `post2021_wage_change_distribution.png` | Full post-2021 distribution of annual wage changes | `03_descriptive_analysis_annual.R` | Annual regression file and COVID file | No | `descriptive/annual/wages/` |
| `post2021_wage_change_distribution_axis_limited.png` | Same annual distribution with the axis limited to ±0.2 | `03_descriptive_analysis_annual.R` | Annual regression file and COVID file | No | `descriptive/annual/wages/` |
| `d_annual_mean_age_uk_born_by_residence.png` | One-year UK-born age change | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/controls/` |
| `d_annual_share_uk_degree_by_residence.png` | One-year UK-born degree-share change | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/controls/` |
| `d_annual_share_uk_low_qual_by_residence.png` | One-year UK-born low-qualification-share change | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/controls/` |
| `d_annual_share_uk_asian_by_residence.png` | One-year UK-born Asian-share change | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/controls/` |
| `d_annual_share_uk_black_by_residence.png` | One-year UK-born black-share change | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/controls/` |
| `d_annual_share_uk_chinese_by_residence.png` | One-year UK-born Chinese-share change | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/controls/` |
| `d_annual_share_uk_eth_other_by_residence.png` | One-year UK-born mixed-and-other-share change | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/controls/` |
| `wage_sample_counts.png` | Annual UK-born wage sample size, summed person-quarters | `03_descriptive_analysis_annual.R` | Annual regression file | No | `descriptive/annual/diagnostics/` |
| `wage_change_vs_sample_size.png` | Absolute annual wage change against the smaller adjacent person-quarter count | `03_descriptive_analysis_annual.R` | Annual regression file and COVID file | No | `descriptive/annual/diagnostics/` |
| `coverage_status.png` | Annual coverage, including years excluded from the annual file | `03_descriptive_analysis_annual.R` | Annual regression file and year-coverage file | No | `descriptive/annual/diagnostics/` |
| `ols_2sls_coefficients.png` | OLS and 2SLS across steps 0–6 | `06_results_analysis.R` | Quarterly coefficient file | Yes | `results/main/` |
| `step5_residualised_binscatter.png` | Binned step-5 OLS association after residualising | `06_results_analysis.R` | Quarterly regression file, COVID file, coefficient file | Yes | `results/main/` |
| `step5_partial_regression.png` | Residualised step-5 first stage | `06_results_analysis.R` | Quarterly regression file, COVID file, coefficient file | Yes | `results/first_stage/` |
| `quarterly_vs_annual_coefficients.png` | Steps 0–5, quarterly and annual, on separate scales | `06_results_analysis.R` | Quarterly and annual coefficient files | Yes | `results/robustness/` |
| `leave_one_region_out_step5.png` | Step 5 re-estimated without each residence, instruments held fixed | `06_results_analysis.R` | Quarterly regression file, COVID file, coefficient file | Yes | `results/robustness/` |

The seven quarterly control charts and the seven annual control charts are the original control-variable plots. The original wage charts still limit the displayed axis to ±0.2 and name the quarters or years outside that limit. Those observations remain in the data. The new wage-distribution figures are two files: the full range, and the same distribution with the axis limited to ±0.2.

Coefficient figures give each panel its own scale. In the leave-one-region-out figure, an interval wider than 30 times the median interval in that panel is named in the caption instead of drawn. `leave_one_region_out_step5.csv` keeps every interval.
