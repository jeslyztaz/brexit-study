## Project structure

```
stunning-guacamole/
├── data/
│   ├── processed/
│   │   ├── residence_quarter_panel_full.csv   # written by pipeline/01_prep_data.R
│   │   ├── residence_quarter_regression.csv   # written by pipeline/02_construct_variables.R
│   │   ├── residence_annual_panel.csv
│   │   ├── residence_annual_regression.csv
│   │   ├── ons_covid_by_area_20.csv
│   │   ├── residence_quarter_panel.csv        # older extract, 2010 Q1–2026 Q2
│   │   └── residence_quarter_panel_v0.csv     # older extract, 2014 Q1–2026 Q2
│   └── quarterly_lfs_raw/*                    # local safeguarded person files
├── docs/
│   ├── residence_quarter_regression.md
│   └── residence_annual_regression.md
├── functions/
│   ├── construct_variables.R
│   ├── construct_variables_annual.R
│   ├── data_cleaners.R
│   ├── lfs_panel.R
│   └── lfs_panel_annual.R
├── notebooks/
├── pipeline/
├── tests/
├── renv/
├── main.R
├── README.md
└── setup.R
```

`pipeline/02_construct_variables.R` reads `residence_quarter_panel_full.csv`. The two older extracts are not that input. `residence_quarter_panel_full.csv` and `residence_annual_panel.csv` are gitignored. The regression files and `ons_covid_by_area_20.csv` are tracked. The quarterly regression tables have 440 residence-quarters, 2021 Q1–2026 Q2. The annual regression tables have 100 residence-years, 2021–2025. The specifications are in the two design notes.

## Annual robustness check

The quarterly pipeline is the main specification. The annual pipeline is a robustness check: it aggregates the same quarterly person files to one row per residence and calendar year, then repeats the shift-share regressions. Run it from the project root, after the quarterly regression file already exists:

```
Rscript pipeline/run_annual.R
```

The separate steps are `pipeline/01_build_annual_panel.R`, `pipeline/02_construct_annual_variables.R`, `pipeline/03_descriptive_analysis_annual.R`, `pipeline/04_main_regressions_annual.R`, and `pipeline/05_compare_quarterly_annual.R`. `tests/test_annual_panel.R` checks the annual aggregation and instruments on synthetic data. Person-level rules stay in `docs/residence_quarter_regression.md`. The annual aggregation, lags, and regression mapping are in `docs/residence_annual_regression.md`.

Annual data files are `data/processed/residence_annual_panel.csv` and `data/processed/residence_annual_regression.csv`. Charts, regression tables, and the quarterly-versus-annual comparison are under `outputs/`, in the folders described in `outputs/README.md`. `main.R` runs the quarterly pipeline, then the annual pipeline, then the results figures. `pipeline/run_annual.R` still runs the annual steps on their own after the quarterly regression file exists.