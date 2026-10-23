## Project Structure

```
stunning-guacamole/
├── data/                                 # shrunk and decoded data
|   ├── processed
|   |   ├── residence_quarter_panel.csv   # generated panel dataset
|   |   └── residence_quarter_regression.csv
|   └── quarterly_lfs_raw/*
├── docs/
|   └── residence_quarter_regression.md
├── functions/
|   ├── construct_variables.R
|   ├── data_cleaners.R
|   └── lfs_panel.R
├── notebooks/
├── pipeline/
├── renv/
├── .gitignore
├── main.R
├── README.md
└── setup.R

*File/Folder is stored locally as it contains safeguarded data
```

## Annual robustness check

The quarterly pipeline is the main specification. The annual pipeline is a robustness check: it aggregates the same quarterly person files to one row per residence and calendar year, then repeats the shift-share regressions. Run it from the project root, after the quarterly regression file already exists:

```
Rscript pipeline/run_annual.R
```

The separate steps are `pipeline/01_build_annual_panel.R`, `pipeline/02_construct_annual_variables.R`, `pipeline/03_descriptive_analysis_annual.R`, `pipeline/04_main_regressions_annual.R`, and `pipeline/05_compare_quarterly_annual.R`. `tests/test_annual_panel.R` checks the annual aggregation and instruments on synthetic data. Person-level rules stay in `docs/residence_quarter_regression.md`. The annual aggregation, lags, and regression mapping are in `docs/residence_annual_regression.md`.

Annual outputs are `data/processed/residence_annual_panel.csv`, `data/processed/residence_annual_regression.csv`, and `outputs/annual/`. Those paths are separate from the quarterly panel, regression file, figures, and tables.