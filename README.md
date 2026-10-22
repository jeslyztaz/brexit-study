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