## Project Structure

```
stunning-guacamole/
├── data/                                 # shrunk and decoded data
├── raws/                                 # raw data files
|   ├── UKDA-8565-tab/                    # LFS Longitudinal Dataset, April 2018 - June 2019
|   |   ├── mrdoc/
|   |   |   └──ukda_data_dictionaries     # data dictionary
|   |   └── tab/                          # dataset
|   ├── UKDA-8665-tab/                    # APS Household Dataset, January - December, 2019
|   ├── UKDA-8779-tab/                    # LFS Longitudinal Dataset, July 2019 - September 2020
|   ├── UKDA-8916-tab/                    # LFS Dataset, October 2020 - December 2021
|   ├── UKDA-9021-tab/                    # APS Household Dataset, January - December, 2021
|   └── census2011.xls                    # 2011 Census Dataset
├── aps_census_decoding.Rmd
├── lfs_decoding.Rmd
└── README.md
```