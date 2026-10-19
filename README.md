## Project Structure

```
stunning-guacamole/
├── data/                                 # shrunk and decoded data
|   ├── aps2019_shrunk_decoded.csv*
|   ├── aps2021_shrunk_decoded.csv*
|   ├── census2011.csv
|   ├── lfs_apr18jun19_shrunk_decoded.csv*
|   ├── lfs_jul19sep20_shrunk_decoded.csv*
|   └── lfs_oct20dec21_shrunk_decoded.csv*
├── raws/                                 # raw data files
|   ├── UKDA-8565-tab/*                   # LFS Longitudinal Dataset, April 2018 - June 2019
|   |   ├── mrdoc/
|   |   |   └──ukda_data_dictionaries     # data dictionary
|   |   └── tab/                          # dataset
|   ├── UKDA-8665-tab/*                   # APS Household Dataset, January - December, 2019
|   ├── UKDA-8779-tab/*                   # LFS Longitudinal Dataset, July 2019 - September 2020
|   ├── UKDA-8916-tab/*                   # LFS Dataset, October 2020 - December 2021
|   ├── UKDA-9021-tab/*                   # APS Household Dataset, January - December, 2021
|   └── census2011.xls                    # 2011 Census Dataset
├── aps_census_decoding.Rmd
├── .gitignore
├── lfs_decoding.Rmd
└── README.md

*File/Folder is stored locally as it contains safeguarded data
```