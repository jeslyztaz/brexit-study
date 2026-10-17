# main.R
# EC4362: Brexit and UK Labour Market Outcomes

# ----------------------------------
# 0. Environment setup
# ----------------------------------
source("setup.R")

# ----------------------------------
# 1. Data preparation
# ----------------------------------
source("scripts/01_import_clean.R")
source("scripts/02_construct_variables.R")

# ----------------------------------
# 2. Exploratory analysis
# ----------------------------------
source("scripts/03_descriptive_analysis.R")

# ----------------------------------
# 3. Econometric analysis
# ----------------------------------
source("scripts/04_main_regressions.R")
source("scripts/05_robustness.R")

# ----------------------------------
# 4. Export results
# ----------------------------------
source("scripts/06_export_results.R")

message("EC4362 analysis completed successfully.")