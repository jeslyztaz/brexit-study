# main.R
# EC4362: Brexit and UK Labour Market Outcomes

# ----------------------------------
# 0. Environment setup
# ----------------------------------
source("setup.R")

# ----------------------------------
# 1. Data preparation
# ----------------------------------
source("pipeline/01_prep_data.R")
source("pipeline/02_construct_variables.R")

# ----------------------------------
# 2. Exploratory analysis
# ----------------------------------
source("pipeline/03_descriptive_analysis.R")

# ----------------------------------
# 3. Econometric analysis
# ----------------------------------
source("pipeline/04_main_regressions.R")
source("pipeline/05_robustness.R")

# ----------------------------------
# 4. Export results
# ----------------------------------
source("pipeline/06_export_results.R")

message("EC4362 analysis completed successfully.")