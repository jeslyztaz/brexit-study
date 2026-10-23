# main.R
# EC4362: Brexit and UK labour market outcomes
#
# Quarterly preparation, descriptive charts, and the main regression come
# first. The annual robustness check is pipeline/run_annual.R: it reads
# the raw quarterly files again, then compares the quarterly and annual
# estimates. Results figures are last. The descriptive scripts do not
# read regression coefficient files.

source("setup.R")

source("pipeline/01_prep_data.R")
source("pipeline/02_construct_variables.R")
source("pipeline/03_descriptive_analysis.R")
source("pipeline/04_main_regressions.R")

source("pipeline/run_annual.R")

source("pipeline/06_results_analysis.R")

message("EC4362 analysis completed successfully.")