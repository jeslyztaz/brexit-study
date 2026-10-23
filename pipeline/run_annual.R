# Annual robustness pipeline. Run from the project root:
#   Rscript pipeline/run_annual.R
# The quarterly scripts are the main specification and are not called here.
# pipeline/05 reads the existing quarterly panel, regression file, and
# coefficient file. It does not rebuild them.

source("pipeline/01_build_annual_panel.R")
source("pipeline/02_construct_annual_variables.R")
source("pipeline/03_descriptive_analysis_annual.R")
source("pipeline/04_main_regressions_annual.R")
source("pipeline/05_compare_quarterly_annual.R")
