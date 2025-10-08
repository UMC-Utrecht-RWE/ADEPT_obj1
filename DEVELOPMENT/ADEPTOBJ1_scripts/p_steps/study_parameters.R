###################################################
# Set Study Parameters
###################################################

# start_study_date is defined in to_run.R
start_study_date <- as.IDate(start_study_date)

# study_end_date
# Read CDM source file to get recommended end date, and assign to end_study_date
CDM_SOURCE <- fread(file.path(CDM_dir, list.files(CDM_dir, pattern = "^CDM_SOURCE")))
# Assign end_study_date
end_study_date <- as.IDate(as.character(CDM_SOURCE[, recommended_end_date]), "%Y%m%d")

# Age_min is the minimum age allowed - both males and females 
age_min <- 12 

# Age_max is the maximum age allowed - only for females 
age_max <- 54 

# look back period - default is 365
# ALL OTHER DEAPS
lookback_period <- years(1)
# EFEMERIS
if (deap_flags$is_EFEMERIS) lookback_period <- 3*30.4375-14  # 2.5 months
# FINLAND
if (deap_flags$is_FIN_REG)  lookback_period <- months(3)     # 3 months









