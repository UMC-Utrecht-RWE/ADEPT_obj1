# Clear all objects from current R environment to start with a clean workspace
rm(list = ls())

#TODO - Test 

# Check if 'rstudioapi' package is installed; if not, install it, then load package
if (!require(rstudioapi)) install.packages("rstudioapi")

# Get the folder path of the currently open R script in RStudio and set it as the working directory
thisdir <- dirname(rstudioapi::getSourceEditorContext()$path)

# ======================
#     DEAP Selections
# ======================

### <<< USER INPUT >>> ###
# Manually uncomment ONE line below to set your DEAP data source:

# DEAP_data <- "BIFAP"
# DEAP_data <- "CPRD"
# DEAP_data <- "EFEMERIS"
DEAP_data <- "FIN_REG"
# DEAP_data <- "NOR_REG"
# DEAP_data <- "PHARMO"
# DEAP_data <- "SIDIAP"
# DEAP_data <- "VAL_PAD"
# DEAP_data <- "VID"

### <<< USER INPUT >>> ###
#<<< ===========================================================================================================>>>
# Set start study date - this is the date from which data is considered of good quality
# Analysis counts will be done from start_study_date + lookback period.
# In the default example, if start_study_date is 1999-01-01, then medicine counts will be done from 2000-01-01
start_study_date <- "1999-01-01"

#<<< ===========================================================================================================>>>
# Set directory where CDM tables are
CDM_dir <- "Path/To/Your/CDM/Folders/here"

#<<< ===========================================================================================================>>>
# Set directory where D3_pregnancy_final.RData file is (created by Pregnancy Algorithm)
preg_dir <- "Path/To/Your/Pregnancy/D3_pregnancy_final.RData/File/Here"

#<<< ===========================================================================================================>>>

# Run to set DEAP flags
source(file.path(thisdir, "p_steps", "set_flags.R"), local = TRUE)

# # === ANALYSIS SCRIPTS ===
# EFEMERIS 
source(file.path(thisdir, "p_steps", "run_analysis_FIN_REG.R"), local = TRUE)

# mask and save as csv
source(file.path(thisdir, "p_steps", "mask_and_save_as_csv.R"), local = TRUE)

# # Objectives 1.1 and 1.2
# source(file.path(thisdir, "p_steps", "run_analysis.R"), local = TRUE)
# 
# # Objectives 1.3 and 1.4
# source(file.path(thisdir, "p_steps", "run_analysis_pregnancies.R"), local = TRUE)
# 
# # Objectives 1.5
# source(file.path(thisdir, "p_steps", "calculate_weighted_daily_dose.R"), local = TRUE)
