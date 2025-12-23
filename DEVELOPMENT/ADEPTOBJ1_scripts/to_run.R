# Clear all objects from current R environment
rm(list = ls())

# Install rstudioapi if not installed already, load package
if (!require(rstudioapi)) install.packages("rstudioapi")

# Get the folder path of current script
thisdir <- dirname(rstudioapi::getSourceEditorContext()$path)

# ======================
#     DEAP SELECTIONS
# ======================

### <<< USER INPUT >>> ###
#<<< ===========================================================================================================>>>
# Select DEAP data source by uncommenting **only one** of the lines below.

DEAP_data <- "BIFAP"
#DEAP_data <- "CPRD"
#DEAP_data <- "EFEMERIS"
#DEAP_data <- "FIN_REG"
#DEAP_data <- "NOR_REG"
#DEAP_data <- "PHARMO"
#DEAP_data <- "SIDIAP"
#DEAP_data <- "VAL_PAD"
#DEAP_data <- "VID"

### <<< USER INPUT >>> ###
#<<< ===========================================================================================================>>>
# Set start date of your study period.  
# This is the date from which data is considered reliable and of good quality.  
# Analysis counts start from start_study_date + lookback period.  
# Example: If start_study_date = "1999-01-01", counts will be computed from "2000-01-01".

start_study_date <- "YYYY-MM-DD"

### <<< USER INPUT >>> ###
#<<< ===========================================================================================================>>>
# Set directory paths

# Path to folder with CDM tables 
CDM_dir <- "Path/To/Your/CDM/Folder/here" 

# Path to Pregnancies (D3_pregnancy_final.RData)
preg_dir <- "Path/To/Your/Pregnancy/D3_pregnancy_final.RData/File/Here"


# ======================
#     SCRIPTS 
# ======================

# Start logging
source(file.path(thisdir, "p_steps", "logging.R"), local = TRUE)
start_logging(file.path(thisdir, "log_file.txt"))

# Set flags
source(file.path(thisdir, "p_steps", "set_flags.R"), local = TRUE)

# BIFAP, CPRD, NOR_REG, PHARMO, SIDIAP, VAL_PAD, VID
if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG){
  source(file.path(thisdir, "p_steps", "run_analysis.R"), local = TRUE)                    #analysis obj 1.1, 1.2
  source(file.path(thisdir, "p_steps", "run_analysis_pregnancies.R"), local = TRUE)        #analysis obj 1.3, 1.4
  # Excluding VID
  if(!deap_flags$is_VID){
    source(file.path(thisdir, "p_steps", "calculate_weighted_daily_dose.R"), local = TRUE) #analysis obj 1.5
  }
  stop_logging() #stop logging
}

# EFEMERIS only
if(deap_flags$is_EFEMERIS){
  source(file.path(thisdir, "p_steps", "run_analysis_EFEMERIS.R"), local = TRUE)           #analysis obj 1.3, 1.4, 1.5
  stop_logging() #stop logging
}

# FINLAND only
if(deap_flags$is_FIN_REG){
  source(file.path(thisdir, "p_steps", "run_analysis_FIN_REG.R"), local = TRUE)            #analysis obj 1.3, 1.4, 1.5
  source(file.path(thisdir, "p_steps", "mask_counts_keep_rates_save_csv.R"), local = TRUE) #masking 
  stop_logging() #stop logging
}

