# Clear all objects from current R environment to start with a clean workspace
rm(list = ls())

# Check if 'rstudioapi' package is installed; if not, install it, then load package
if (!require(rstudioapi)) { install.packages("rstudioapi") }

# Get the folder path of the currently open R script in RStudio and set it as the working directory
thisdir <- dirname(rstudioapi::getSourceEditorContext()$path)

# ======================
#     DEAP Selections
# ======================

### <<< USER INPUT >>> ###
# Manually uncomment ONE line below to set your DEAP data source:

# DEAP_data <- "BIFAP"
DEAP_data <- "CPRD"
# DEAP_data <- "EFEMERIS"
# DEAP_data <- "FIN_REG"
# DEAP_data <- "NOR_REG"
# DEAP_data <- "PHARMO"
# DEAP_data <- "SIDIAP"
# DEAP_data <- "VAL_PAD"
# DEAP_data <- "VID"

### <<< USER INPUT >>> ###
# If Multiple Regions (BIFAP) set to TRUE and add path to folder with multiple regions

# multiple_regions     <- TRUE
# multiple_regions_dir <- "SET PATH TO MULTIPLE REGIONS FOLDER HERE!"

# ELSE 

# Leave Multiple Regions as FALSE and set path to folder with CDM instances
multiple_regions <- FALSE
CDM_dir <- "C:/Users/mgamb/Documents/GitHub/ADEPT_obj1/DEVELOPMENT/CDM_instances"


# === SET FLAGS === 

# Set DEAP flags
source(file.path(thisdir, "scripts", "set_flags.R"), local = TRUE)


# === ANALYSIS SCRIPTS === 
source(file.path(thisdir, "scripts", "run_analysis.R"), local = TRUE)



