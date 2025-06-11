# Clear all objects from current R environment to start with a clean workspace
rm(list = ls())

# Check if 'rstudioapi' package is installed; if not, install it, then load package
if (!require(rstudioapi)) { install.packages("rstudioapi") }
library(rstudioapi)

# Get the folder path of the currently open R script in RStudio and set it as the working directory
path_project_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path_project_folder)

# Source the 99_path.R script to set up all project directory paths
source("99_path.R")

# Source script to load and install all required R packages for the project
source(paste0(path_p_steps, "/packages.R"))

# ======================
#     DEAP Selection
# ======================

### 🛑 <<< OPTION 1: MANUAL SELECTION >>> ###
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

# Source script to dynamically set logical flags for the selected DAP based on DEAP_data
source(paste0(path_p_steps, "/set_deap_flags.R"))


### 🛑 <<<  OPTION 2: MANUAL SELECTION >>> ###
# Manually uncomment ONE line below to set if multiple regions (BIFAP):
# If multiple_regions <- T, please set path to region directory

multiple_regions <- FALSE
# multiple_regions <- TRUE 
# multiple_regions_dir <- paste0(path_CDM_dir, "BIFAP/")


# ========================================================
#     SOURCE POPULATION CREATION + PRELIMINARY COUNTS
# ========================================================



# source(paste0(pre_dir,"intermediate/run_counts_prelim.R"))
# 



# ###########################################
# # clear g_intermediate 
# #set to TRUE to clear out intermediate files PLEASE REPLACE T WITH F IF YOU WANT TO SAVE INTERMEDIATE DATA SETS, I.E. TO REDUCE AMOUNT OF STORED DATA"
# clear_int_files<-F
# #user input parameter
# #set to TRUE to clear out intermediate files PLEASE REPLACE T WITH F IF YOU WANT TO SAVE INTERMEDIATE DATA SETS, I.E. TO REDUCE AMOUNT OF STORED DATA"
# if(clear_int_files==T){
#   unlink(paste0(g_intermediate, "/tmp"), recursive = TRUE)
#   unlink(paste0(g_intermediate, "/populations"), recursive = TRUE)
# }
# 
# 
# 
# 

