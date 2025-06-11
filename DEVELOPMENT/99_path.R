################################################################################
# Script Name:     99_path.R
# Author:          Magdalena Gamba
# Email:           m.a.gamba@uu.nl
# Organization:    Utrecht University, Utrecht, The Netherlands
# Created:         YYYY-MM-DD
# Last Modified:   YYYY-MM-DD
# Description:     This script sets and creates all project directory paths needed.
#                  It should be sourced before any analysis or data processing scripts.
# Inputs:          [List key input files, databases, or objects]
# Outputs:         [List key output files, tables, or plots]
# Notes:           [Any important details or warnings]
################################################################################

# ============================
# Project Directory Setup 
# ============================

# Move up one directory level to reach project root
setwd('..') 

# Get absolute path of project root directory
path_base_dir <- getwd() 


# ================================================
# 🚨📂 SET PATH TO CDM INSTANCE FOLDER HERE! 📂🚨
# ================================================

# Set the name of the study folder inside CDMInstances
StudyName <- "ADEPT_obj1"

# Construct full path to the CDM instance folder
path_CDM_dir <- paste0(path_base_dir, "/CDMInstances/", StudyName, "/")


# ===============================
# 🔧 Create Base Output Folders 🔧
# ===============================

# Helper function: create directory if it doesn't exist
create_dir_if_missing <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

# Define paths to key output folders
path_g_intermediate <- file.path(path_project_folder, "g_intermediate")
path_g_output       <- file.path(path_project_folder, "g_output")
path_p_steps        <- file.path(path_project_folder, "p_steps")  # NOTE: Folder should exist; scripts live here

# Create output folders (p_steps should already exist)
create_dir_if_missing(path_g_intermediate)
create_dir_if_missing(path_g_output)

# Define paths to g_intermediate subfolders
path_populations        <- file.path(path_g_intermediate, "populations")
path_tmp                <- file.path(path_g_intermediate, "tmp")
path_subsets            <- file.path(path_g_intermediate, "subsets")
path_subsets_atc        <- file.path(path_subsets, "atc")
path_subsets_dx         <- file.path(path_subsets, "dx")
path_treatment_episodes <- file.path(path_g_intermediate, "treatment_episodes")

# Create subfolders within g_intermediate
create_dir_if_missing(path_populations)
create_dir_if_missing(path_tmp)
create_dir_if_missing(path_subsets)
create_dir_if_missing(path_subsets_atc)
create_dir_if_missing(path_subsets_dx)
create_dir_if_missing(path_treatment_episodes)

# Define paths to g_output subfolders







# # CONCEPT SET FOLDERS
# invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_dx")), dir.create(paste0(tmp, "conceptsets_dx")), FALSE))
# conceptsets_DX_dir<-paste0(tmp, "conceptsets_dx/")
# invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_atc")), dir.create(paste0(tmp, "conceptsets_atc")), FALSE))
# conceptsets_ATC_dir<-paste0(tmp, "conceptsets_atc/")
# invisible(ifelse(!dir.exists(paste0(tmp, "conceptsets_proc")), dir.create(paste0(tmp, "conceptsets_proc")), FALSE))
# conceptsets_PROC_dir<-paste0(tmp, "conceptsets_proc/")
# 
# # MEDICINES TABLES FOLDER
# # Temporary folder 
# invisible(ifelse(!dir.exists(paste0(tmp, "events_atc")), dir.create(paste0(tmp, "events_atc")), FALSE))
# events_tmp_ATC<-paste0(tmp, "events_atc/")
# # Permanent folder
# invisible(ifelse(!dir.exists(paste0(tmp, "medications")), dir.create(paste0(tmp, "medications")), FALSE))
# medications_pop<-paste0(tmp, "medications/")
# 
# # Monthly counts
# invisible(ifelse(!dir.exists(paste0(output_dir, "monthly_counts_atc")), dir.create(paste0(output_dir, "monthly_counts_atc")), FALSE))
# monthly_counts_atc<-paste0(output_dir, "monthly_counts_atc")
# 
# # STERILITY FOLDERS
# # Temporary folder
# invisible(ifelse(!dir.exists(paste0(tmp, "events_sterility")), dir.create(paste0(tmp, "events_sterility")), FALSE))
# events_tmp_sterility<-paste0(tmp, "events_sterility/")
# # Permanent folder
# invisible(ifelse(!dir.exists(paste0(tmp, "sterility")), dir.create(paste0(tmp, "sterility")), FALSE))
# sterility_pop<-paste0(tmp, "sterility/")
# 
# # PLOT FOLDER
# invisible(ifelse(!dir.exists(paste0(output_dir, "plots")), dir.create(paste0(output_dir, "plots")), FALSE))
# plot_folder<-paste0(output_dir, "plots")
# 
# # MAIN OUTPUT FOLDERS
# # 1. PRELIMINARY COUNTS 
# invisible(ifelse(!dir.exists(paste0(output_dir, "preliminary_counts")), dir.create(paste0(output_dir, "preliminary_counts")), FALSE))
# preliminary_counts_dir<-paste0(output_dir, "preliminary_counts")
# retinoid_counts_dfs <- file.path(counts_dfs_dir, "retinoid_counts")
# create_dir_if_missing(retinoid_counts_dfs)
# # 2. BASELINE TABLES 
# invisible(ifelse(!dir.exists(paste0(output_dir, "baseline_tables")), dir.create(paste0(output_dir, "baseline_tables")), FALSE))
# baseline_tables_dir<-paste0(output_dir, "baseline_tables")
# # 3. MEDICINES COUNTS 
# invisible(ifelse(!dir.exists(paste0(output_dir, "medicines_counts")), dir.create(paste0(output_dir, "medicines_counts")), FALSE))
# medicines_counts_dir<-paste0(output_dir, "medicines_counts")
# 
# # Move stratified records into stratified folders
# # Create stratified folder
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/","stratified")), dir.create(paste0(medicines_counts_dir,"/","stratified")), FALSE))
# medicines_stratified_dir<-paste0(medicines_counts_dir,"/","stratified")
# # Create stratified by age groups folder
# invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","age_group")), dir.create(paste0(medicines_stratified_dir,"/","age_group")), FALSE))
# medicines_stratified_age_groups<-paste0(medicines_stratified_dir ,"/","age_group")
# # Create stratified by indication folder 
# invisible(ifelse(!dir.exists(paste0(medicines_stratified_dir,"/","indication")), dir.create(paste0(medicines_stratified_dir,"/","indication")), FALSE))
# medicines_stratified_indication<-paste0(medicines_stratified_dir ,"/","indication")
# 
# # To save incidence and prevalence patient level records  
# invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_1")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_1")),FALSE))
# objective1_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_1/")
# # To save incidence and prevalence counts/rates
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_1")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_1")),FALSE))
# objective1_dir<-paste0(medicines_counts_dir,"/RAM_Objective_1")
# # To save incidence and prevalence stratified counts
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_1/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_1/Stratified")),FALSE))
# objective1_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_1/Stratified")
# 
# # To save switching and discontinued patient level records  
# invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_2")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_2")),FALSE))
# objective2_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_2/")
# # To save switching and discontinued counts/rates
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_2")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_2")),FALSE))
# objective2_dir<-paste0(medicines_counts_dir,"/RAM_Objective_2")
# # To save switching and discontinued stratified counts
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_2/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_2/Stratified")),FALSE))
# objective2_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_2/Stratified")
# 
# # To save concomitance patient level records  
# invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_3")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_3")),FALSE))
# objective3_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_3/")
# # To save concomitance patient level records  
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_3")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_3")),FALSE))
# objective3_dir<-paste0(medicines_counts_dir,"/RAM_Objective_3")
# # To save concomitance patient level records  
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_3/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_3/Stratified")),FALSE))
# objective3_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_3/Stratified")
# 
# # To save concomitance patient level records  
# invisible(ifelse(!dir.exists(paste0(counts_dfs_dir,"RAM_Objective_4")),dir.create(paste0(counts_dfs_dir,"RAM_Objective_4")),FALSE))
# objective4_temp_dir<-paste0(counts_dfs_dir,"RAM_Objective_4/")
# # To save concomitance patient level records  
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_4")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_4")),FALSE))
# objective4_dir<-paste0(medicines_counts_dir,"/RAM_Objective_4")
# # To save concomitance patient level records  
# invisible(ifelse(!dir.exists(paste0(medicines_counts_dir,"/RAM_Objective_4/Stratified")),dir.create(paste0(medicines_counts_dir,"/RAM_Objective_4/Stratified")),FALSE))
# objective4_strat_dir<-paste0(medicines_counts_dir,"/RAM_Objective_4/Stratified")