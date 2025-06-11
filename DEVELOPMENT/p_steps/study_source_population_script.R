#Author: Vjola Hoxhaj Drs./Roel Elbers MSc.
#email: v.hoxhaj@umcutrecht.nl/r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

#################################################################
#Study_population and Source_population
################################################################

# Create Output Folders 

#<<< case when NO subpopulations are present >>>
if (subpopulations_present == F) {
  
  #<<< case when study_source_population folder exists in g_output >>>
  if ("STUDY_SOURCE_POPULATION" %in% list.files(path_g_output)) {
    # delete folder 
    unlink(paste0(path_g_output, "/STUDY_SOURCE_POPULATION"), recursive = T)
    # create new folder
    dir.create(paste(path_g_output, "/STUDY_SOURCE_POPULATION", sep = ""))
    # set path to new folder 
    path_study_source_pop <- paste(path_g_output, "/STUDY_SOURCE_POPULATION", sep = "")
    # create masked subfolder in study_source_population folder 
    dir.create(paste(path_study_source_pop, "/Masked", sep = ""))
    
    #<<< case when study_source_population folder does not exists in g_output >>>
  } else {
    # create new folder 
    dir.create(paste(path_g_output, "/STUDY_SOURCE_POPULATION", sep = ""))
    # set path to new folder 
    path_study_source_pop <- paste(path_g_output, "/STUDY_SOURCE_POPULATION", sep = "")
    # create masked subfolder in study_source_population folder 
    dir.create(paste(path_study_source_pop, "/Masked", sep = ""))
  }
  
  #<<< case when study_source_population folder exists in g_intermediate/tmp >>>
  if ("STUDY_SOURCE_POPULATION" %in% list.files(path_tmp)) {
    # delete folder 
    unlink(paste0(path_tmp, "/STUDY_SOURCE_POPULATION"), recursive = T)
    # create new folder
    dir.create(paste(path_tmp, "/STUDY_SOURCE_POPULATION", sep = ""))
    # create masked subfolder in study_source_population folder 
    path_study_source_pop_tmp <- paste(path_tmp, "/STUDY_SOURCE_POPULATION", sep = "")
    
    #<<< case when study_source_population folder does not exist in g_intermediate/tmp >>>
  } else {
    # create new folder
    dir.create(paste(path_tmp, "/STUDY_SOURCE_POPULATION", sep = ""))
    # set path to new folder 
    path_study_source_pop_tmp <- paste(path_tmp, "/STUDY_SOURCE_POPULATION/", sep = "")
  }
  
  #<<< case when subpopulations are present >>>
} else {
  
  #<<< case when study_source_population folder exists in g_output >>>
  if ("STUDY_SOURCE_POPULATION" %in% list.files(path_g_output)) {
    # delete folder 
    unlink(paste0(path_g_output, "/STUDY_SOURCE_POPULATION"), recursive = T)
    # create new folder
    dir.create(paste(path_g_output, "/STUDY_SOURCE_POPULATION", sep = ""))
    # set path to new folder 
    path_study_source_pop <- paste(path_g_output, "/STUDY_SOURCE_POPULATION", sep = "")
    # Remove any files inside the new STUDY_SOURCE_POPULATION folder, just in case any remain
    do.call(file.remove, list(list.files(path_study_source_pop, full.names = T)))
    # Loop over all subpopulations and:
    for (i in 1:length(subpopulations_names)) {
      # Create a subfolder for each subpopulation inside the STUDY_SOURCE_POPULATION folder
      dir.create(paste0(path_study_source_pop, subpopulations_names[i]))
      # Inside each subpopulation folder, create a subfolder called "Masked"
      dir.create(paste0(path_study_source_pop, subpopulations_names[i], "/Masked"))
    }
    
    #<<< case when study_source_population folder does not exists in g_output >>>
  } else {
    # create new folder
    dir.create(paste(path_g_output, "/STUDY_SOURCE_POPULATION", sep = ""))
    # set path to new folder
    path_study_source_pop <- paste(path_g_output, "/STUDY_SOURCE_POPULATION", sep = "")
    # Loop over all subpopulations and:
    for (i in 1:length(subpopulations_names)) {
      # Create a subfolder for each subpopulation inside the STUDY_SOURCE_POPULATION folder
      dir.create(paste0(path_study_source_pop, subpopulations_names[i]))
      # Inside each subpopulation folder, create a subfolder called "Masked"
      dir.create(paste0(path_study_source_pop, subpopulations_names[i], "/Masked"))
    }
  }
  
  #<<< case when study_source_population folder exists in g_intermediate/tmp >>>
  if ("STUDY_SOURCE_POPULATION" %in% list.files(path_tmp)) {
    # delete folder 
    unlink(paste0(path_tmp, "/STUDY_SOURCE_POPULATION"), recursive = T)
    # create new folder
    dir.create(paste(path_tmp, "/STUDY_SOURCE_POPULATION", sep = ""))
    # set path to new folder
    path_study_source_pop_tmp <- paste(path_tmp, "/STUDY_SOURCE_POPULATION", sep = "")
    
    #<<< case when study_source_population folder does not exist in g_intermediate/tmp >>>
  } else {
    # create new folder
    dir.create(paste(path_tmp, "/STUDY_SOURCE_POPULATION", sep = ""))
    # set path to new folder
    path_study_source_pop_tmp <- paste(path_tmp, "/STUDY_SOURCE_POPULATION", sep = "")
  }
}

#######################################################
#path_study_source_pop output folder for study_source population
#path_study_source_pop_tmp output folder for temporary files
#############################################################

#Load functions
source(paste0(path_p_steps,"/functions/CreateSpells_v15.R"))
source(paste0(path_p_steps,"/functions/CountPersonTimeV13.6.R"))
source(paste0(path_p_steps,"/functions/FUNCTIONS.R"))

#Set parameters
source(paste0(path_p_steps,"/Step_00_SetParameters.R"))
 
#Preparation of analyses input tables
source(paste0(path_p_steps,"/Step_01_CreateSpells.R"))
source(paste0(path_p_steps,"/Step_02_PreparePersonsTable.R"))
source(paste0(path_p_steps,"/Step_03_CreateSourceTable.R"))
# source(paste0(pre_dir,"studypopulation/Step_04_CreateStudyPopulation.R"))
# 
# source(paste0(pre_dir,"studypopulation/Step_05_AddVariablesSourcePopulation.R"))
# source(paste0(pre_dir,"studypopulation/Step_06_AddVariablesStudyPopulation.R"))
# source(paste0(pre_dir,"studypopulation/Step_07_RunCountPersonTime.R"))

