#################################################################
#Study_population and Source_population Main Script
################################################################

#Load functions
source(file.path(thisdir, "p_steps", "functions", "CreateSpells_v15.R" ), local = TRUE)
source(file.path(thisdir, "p_steps", "functions", "CountPersonTimeV13.6.R" ), local = TRUE)
source(file.path(thisdir, "p_steps", "functions", "functions.R" ), local = TRUE)

#Set parameters
source(file.path(thisdir, "p_steps", "Step_00_SetParameters.R"), local = TRUE)

#Preparation of analyses input tables
source(file.path(thisdir, "p_steps", "Step_01_CreateSpells.R"), local = TRUE)
source(file.path(thisdir, "p_steps", "Step_02_PreparePersonsTable.R"), local = TRUE)
source(file.path(thisdir, "p_steps", "Step_03_CreateSourceTable.R"), local = TRUE)
source(file.path(thisdir, "p_steps", "Step_04_CreateStudyPopulation.R"), local = TRUE)
source(file.path(thisdir, "p_steps", "Step_05_AddVariablesSourcePopulation.R"), local = TRUE)
source(file.path(thisdir, "p_steps", "Step_06_AddVariablesStudyPopulation.R"), local = TRUE)
source(file.path(thisdir, "p_steps", "Step_07_RunCountPersonTime.R"), local = TRUE)
