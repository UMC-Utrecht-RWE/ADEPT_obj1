#################################################################
#Study_population and Source_population Main Script
################################################################

#Load functions
source(file.path(thisdir, "scripts", "functions", "CreateSpells_v15.R" ), local = TRUE)
source(file.path(thisdir, "scripts", "functions", "CountPersonTimeV13.6.R" ), local = TRUE)
source(file.path(thisdir, "scripts", "functions", "functions.R" ), local = TRUE)

#Set parameters
source(file.path(thisdir, "scripts", "Step_00_SetParameters.R"), local = TRUE)

#Preparation of analyses input tables
source(file.path(thisdir, "scripts", "Step_01_CreateSpells.R"), local = TRUE)
source(file.path(thisdir, "scripts", "Step_02_PreparePersonsTable.R"), local = TRUE)
source(file.path(thisdir, "scripts", "Step_03_CreateSourceTable.R"), local = TRUE)
source(file.path(thisdir, "scripts", "Step_04_CreateStudyPopulation.R"), local = TRUE)
source(file.path(thisdir, "scripts", "Step_05_AddVariablesSourcePopulation.R"), local = TRUE)
source(file.path(thisdir, "scripts", "Step_06_AddVariablesStudyPopulation.R"), local = TRUE)
source(file.path(thisdir, "scripts", "Step_07_RunCountPersonTime.R"), local = TRUE)
