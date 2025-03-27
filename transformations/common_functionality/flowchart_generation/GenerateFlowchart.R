# Author: Albert Cid Royo MSc.
# email: a.cidroyo@umcutrecht.nl
# Organisation: UMC Utrecht, Utrecht, The Netherlands
# Date: 11/04/2022

#Create Flowchart

if (!require("data.table")) {
  install.packages("data.table")}
library(data.table)
library(readr)

flowChartFolder <- paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/')
setwd(flowChartFolder)
setwd("..")
metadata_dir <- paste0(getwd(),'/')
setwd("..")
projectFolder <- paste0(getwd(),'/')
functions_dir <- paste0(projectFolder,'p_steps/functions/')
#loading CreateFlowChart function
source(paste0(functions_dir,"CreateFlowChart_csv.R"))



#Load project program file
PROGRAM <- as.data.table(read_delim(paste0(meta_dir,"AZ_program.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE))
CSV_TEMPLATE_PATH <- paste0(flowChartFolder, "AZ_flowChart_csv_template.csv")
CreateFlowChart_csv(PROGRAM, CSV_TEMPLATE_PATH, OUTPUT_DIR = flowChartFolder, flowChartName = "AZ_FlowChart_ALL.csv")
