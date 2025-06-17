if(multiple_regions==T){
  # # Gets a list of region names from the CDMInstances folder 
  # regions<-list.dirs(path=multiple_regions_dir, full.names=FALSE, recursive=FALSE)
  # # Loops over each region
  # for(reg in 1:length(regions)){
  #   # Prints region loop is currently working on
  #   print("##################################################")
  #   print("##################################################")
  #   print(paste("############ RUNNING ANALYSIS FOR ", regions[reg], "############"))
  #   print("##################################################")
  #   print("##################################################")
  #   # Sets paths to data folder for each region
  #   path_dir<-paste0(multiple_regions_dir, regions[reg], "/")
  #   # Sources folders for each region 
  #   source(paste0(pre_dir,"info.R"))
  #   source(paste0(pre_dir,"parameters/study_parameters.R"))
  #   # Creates folder for each region 
  #   invisible(ifelse(!dir.exists(paste0(projectFolder, "/", regions[reg])), dir.create(paste0(projectFolder, "/", regions[reg])), FALSE))
  #   reg_dir<-paste0(projectFolder, "/", regions[reg], "/")
  #   # Creates main output folders 
  #   ## First removes g_intermediate/g_output
  #   if("g_intermediate" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_intermediate"), recursive = T)}
  #   if("g_output" %in% list.files(projectFolder)){unlink(paste0(projectFolder,"/g_output"), recursive = T)}
  #   # Creates main output folders + paths
  #   dir.create(paste0(projectFolder, "/g_intermediate"))
  #   g_intermediate<-paste0(projectFolder, "/g_intermediate/")
  #   dir.create(paste0(projectFolder, "/g_output"))
  #   output_dir<-paste0(projectFolder, "/g_output/")
  #   # Creates g_intermediate subfolders
  #   dir.create(paste0(g_intermediate, "/populations"))
  #   populations_dir<-paste0(g_intermediate,"populations/")
  #   dir.create(paste0(g_intermediate, "/tmp")) 
  #   tmp<-paste0(g_intermediate,"tmp/")
  #   dir.create(paste0(g_intermediate, "/counts_dfs")) 
  #   counts_dfs_dir <-paste0(g_intermediate,"counts_dfs/")
  #   # Creates g_output subfolders
  #   ## for Concept sets 
  #   dir.create(paste0(tmp, "conceptsets_dx"))
  #   conceptsets_DX_dir<-paste0(tmp, "conceptsets_dx/")
  #   dir.create(paste0(tmp, "conceptsets_atc"))
  #   conceptsets_ATC_dir<-paste0(tmp, "conceptsets_atc/")
  #   dir.create(paste0(tmp, "conceptsets_proc"))
  #   conceptsets_PROC_dir<-paste0(tmp, "conceptsets_proc/")
  #   ## for records from MEDICINES TABLES
  #   # Temporary folder - will be deleted 
  #   dir.create(paste0(tmp, "events_atc"))
  #   events_tmp_ATC<-paste0(tmp, "events_atc/")
  #   # Permanent folder
  #   dir.create(paste0(tmp, "medications"))
  #   medications_pop<-paste0(tmp, "medications/")
  #   # Monthly counts
  #   dir.create(paste0(output_dir, "monthly_counts_atc"))
  #   monthly_counts_atc<-paste0(output_dir, "monthly_counts_atc")
  #   ## for sterility records
  #   # Temporary folder - will be deleted 
  #   dir.create(paste0(tmp, "events_sterility"))
  #   events_tmp_sterility<-paste0(tmp, "events_sterility/")
  #   # Permanent folder
  #   dir.create(paste0(tmp, "sterility"))
  #   sterility_pop<-paste0(tmp, "sterility/")
  #   ## for plots
  #   dir.create(paste0(output_dir, "plots"))
  #   plot_folder<-paste0(output_dir, "plots")
  #   # Creates folders for final storage
  #   ## For all preliminary counts
  #   dir.create(paste0(output_dir, "preliminary_counts"))
  #   preliminary_counts_dir<-paste0(output_dir, "preliminary_counts")
  #   # Sources study_source_population_script.R
  #   source(paste0(pre_dir,"studypopulation/study_source_population_script.R"))
  #   # Sources run_counts_prelim_each_pop.R 
  #   source(paste0(pre_dir,"intermediate/run_counts_prelim_each_pop.R"))
  #   # Moves g_intermediate, g_output folders from LOT4_script folder to respective regional folders
  #   file.move(paste0(projectFolder,"/g_intermediate"), paste0(projectFolder, "/", regions[reg], "/g_intermediate"))
  #   file.move(paste0(projectFolder,"/g_output"), paste0(projectFolder, "/", regions[reg], "/g_output"))
  # }
  
} else {
  
  # Create folder structure and set paths
  source(file.path(thisdir, "scripts", "99_path.R"), local = TRUE)
  
  # Load packages
  source(file.path(thisdir, "scripts", "packages.R"), local = TRUE)
  
  # Detect available CDM tables, extract subpop info
  source(file.path(thisdir, "scripts", "info.R"), local = TRUE)
  
  # Set study parameters
  source(file.path(thisdir, "scripts", "study_parameters.R"), local = TRUE)
  
  # Create Study Population 
  source(file.path(thisdir, "scripts", "study_source_population_script.R"), local = TRUE)
  
  # clean up before moving on
  rm(list = grep("actual_tables|CDM_SOURCE|^flowchart|^Flow|inputed|^METADATA|^missing|^OBSERVATION|persons|^PT|^SCHEME|^Selection|^SOURCE|SPELLS|study_population|TEMP", ls(), value = TRUE))
  
  # runs main analysis 
  source(file.path(thisdir, "scripts", "run_analysis_each_pop.R"), local = TRUE)
}


