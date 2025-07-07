if(multiple_regions==T){
  # Gets a list of region names from the CDMInstances folder
  regions<-list.dirs(path=multiple_regions_dir, full.names=FALSE, recursive=FALSE)

  # Loops over each region
  for(reg in 1:length(regions)){
    # Prints region loop is currently working on
    print("##################################################")
    print("##################################################")
    print(paste("############ RUNNING ANALYSIS FOR ", regions[reg], "############"))
    print("##################################################")
    print("##################################################")
    
    region_dir <- file.path(thisdir, regions[reg])
    
    if (!dir.exists(region_dir)) dir.create(region_dir, recursive = TRUE)
    
    message("Using regional folder: ", region_dir)
    
    # Assign root_dir to region_dir before sourcing
    root_dir <- region_dir
    
    # Before sourcing info.R, set CDM_dir for the current region:
    CDM_dir <-  file.path(multiple_regions_dir, regions[reg])
    
    # Source the script; inside 99_path.R, root_dir will be taken from your environment
    source(file.path(thisdir, "p_steps", "99_path.R"), local = TRUE)
    
    # Load packages
    source(file.path(thisdir, "p_steps", "packages.R"), local = TRUE)
    
    # Detect available CDM tables, extract subpop info
    source(file.path(thisdir, "p_steps", "info.R"), local = TRUE)
    
    # Set study parameters
    source(file.path(thisdir, "p_steps", "study_parameters.R"), local = TRUE)
    
    # Create Study Population 
    source(file.path(thisdir, "p_steps", "study_source_population_script.R"), local = TRUE)
    
    # clean up before moving on
    # Data 
    rm(list = grep("actual_tables|CDM_SOURCE|^flowchart|^Flow|inputed|^METADATA|^missing|^OBSERVATION|persons|^PT|^SCHEME|^Selection|^SOURCE|SPELLS|study_population|TEMP", ls(), value = TRUE))
    # Values 
    rm(list = grep("^after|Analyse_dates|attrition|^before|CreateSpellsStep|crit_name|deap_names|default_start|earliest_in_data|end_study_date2|^f$|^i$|^intv|^j$|OBS_number|^p$|required_packages|select_most_recent|start_study_date2|^step|subfolders|T0|T1|T2|vars|^Y$|^years|YEN2|YST2", ls(), value = TRUE))
    
    # runs main analysis 
    source(file.path(thisdir, "p_steps", "run_analysis_each_pop.R"), local = TRUE)
    
   }
  
} else {
  
  # Create folder structure and set paths
  source(file.path(thisdir, "p_steps", "99_path.R"), local = TRUE)
  
  # Load packages
  source(file.path(thisdir, "p_steps", "packages.R"), local = TRUE)
  
  # Detect available CDM tables, extract subpop info
  source(file.path(thisdir, "p_steps", "info.R"), local = TRUE)
  
  # Set study parameters
  source(file.path(thisdir, "p_steps", "study_parameters.R"), local = TRUE)
  
  # Create Study Population 
  source(file.path(thisdir, "p_steps", "study_source_population_script.R"), local = TRUE)
  
  # clean up before moving on
  rm(list = grep("actual|SOURCE|FlowChart|inputed|METADATA|missing|OBSERVATION|persons|PT|SCHEME|Selection|SPELLS|TEMP|after|before|step", ls(), value = TRUE))
  
  # runs main analysis 
  source(file.path(thisdir, "p_steps", "run_analysis_each_pop.R"), local = TRUE)
}


