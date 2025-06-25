# Takes into account subpopulations 
# Runs individual scripts for each subpopulation
# Result: If SUBP -> TRUE then each folder will contain (if present) results coming from all indicated subpops. Resulting files are prefixed with the name of the subpop

# Loads study population/populations 
populations <- list.files(file.path(paths$D3_dir, "study_population"))

# Loops over each subpopulation
for(pop in 1:length(populations)){
  
  # Loads study population
  study_population <- readRDS(file.path(paths$D3_dir, "study_population", populations[pop]))
  
  # Assign study population prefix name
  pop_prefix<-gsub("_study_population.rds", "", populations[pop])
  
  # Denominator Counts
  source(file.path(thisdir, "scripts", "denominator_annual.R"), local = TRUE)
  
  # Create concept sets
  source(file.path(thisdir, "scripts", "create_concept_sets.R"), local = TRUE)

  # Create ATC subsets
  source(file.path(thisdir, "scripts", "create_subsets_ATC.R"), local = TRUE)
  
  # TODO
  # Create Dx subsets
  
  # Move algorithm inputs to folders 
  source(file.path(thisdir, "scripts", "move_algorithm_inputs_to_folders.R"), local = TRUE)
  
  # clean up before moving on
  # Data
  rm(list = grep("^algo|^ATC_|bridge|^codelist|combined_dt|cs_row|dp_algorithm|drug_dt|dt|file_info|FUyears_dt|merged_drug|merged_dx|not_algorithm|subset_dt", ls(), value = TRUE))
  # Values 
  rm(list = grep("^code|col|^component|copied_files|^current|end_year|exact|^f$|^file|flas|FUyears|^i$|^med|sf_name|start_year|subfolder_name|subset_files|success|^unmatched|^v$|varname", ls(), value = TRUE))
  
  # Create Treatment Episodes 
  source(file.path(thisdir, "scripts", "create_treatment_episodes_exp.R"), local = TRUE)
  
  # Calculate incidence 
  source(file.path(thisdir, "scripts", "calculate_incidence.R"), local = TRUE)
  
  # Calculate prevalence 
  source(file.path(thisdir, "scripts", "calculate_prevalence.R"), local = TRUE)
  
  # Calculate Discontinuers
  source(file.path(thisdir, "scripts", "calculate_discontinuation.R"), local = TRUE)
  
  # Create Treatment episodes for alternative meds
  source(file.path(thisdir, "scripts", "create_treatment_episodes_alt.R"), local = TRUE)
  
  # # Flowchart
  # source(paste0(pre_dir,"flowchart_base_population.R"))
  # Flowchart
  # source(paste0(pre_dir,"plots/plots_mask.R"))
}

# # Moves all counts, plots, formatted files to preliminary_counts folder
# pattern1 = c("monthly_counts", "plots", paste0(my_format,"_files"))
# # files_to_move <- list.files(path=output_dir, pattern=paste0(pattern1, collapse="|"))
# for(file in list.files(path=output_dir, pattern=paste0(pattern1, collapse="|"), ignore.case = T)){file.move(paste0(output_dir,file), paste0(paste0(preliminary_counts_dir, "/") ,file))}
# # Deletes temp files
# for(file in list.files(path = tmp, pattern ="events_")){unlink(paste0(tmp, file), recursive = TRUE)}
# # Delete Flowchart files 
# for(file in list.files(path = output_dir, pattern ="FlowChart")){unlink(paste0(output_dir, file), recursive = TRUE)}
# # Delete Study_population_folder 
# for(file in list.files(path = output_dir, pattern ="STUDY_SOURCE_POPULATION")){unlink(paste0(output_dir, file), recursive = TRUE)}