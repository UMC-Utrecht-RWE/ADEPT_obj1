# Takes into account subpopulations 
# Runs individual p_steps for each subpopulation
# Result: If SUBP -> TRUE then each folder will contain (if present) results coming from all indicated subpops. Resulting files are prefixed with the name of the subpop

# Loads study population/populations 
populations <- list.files(file.path(paths$D3_dir, "study_population"))

# Loops over each subpopulation
for(pop in seq_along(populations)){
  
  # Loads study population
  study_population <- readRDS(file.path(paths$D3_dir, "study_population", populations[pop]))
  
  # Assign study population prefix name
  original_pop_prefix <- gsub("_study_population.rds", "", populations[pop])

  # Get unique sex groups (e.g., "F", "M")
  sex_groups <- unique(study_population$sex_at_instance_creation)
  sex_groups <- sex_groups[!is.na(sex_groups)]  
  
  study_population_all <- copy(study_population)
  
  # Loop through each sex group
  for (sex in seq_along(sex_groups)) {
    
    # Subset study population for current sex
    study_population_sex <- study_population_all[sex_at_instance_creation == sex_groups[sex]]
    
    # Create sex-specific prefix (e.g., "CPRD_F")
    sex_label <- ifelse(sex_groups[sex] == "F", "F", "M")
    pop_prefix <- paste0(original_pop_prefix, "_", sex_groups[sex])
    
    # Assign to environment so sourced scripts can access
    assign("study_population", study_population_sex, envir = .GlobalEnv)
    
    # Denominator Counts
    source(file.path(thisdir, "p_steps", "calculate_denominator_annual.R"), local = TRUE)
    
    # Create concept sets
    source(file.path(thisdir, "p_steps", "create_concept_sets.R"), local = TRUE)
    
    # Create ATC subsets
    source(file.path(thisdir, "p_steps", "create_subsets_ATC.R"), local = TRUE)
    
    # TODO
    # Create Dx subsets
    
    # Move algorithm inputs to folders
    source(file.path(thisdir, "p_steps", "move_algorithm_inputs_to_folders.R"), local = TRUE)
    
    rm(list = grep("algo|alt|ATC|bridge|cs_row|codelist|dt|file_info|matched|merged", ls(), value = TRUE))
    
    # Create Treatment Episodes
    source(file.path(thisdir, "p_steps", "create_treatment_episodes.R"), local = TRUE)
    
    # Create Treatment Episodes - groups
    source(file.path(thisdir, "p_steps", "create_treatment_episodes_groups.R"), local = TRUE)
    
    # Calculate incidence 
    source(file.path(thisdir, "p_steps", "calculate_incidence.R"), local = TRUE)
    
    # Calculate incidence - groups
    source(file.path(thisdir, "p_steps", "calculate_incidence_groups.R"), local = TRUE)
    
    # Calculate prevalence 
    source(file.path(thisdir, "p_steps", "calculate_prevalence.R"), local = TRUE)
    
    # Calculate prevalence - groups
    source(file.path(thisdir, "p_steps", "calculate_prevalence_groups.R"), local = TRUE)
    
    # Treatment Durations
    source(file.path(thisdir, "p_steps", "calculate_treatment_duration.R"), local = TRUE)
    
    # Calculate Discontinuers 
    source(file.path(thisdir, "p_steps", "calculate_discontinuation.R"), local = TRUE)
    
    # Calculate Discontinuers - groups
    source(file.path(thisdir, "p_steps", "calculate_discontinuation_groups.R"), local = TRUE)
    
    # Calculate alternative medications
    source(file.path(thisdir, "p_steps", "calculate_altmeds.R"), local = TRUE)
    
    # Calculate Switching
    source(file.path(thisdir, "p_steps", "calculate_switching.R"), local = TRUE)
    
    # Find Polytherapy
    source(file.path(thisdir, "p_steps", "calculate_polytherapy.R"), local = TRUE)
    
    # Baseline Tables 
    source(file.path(thisdir, "p_steps", "create_baseline_tables.R"), local = TRUE)
    
    # Clean up
    rm(list = grep("dt|overall|incidence|prev|discontinue|overlap|switcher|treat|stat|summary|altmed", ls(), value = TRUE))
  }
  
}

