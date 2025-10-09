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
rm(list = grep("actual_tables|CDM_SOURCE|^flow_chart|inputed|METADATA|OBSERVATION|PERSONS|SCHEME|Selection|SOURCE|SPELLS", ls(), value = TRUE, ignore.case = TRUE))

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
    source(file.path(thisdir, "p_steps", "calculate_denominator.R"), local = TRUE)
    
    # Create concept sets
    source(file.path(thisdir, "p_steps", "create_concept_sets.R"), local = TRUE)
    
    # Create ATC subsets
    source(file.path(thisdir, "p_steps", "create_subsets_ATC.R"), local = TRUE)
    
    # Create indication subsets
    source(file.path(thisdir, "p_steps", "create_subsets_dx.R"), local = TRUE)
    
    # Move algorithm inputs to folders
    source(file.path(thisdir, "p_steps", "move_files_to_folders.R"), local = TRUE)
    
    # Comorbidity and Indication Counts
    source(file.path(thisdir, "p_steps", "calculate_indication_and_comorbidities.R"), local = TRUE)
    
    # Create Treatment Episodes
    source(file.path(thisdir, "p_steps", "create_treatment_episodes.R"), local = TRUE)
    
    # Calculate incidence
    source(file.path(thisdir, "p_steps", "calculate_incidence.R"), local = TRUE)
    
    # Calculate incidence - stratification
    source(file.path(thisdir, "p_steps", "calculate_incidence_stratification.R"), local = TRUE)
    
    # Calculate prevalence
    source(file.path(thisdir, "p_steps", "calculate_prevalence.R"), local = TRUE)
    
    # Calculate prevalence - stratification
    source(file.path(thisdir, "p_steps", "calculate_prevalence_stratification.R"), local = TRUE)
    
    # Treatment Durations
    source(file.path(thisdir, "p_steps", "calculate_treatment_duration.R"), local = TRUE)
    
    # Calculate Discontinuers
    source(file.path(thisdir, "p_steps", "calculate_discontinuation.R"), local = TRUE)
    
    # Calculate Discontinuers - stratification
    source(file.path(thisdir, "p_steps", "calculate_discontinuation_stratification.R"), local = TRUE)
    
    # Calculate alternative medications
    source(file.path(thisdir, "p_steps", "calculate_altmeds.R"), local = TRUE)
    
    # Calculate Switching
    source(file.path(thisdir, "p_steps", "calculate_switching.R"), local = TRUE)
    
    # Find Polytherapy
    source(file.path(thisdir, "p_steps", "calculate_polytherapy.R"), local = TRUE)
    
    # Find Polytherapy - stratification
    source(file.path(thisdir, "p_steps", "calculate_polytherapy_indications.R"), local = TRUE)
    
    # Baseline Tables
    source(file.path(thisdir, "p_steps", "create_baseline_tables.R"), local = TRUE)
    
  }
}

# clean up before moving on
rm(list = grep("agegroup|^age_at|^age_group|age_levels|algo|all_|alt|anti|ATC|attrition|baseline|benzo|bridge|check_counts|code|combined|common|comorbidity|concept|current|denom_counts|denominator|discont|dt|file|final|flow|fu_|gaba|incidence|indication|med|merge|overlap|prev|row|step|study_pop|switcher|treat", ls(), value = TRUE, ignore.case = TRUE))

