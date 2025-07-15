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
  pop_prefix <- gsub("_study_population.rds", "", populations[pop])
  
  # get female population only  
  study_population <- study_population[sex_at_instance_creation=="F",]
  
  # Pre-pregnancy ASM use - Individual
  
  source(file.path(thisdir, "p_steps", "calculate_pre_pregnancy_asm_use.R"), local = TRUE)
  
  # Pre-pregnancy ASM use - Groups 
  source(file.path(thisdir, "p_steps", "calculate_pre_pregnancy_asm_use_groups.R"), local = TRUE)
  
  # Initiation Rates during pregnancy
  source(file.path(thisdir, "p_steps", "calculate_initiation_rate_during_pregnancy.R"), local = TRUE)
  
  # Initiation Rates during pregnancy
  source(file.path(thisdir, "p_steps", "calculate_initiation_rate_during_pregnancy_groups.R"), local = TRUE)
  
  # Continued Use during pregnancy
  source(file.path(thisdir, "p_steps", "calculate_continuous_use_rate.R"), local = TRUE)
  
  
}

