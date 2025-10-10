# Loads study population/populations 
populations <- list.files(file.path(paths$D3_dir, "study_population"))

# Loops over each subpopulation
for(pop in seq_along(populations)){
  
  # Loads study population
  study_population <- readRDS(file.path(paths$D3_dir, "study_population", populations[pop]))
  study_population[,person_id:=as.character(person_id)]
  
  # Assign study population prefix name
  pop_prefix <- gsub("_study_population.rds", "", populations[pop])
  
  # get female population only  
  study_population <- study_population[sex_at_instance_creation=="F",]
  
  # Pregnancy Attrition Table 
  source(file.path(thisdir, "p_steps", "create_pregnancy_attrition_table.R"), local = TRUE)
  
  # Pre-pregnancy ASM use
  source(file.path(thisdir, "p_steps", "calculate_pre_pregnancy_use.R"), local = TRUE)
  
  # Initiation Rates during pregnancy
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_initiation_rate.R"), local = TRUE)

  # Initiation Rates during pregnancy - stratification
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_initiation_rate_stratification.R"), local = TRUE)

  # Continued Use during pregnancy
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_continuous_use_rate.R"), local = TRUE)

  # Continued Use during pregnancy - stratification
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_continuous_use_rate_stratification.R"), local = TRUE)

  # Discontinuation during pregnancy
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_discontinuation_rate.R"), local = TRUE)

  # Switching during pregnancy
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_switching_rate.R"), local = TRUE)

  # Polytherapy during pregnancy
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_polytherapy_rate.R"), local = TRUE)
  
  #Polytherapy during pregnancy - stratification
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_polytherapy_rate_stratification.R"), local = TRUE)

}

