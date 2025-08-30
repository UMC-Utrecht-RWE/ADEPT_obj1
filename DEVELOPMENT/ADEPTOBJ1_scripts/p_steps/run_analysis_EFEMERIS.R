# Create folder structure and set paths
source(file.path(thisdir, "p_steps", "99_path.R"), local = TRUE)

# Load packages
source(file.path(thisdir, "p_steps", "packages.R"), local = TRUE)

# Detect available CDM tables, extract subpop info
source(file.path(thisdir, "p_steps", "info.R"), local = TRUE)

# Set study parameters
source(file.path(thisdir, "p_steps", "study_parameters.R"), local = TRUE)


# Create Population 
# Load pregnancies file
study_population <- as.data.table(get(load(file.path(preg_dir, "D3_pregnancy_final.RData"))))
# Remove true duplicates
study_population <- unique(study_population)

# Load all persons Tables - to get date of birth 
PERSONS <- as.data.table(rbindlist(lapply(list.files(CDM_dir, pattern = "^PERSONS", full.names = TRUE), fread), use.names = TRUE, fill = TRUE))

# creates columns birth dates and death dates if any
source(file.path(thisdir, "p_steps", "Step_02_PreparePersonsTable.R"), local = TRUE)

# Get birth date info 
setkey(PERSONS, person_id)
setkey(study_population, person_id)  # Replace with the correct column in pregnancies that identifies the mother

# Merge to get basic info into pregnancies file
study_population <- merge(study_population, 
                          PERSONS[, .(person_id, birth_date)], 
                          by = "person_id", all.x = TRUE)

# op_start_date = 2.5 months before the pregnancy_start_date 
# op_end_date = pregnancy end date 
study_population[,op_start_date := as.IDate(pregnancy_start_date) - lookback_period][,op_start_date:=as.IDate(op_start_date)]
study_population[,op_end_date   := as.IDate(pregnancy_end_date)]

# start_follow_up = pregnancy start date 
# end_follow_up = pregnancy end date 
study_population[,start_follow_up := as.IDate(pregnancy_start_date)]
study_population[,end_follow_up   := as.IDate(pregnancy_end_date)]

# Calculates age at op_start and op_end and date person turned 12, last date person was 54
# age min and max values are defined in study_parameters.R
study_population[, ':=' 
                 ( date_min = as.IDate(add_with_rollback(birth_date, period(age_min, units = "year"), roll_to_first = T, preserve_hms = TRUE)),
                   date_max = as.IDate(add_with_rollback(birth_date, period(age_max + 1, units = "year"), roll_to_first = TRUE, preserve_hms = TRUE)) - 1
                 )
]  

# Count number or rows/pregnancies  
n_before <- nrow(study_population)

# Keep only women who are younger than 55 at the start of pregnancy
study_population1 <- study_population[date_max >= start_follow_up, ]

# Count number or rows/pregnancies  
n_after <- nrow(study_population)

# Clean up population columns
study_population <- study_population[,.(pregnancy_id, person_id, pregnancy_start_date, pregnancy_end_date, sex_at_instance_creation, op_start_date, op_end_date, start_follow_up, end_follow_up, birth_date)]

# Save study population 
saveRDS(study_population, file = file.path(paths$D3_dir, "study_population", "ALL_study_population.rds"))

# Loads study population/populations 
populations <- list.files(file.path(paths$D3_dir, "study_population"))

# Loops over each subpopulation
for(pop in seq_along(populations)){
  
  # Loads study population
  study_population <- readRDS(file.path(paths$D3_dir, "study_population", populations[pop]))
  
  # Assign study population prefix name
  pop_prefix <- gsub("_study_population.rds", "", populations[pop])
  
  # Create concept sets
  source(file.path(thisdir, "p_steps", "create_concept_sets.R"), local = TRUE)
  
  # Create ATC subsets
  source(file.path(thisdir, "p_steps", "create_subsets_ATC.R"), local = TRUE)
  
  # Create dx subsets
  source(file.path(thisdir, "p_steps", "create_subsets_dx.R"), local = TRUE)
  
  # Move algorithm inputs to folders
  source(file.path(thisdir, "p_steps", "move_files_to_folders.R"), local = TRUE)
  
  # Comorbidity and Indication Counts
  source(file.path(thisdir, "p_steps", "calculate_indication_and_comorbidities.R"), local = TRUE)
  
  # Create Treatment Episodes
  source(file.path(thisdir, "p_steps", "create_treatment_episodes.R"), local = TRUE)
  
  # Preps altmeds for the switching script
  source(file.path(thisdir, "p_steps", "calculate_altmeds.R"), local = TRUE)
  
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

  # Calculate Discontinuers /discontinuation in pregnancy
  source(file.path(thisdir, "p_steps", "calculate_discontinuation.R"), local = TRUE)
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_discontinuation_rate.R"), local = TRUE)

  # Calculate Switching
  source(file.path(thisdir, "p_steps", "calculate_switching.R"), local = TRUE)
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_switching_rate.R"), local = TRUE)

  # Polytherapy
  source(file.path(thisdir, "p_steps", "calculate_polytherapy.R"), local = TRUE)
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_polytherapy_rate.R"), local = TRUE)
  #Polytherapy during pregnancy - stratification
  source(file.path(thisdir, "p_steps", "calculate_pregnancies_polytherapy_rate_stratification.R"), local = TRUE)

  # Baseline Tables
  source(file.path(thisdir, "p_steps", "create_baseline_tables.R"), local = TRUE)
  
}

