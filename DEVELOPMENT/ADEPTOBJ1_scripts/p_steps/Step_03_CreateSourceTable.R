#################################################################
# Create Source Table
################################################################

# If SUBP == TRUE, create a scheme table (SCHEME_03) that lists:
#   - The input file names for observation spells per subpopulation (e.g., "PC_OBS_SPELLS.rds")
#   - The output file names for the resulting source population files
#   - The output folder name
# This scheme table will be used in a loop to process each subpopulation separately.
if (SUBP) {
  SCHEME_03 <- copy(subpopulation_meanings)
  SCHEME_03[, ":=" (file_in = paste0(subpopulations, "_OBS_SPELLS.rds"), file_out = paste0(subpopulations, "_source_population.rds"))]
}

# If SUBP == FALSE create a simplified scheme with only one row corresponding to the entire population ("ALL"):
#   - file_in:   the input spells file for all persons
#   - file_out:  the output file where the combined source population will be saved
#   - folder_out: name of the output folder for temporary/intermediate use
if (!SUBP) {
  SCHEME_03 <- data.table(subpopulations = "ALL")
  SCHEME_03[, `:=`(file_in = paste0(subpopulations, "_OBS_SPELLS.rds"), file_out = paste0(subpopulations, "_source_population.rds"))]
}

# Load persons file
PERSONS <- readRDS(file.path(paths$D3_dir, "source_population", "persons.rds"))

# Loop over each row (subpopulation) in SCHEME_03
for (i in seq_len(nrow(SCHEME_03))){
  
  # Read observation spells data for the current subpopulation
  SPELLS <- readRDS(file.path(paths$D3_dir, "spells", SCHEME_03[["file_in"]][i]))
  
  # Check for duplicated person_id in SPELLS
  if (any(duplicated(SPELLS[["person_id"]]))) stop("Duplicates in person or observation_period table")
  
  # Print message
  print(paste0("Merge person table with observation_periods table ", SCHEME_03[["subpopulations"]][i]))
  
  # Set keys
  setkey(PERSONS, "person_id")
  setkey(SPELLS, "person_id")
  
  # Merge PERSONS and SPELLS by person_id, keeping only matching rows
  SOURCE_POPULATION <- merge(PERSONS, SPELLS, by = "person_id")
  
  # Print Message
  print(paste0("If op_start_date is before birth_date replace op_start_date with birth_date ", SCHEME_03[["subpopulations"]][i]))
  SOURCE_POPULATION[op_start_date < birth_date, op_start_date := birth_date]
  
  # Print message
  print(paste0("Calculate age at op_start_date and op_end_date and dates of which age_min and age_max are reached  ", SCHEME_03[["subpopulations"]][i]))
  
  # Calculate age at op_start_date and op_end_date and date_min and date_max
  SOURCE_POPULATION[, ":=" (age_op_start_date = floor(time_length(interval(birth_date, op_start_date), "year")),
                            age_op_end_date = floor(time_length(interval(birth_date, op_end_date), "year")),
                            date_min = as.IDate(add_with_rollback(birth_date, period(age_min, units = "year"), roll_to_first = TRUE, preserve_hms = TRUE)))]
  
  # Calculate age_max in women
  SOURCE_POPULATION[, date_max := fifelse(
    sex_at_instance_creation == "F",
    as.IDate(add_with_rollback(birth_date, period(age_max + 1, units = "year"), roll_to_first = TRUE, preserve_hms = TRUE)) - 1,
    as.IDate(NA)
  )]
  
  # Max one observation period per person_id
  ## ENTRY DATE is defined as the latest of the following
  ### 1. start study date (defined by DAP)
  ### 2. date_min (date person turns 12)
  ### 3. Op_start_date
  SOURCE_POPULATION[, entry_date := pmax(start_study_date, date_min, op_start_date, na.rm = TRUE)]
  
  # EXIT DATE is defined as the earliest of the following
  ### 1. end study date - this is equal to recommended end date as per CDM table
  ### 2. Date of instance creation as per CDM table
  ### 3. date_max (last date person was 54 - this is defined above only for females)
  ### 4. Op_end_date
  ### 5. Death date
  SOURCE_POPULATION[, exit_date := pmin(end_study_date, date_creation, date_max, op_end_date, death_date, na.rm = TRUE)]
  
  # Add a column indicating the current subpopulation
  SOURCE_POPULATION[, population := SCHEME_03[["subpopulations"]][i]]
  
  # Save file
  saveRDS(SOURCE_POPULATION, file = file.path(paths$D3_dir, "source_population", SCHEME_03[["file_out"]][i])
  )
}
