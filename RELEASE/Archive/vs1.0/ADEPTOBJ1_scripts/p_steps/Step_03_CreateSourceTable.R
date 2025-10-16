#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

# If subpopulation flag is TRUE, create SCHEME_03 data.table with file names based on subpopulations
if(SUBP) {
  
  SCHEME_03 <- copy(subpopulation_meanings)
  SCHEME_03 <- SCHEME_03[, ':=' (file_in = paste0(subpopulations,"_OBS_SPELLS.rds"), 
                                 file_out = paste0(subpopulations,"_source_population.rds"), 
                                 folder_out = "tmp2") ]
  # SCHEME_03 <- rbind(data.frame(subpopulations = c("ALL"),meaning_sets = "ALL",file_in = "ALL_OBS_SPELLS.rds", file_out = "ALL_source_population.rds", folder_out = "tmp2"),SCHEME_03)
}

# If subpopulation flag is FALSE, create SCHEME_03 with a single entry for "ALL"
if(!SUBP) SCHEME_03 <- data.frame(subpopulations = c("ALL"),
                                  file_in = "ALL_OBS_SPELLS.rds", 
                                  file_out = "ALL_source_population.rds", 
                                  folder_out = "tmp2")

# SCHEME_03$nrows <- as.integer(NA)
# SCHEME_03$ncols <- as.integer(NA)
# SCHEME_03$ncolsneeded <- 19

# Read in Persons File
persons <- readRDS(file.path(paths$D3_dir, "source_population", "persons.rds"))

# Loop over each row (subpopulation) in SCHEME_03
for(i in 1:nrow(SCHEME_03)){
  
  # Read observation spells data for the current subpopulation
  SPELLS <- readRDS(file.path(paths$D3_dir, "spells", SCHEME_03[["file_in"]][i]))

  # Check for duplicated person_id in SPELLS; stop if found
  if(any(duplicated(SPELLS[["person_id"]]))) stop("Duplicates in person or observation_period table") 
  
  # Print message
  print(paste0("Merge person table with observation_periods table ",SCHEME_03[["subpopulations"]][i]))
  
  # Set keys 
  setkey(persons, "person_id")
  setkey(SPELLS, "person_id")
  
  # Merge PERSONS and SPELLS by person_id, keeping only matching rows
  SOURCE_POPULATION <- merge(persons, SPELLS, by = "person_id")
  
  # Print Message
  print(paste0("If op_start_date is before birth_date replace op_start_date with birth_date ",SCHEME_03[["subpopulations"]][i]))
  
  # Print message
  print(paste0("Calculate age at op_start_date and op_end_date and dates of which Age_min and Age_max are reached  ",SCHEME_03[["subpopulations"]][i]))
  
  # Calculate age at op_start_date and op_end_date and dates for min and max age limits
  SOURCE_POPULATION <- SOURCE_POPULATION[, ':=' 
                                         ( age_op_start_date = floor(time_length(interval(birth_date, op_start_date),"year")),
                                           age_op_end_date   = floor(time_length(interval(birth_date, op_end_date),"year")),
                                           date_min          = as.IDate(add_with_rollback(birth_date, period(age_min,units = "year"), roll_to_first = T, preserve_hms = T))
                                         )
  ]  
  
  # Set date_max conditionally based on sex
  SOURCE_POPULATION[sex_at_instance_creation == "F", date_max := as.IDate(add_with_rollback(birth_date, period(age_max + 1, units = "year"), roll_to_first = TRUE, preserve_hms = TRUE)) - 1]
  SOURCE_POPULATION[sex_at_instance_creation != "F", date_max := as.IDate(NA)] 
  
  # Adjust op_start date to latest of these values: 
  ## date person turned min age: 12 (date_min)
  ## date person entered data source: obs_start_date
  ## start of study period: 20000101
  SOURCE_POPULATION <- SOURCE_POPULATION[, entry_date:= pmax(date_min, op_start_date, start_study_date, na.rm = TRUE)]
  
  # Adjust op_end date to latest of these values: 
  ## date person turned max age - 56 (date_max) - females only
  ## moving out of data source - op_end_date
  ## death
  ## last data available from data source: recommended_end_date
  ## last data extraction from data source: date_creation
  ## end of study period - current date

 # Adjust date_max to death_date if person has died before date_max
  SOURCE_POPULATION <- SOURCE_POPULATION[, exit_date:= pmin(date_max, op_end_date, death_date, recommended_end_date, date_creation, end_study_date, na.rm = TRUE)]
 
  
  # Add a column indicating the current subpopulation
  SOURCE_POPULATION <- SOURCE_POPULATION[, Population := SCHEME_03[["subpopulations"]][i]]
  
  # Save file
  saveRDS(SOURCE_POPULATION, file = file.path(paths$D3_dir, "source_population", SCHEME_03[["file_out"]][i])
  )
  
}

# save file 
saveRDS(SCHEME_03, file = file.path(paths$D3_dir, "source_population", "scheme_03.rds"))