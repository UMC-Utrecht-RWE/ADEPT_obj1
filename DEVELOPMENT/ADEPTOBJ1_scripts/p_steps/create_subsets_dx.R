#################################################################
# Create Diagnosis Subsets
################################################################
print("=======================================================================")
print("========================= CREATING DX SUBSETS =========================")
print("=======================================================================")

# Load all concept sets and bind them. Remove all true duplicates
dx_concept_sets <- unique(
  rbindlist(
    lapply(
      list.files(path = file.path(paths$D3_dir, "concept_sets"),
                 pattern = "dx", ignore.case = TRUE, full.names = TRUE),
      function(file) fread(file, colClasses = list(character = "code"))  # force code to character
    ),
    use.names = TRUE, fill = TRUE
  )
)

# Create subsets of dx_concept_sets
dx_concept_set_nodot      <- dx_concept_sets[coding_system %in% c("ICD10", "ICD10CM", "ICD10DA", "ICD9CM", "ICD9CMP", "ICPC", "ICPC2P", "MTHICD9", "ICPC2EENG")]
dx_concept_set_startswith <- dx_concept_sets[coding_system %in% c("RCD", "RCD2")]
dx_concept_set_exact      <- dx_concept_sets[coding_system %in% c("MEDCODEID", "SCTSPA", "SNOMEDCT_US", "SNM")]

# Initialize unmatched code tracker
unmatched_log <- data.table(med_file = character(), varname = character())

#<<< LOAD EVENTS FILES >>>
event_files <- list.files(path = CDM_dir, pattern = "EVENTS", ignore.case = TRUE)

# Read in each event table at a time
for (event in seq_along(event_files)) {
  
  # Extract the event file name
  current_table <- gsub(".csv", "", event_files[event])
  
  # Print message
  message(blue$bold("searching in: ",  current_table))
  
  # Read current events file
  dt <- fread(file.path(CDM_dir, event_files[event]), colClasses = list(character = c("event_code")), stringsAsFactors = FALSE)
  
  if (nrow(dt) == 0) {
    message(red("Skipping empty EVENTS file: ", current_table))
    next
  }
  
  # Keep only needed columns
  dt <- dt[, .(person_id, start_date_record, event_code, event_record_vocabulary, meaning_of_event, event_free_text)]
  
  # Rename columns
  setnames(dt, c("start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event"), c("event_date", "code", "coding_system", "meaning"))
  
  # Convert event date to IDate format
  dt[, event_date := as.IDate(as.character(event_date), format = "%Y%m%d")]
  
  # Merge event table with study population - make sure person_id is of the same type
  dt[, person_id := as.character(person_id)]
  study_population[, person_id := as.character(person_id)]
  
  if (deap_flags$is_EFEMERIS || deap_flags$is_FIN_REG) {
    
    # Set windows - study population
    if (deap_flags$is_EFEMERIS) study_population[, start_window := as.IDate(op_start_date)][, end_window := as.IDate(op_end_date)]
    if (deap_flags$is_FIN_REG)  study_population[, start_window := as.IDate(add_with_rollback(pregnancy_start_date, years(-1)))][, end_window := as.IDate(op_end_date)]
    
    # Set windows - diagnosis 
    dt[, start_event := event_date][,end_event := event_date]
    
    # Set keys
    setkey(study_population, person_id, start_window, end_window)
    setkey(dt, person_id, start_event, end_event)
    
    # Foverlaps join: match prescription dates to pregnancy windows
    dt <- foverlaps(
      dt, study_population,
      by.x = c("person_id", "start_event", "end_event"),   # columns in dt
      by.y = c("person_id", "start_window", "end_window"), # columns in study_population
      type = "within",
      nomatch = 0L
    )
    
    # Delete helper columns
    dt[, c("start_event", "end_event", "start_window", "end_window") := NULL]
    study_population[, c("start_window", "end_window") := NULL]
    
  } else {

    # Merge dt with study population. Keep only those in study population
    dt <- dt[study_population, on = .(person_id), allow.cartesian = TRUE]
    
    # Drop any records that fall outside entry and exit dates
    dt <- dt[event_date >= entry_date & event_date <= exit_date]
  }
  
  # Delete any rows where event_date or code is missing
  dt <- dt[!((is.na(code) | trimws(code) == "") & (is.na(coding_system) | trimws(coding_system) == ""))]

  # Exclusion of meanings ### for BIFAP
  if (pop_prefix == "PC_F" || pop_prefix == "PC_M") dt <- dt[!meaning %in% exclude_meanings_PC]
  
  # Remove any true duplicates
  dt <- unique(dt)
  
  # If no records for dt, then skip to the next Medicines table
  if (nrow(dt) == 0) next

  # Add column with Vocabulary main type i.e. start, READ, SNOMED
  dt[, vocab := fifelse(grepl("ICD|ICPC|CIAP", coding_system, ignore.case = TRUE), "start",
                        fifelse(grepl("READ|RCD", coding_system, ignore.case = TRUE), "READ",
                                fifelse(grepl("SNOMED|SCTSPA|MEDCODEID|SNM", coding_system, ignore.case = TRUE), "SNOMED",
                                        "UNKNOWN")))]
  
  # Get unique vocab type of patient data
  unique_dt_vocab <- unique(dt$vocab)
  
  # For each unique vocab group in dt -shouldn't be more than one but just in case
  for (voc in seq_along(unique_dt_vocab)) {
    
    # Create subset of dataset with the vocabulary
    subset_dt <- dt[vocab == unique_dt_vocab[voc]]
    
    # Check which vocab type
    # if SNOMED, match exactly
    if (unique_dt_vocab[voc] == "SNOMED") {
      
      # Loop over each unique Varname in the exact concept set
      for (varname in unique(dx_concept_set_exact$Varname)) {
        
        # Print message
        message("looking for: ", varname)
        
        # For each varname, create a subset of all the codes belonging to the varname
        concept_subset <- dx_concept_set_exact[Varname == varname]
        
        if (nrow(concept_subset) == 0) {
          message(yellow("Skipping ", varname, ": no SNOMED codes found in concept set"))

          unmatched_log <- rbind(unmatched_log, data.table(
            med_file = current_table,
            varname = varname,
            reason = "No SNOMED codes in concept set"
          ), use.names = TRUE, fill = TRUE)
          next
        }
        
        # Match codes exactly
        subset_dt[, code := as.character(code)]
        concept_subset[, code := as.character(code)]
        
        matched <- subset_dt[code %chin% concept_subset$code]
        matched <- unique(matched)
        
        # Save if there's any match
        if (nrow(matched) > 0) {
          
          # Create column event_definition 
          matched[, event_definition := varname]
          
          # Save file 
          saveRDS(matched, file.path(paths$D3_dir, "tmp", paste0(pop_prefix, "_", varname, "_", unique_dt_vocab[voc], "_", current_table, ".rds")))
          
        } else {
          
          # Print message
          message(red("no matching records found for: ", varname))
          
          # If no matches found, log the unmatched code
          unmatched_log <- rbind(unmatched_log, data.table(
            med_file = current_table,
            varname = varname,
            reason = "No matches in data"
          ), use.names = TRUE, fill = TRUE)
        }
      }
    }
    
    # If ICD or ICPC, remove dot before matching
    if (unique_dt_vocab[voc] == "start") {
      
      for (varname in unique(dx_concept_set_nodot$Varname)) {
        
        # Print message
        message("looking for : ", varname)
        
        # For each varname, create a subset of all the codes belonging to the varname
        concept_subset <- dx_concept_set_nodot[Varname == varname]
        
        if (nrow(concept_subset) == 0) {
          
          # Print message
          message(yellow("Skipping ", varname, ": no ICD/ICPC codes found in concept set"))
          
          # If no matches found, log the unmatched code
          unmatched_log <- rbind(unmatched_log, data.table(
            med_file = current_table,
            varname = varname,
            reason = "No ICD/ICPC codes in concept set"
          ), use.names = TRUE, fill = TRUE)
          next
        }
        
        # Create column in subset_dt with no dot
        subset_dt_tmp <- copy(subset_dt)
        subset_dt_tmp[, code_nodot := gsub("\\.", "", code)]
        
        # Create column in concept_subset with not dot
        concept_subset_tmp <- copy(concept_subset)
        concept_subset_tmp[, code_nodot := gsub("\\.", "", code)]
        
        # Check for matches
        matched <- subset_dt_tmp[code_nodot %chin% concept_subset_tmp$code_nodot]
        matched <- unique(matched)
        
        # Drop column nodot
        matched[, code_nodot := NULL]
        
        # Save if there's any match
        if (nrow(matched) > 0) {
          
          # Create column event_definition 
          matched[, event_definition := varname]
          
          # Save file
          saveRDS(matched, file.path(paths$D3_dir, "tmp", paste0(pop_prefix, "_", varname, "_", unique_dt_vocab[voc], "_", current_table, ".rds")))
          
        } else {
          
          # Print message
          message(red("no matching records found for: ", varname))
          
          # If no matches found, log the unmatched code
          unmatched_log <- rbind(unmatched_log, data.table(
            med_file = current_table,
            varname = varname,
            reason = "No matches in data"
          ), use.names = TRUE, fill = TRUE)
        }
      }
    }
    
    # If Read Code
    if (unique_dt_vocab[voc] == "READ") {
      
      for (varname in unique(dx_concept_set_startswith$Varname)) {
        
        # Print message
        message("looking for: ", varname)

        # For each varname, create a subset of all the codes belonging to the varname
        concept_subset <- dx_concept_set_startswith[Varname == varname]
        
        if (nrow(concept_subset) == 0) {
          
          # Print message
          message(yellow("Skipping ", varname, ": no READ codes found in concept set"))
          
          # If no matches found, log the unmatched code
          unmatched_log <- rbind(unmatched_log, data.table(
            med_file = current_table,
            varname = varname,
            reason = "No READ codes in concept set"
          ), use.names = TRUE, fill = TRUE)
          next
        }
        
        # Create column in concept_subset with escaped dots - literal dot
        concept_subset_tmp <- copy(concept_subset)
        concept_subset_tmp[, code_regex := gsub("\\.", "\\\\.", code)]
        combined_pattern <- paste0("^(", paste(concept_subset_tmp$code_regex, collapse = "|"), ")")
        
        # Check for matches
        matched <- subset_dt[grepl(combined_pattern, code)]
        
        # Save if there's any match
        if (nrow(matched) > 0) {
          
          # Create column event_definition 
          matched[, event_definition := varname]
          
          # Save file 
          saveRDS(matched, file.path(paths$D3_dir, "tmp", paste0(pop_prefix, "_", varname, "_", unique_dt_vocab[voc], "_", current_table, ".rds")))
          
        } else {
          
          # Print Message
          message(red("no matching records found for: ", varname))
          
          # If no matches found, log the unmatched code
          unmatched_log <- rbind(unmatched_log, data.table(
            med_file = current_table,
            varname = varname,
            reason = "No matches in data"
          ), use.names = TRUE, fill = TRUE)
        }
      }
    }
    
    if (unique_dt_vocab[voc] == "UNKNOWN") {
      message(red("Vocabulary Type is Unknown!"))
      next
    }
  }
  rm(dt) #cleanup 
}

# Save unmatched codes
fwrite(unmatched_log, file.path(paths$D5_dir, paste0(pop_prefix, "_unmatched_dx_codes.csv")))

# <<< CONCATENATE SUBSETS AND SAVE IN FOLDERS >>>
# Build table of tmp RDS files
file_info <- data.table(
  
  # Get full file paths for all .rds files inside the "tmp" folder under paths$D3_dir
  path = list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE),
  
  # Get just the base filenames (without the directory) of those same .rds files
  file = basename(list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE))

)

# Create a new column 'varname' by cleaning up the file names:
file_info[, Varname := sub("_[^_]+_EVENTS.*", "", sub("\\.rds$", "", file))]

# Loop over each unique Varname found in the file_info table
for (varname in unique(file_info$Varname)) {
  
  # Get all file paths corresponding to the current varname
  files_to_bind <- file_info[varname == Varname, path]    

  if (length(files_to_bind) == 0) {
    message("No files to bind for: ", varname)
    next
  }
  
  # Read all RDS files for this varname and combine them into one data.table
  combined_dt <- rbindlist(lapply(files_to_bind, readRDS), use.names = TRUE, fill = TRUE)
  
  # Remove true duplicates
  combined_dt <- unique(combined_dt)
  
  # Save the combined data.table back to disk (overwrite or new file)
  saveRDS(combined_dt, file.path(paths$D3_dir, paste0(varname, ".rds")))
}

# Clean up temp folder
invisible(file.remove(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)))
  







      







        








  


