#################################################################
# Create Diagnosis Subsets
################################################################

print("=======================================================================")
print("========================= CREATING DX SUBSETS =========================")
print("=======================================================================")

# Load all concept sets and bind them. Remove all true duplicates
dx_concept_sets <- unique(rbindlist(lapply(list.files(path = file.path(paths$D3_dir, "concept_sets"), pattern = "dx", ignore.case = TRUE, full.names = TRUE), fread), use.names = TRUE, fill = TRUE))

# Remove any true duplicates 
dx_concept_sets <- unique(dx_concept_sets)

# Create a column in algorithm input for values with no dot
dx_concept_sets[, code_nodot := gsub("\\.", "", code)]

#<<< LOAD EVENTS FILES >>> 
event_files <- list.files(path = CDM_dir, pattern = "EVENTS", ignore.case = TRUE) 

for (event in seq_along(event_files)) {
  
  # Extract the event file name 
  current_table <- gsub(".csv", "", event_files[event])
  
  # Print message
  cat(blue$bold(paste0("searching in: ", current_table)), "\n")
  
  # Read Current Medicines file
  dt <- fread(file.path(CDM_dir, event_files[event]), stringsAsFactors = FALSE)
  
  # Keep only needed columns
  dt <- dt[, .(person_id, start_date_record, event_code, event_record_vocabulary, meaning_of_event, event_free_text)]
  
  # Rename columns
  setnames(dt, c("start_date_record", "event_code", "event_record_vocabulary", "meaning_of_event"), c("event_date", "code", "coding_system", "meaning"))   
  
  # Convert event date to IDate format
  dt[, event_date := as.IDate(as.character(event_date), format = "%Y%m%d")]
  
  # Merge event table with study population - make sure person_id is of the same type
  dt[, person_id := as.character(person_id)]
  study_population[, person_id := as.character(person_id)]
  
  # Merge on person id - keep all in study population
  dt <- dt[study_population,on=.(person_id)]
  
  # Drop any records that fall outside entry and exit dates
  dt <- dt[event_date >= entry_date & event_date <= exit_date]
  
  # Delete any rows where event_date or code is missing
  dt <- dt[!((is.na(code) | trimws(code) == "") & (is.na(coding_system) | trimws(coding_system) == ""))]
  
  # Create a column with removed dot from code if any
  dt[, code_nodot := gsub("\\.", "", code)]
  
  # Exclusion of meanings ### for BIFAP
  # PC: Meanings to be limited/restricted to: "primary_care_events_BIFAP" (or "procedure_primary_care", where applicable); 
  # excludes "primary_care_conditionants_BIFAP", "primary_care_antecedents_BIFAP", "hospitalisation_primary" and "hospitalisation_secundary"
  if(pop_prefix == "PC_F" | pop_prefix == "PC_M"){df<-df[Meaning=="primary_care_events_BIFAP",]}
  # PC_HOSP: Meanings to be limited/restricted to:  "primary_care_events_BIFAP" and "hospitalisation_primary" (or "procedure_primary_care" and  "procedure_during_hospitalisation" where applicable); 
  # excludes "primary_care_conditionants_BIFAP", "primary_care_antecedents_BIFAP" and "hospitalisation_secundary".
  if(pop_prefix == "PC_HOSP_F" | pop_prefix == "PC_HOSP_M"){df<-df[Meaning=="primary_care_events_BIFAP" | Meaning=="hospitalisation_primary",]}
  
  # Remove any true duplicates 
  dt <- unique(dt)
  
  # If no records for dt, then skip to the next Medicines table 
  if (nrow(dt) == 0) next  
  
  # Create a vector of unique event_definitions
  varnames <- unique(dx_concept_sets$Varname)
  
  # For each event definition, subset matching rows in concept set and merge it with dt
  for (var in seq_along(varnames)) {
    
    # print message
    cat(paste0("looking for: ", varnames[var]), "\n")
    
    # Create a subset of codes from concept set matching current def
    dx_concept_sets_subset <- dx_concept_sets[Varname==varnames[var],]
    
    # Create a subset of dt which contains the codes in the algorithm input subset
    subset_dt <- dt[code_nodot %in% dx_concept_sets_subset$code_nodot]
    
    if (nrow(subset_dt) > 0) {
      
      # Remove any true duplicates
      subset_dt <- unique(subset_dt)
      
      # Add or update the 'event_def' column in subset_dt with the retrieved value
      subset_dt[, event_definition := varnames[var]]
      
      # Save the subset_dt to file
      saveRDS(subset_dt, file.path(paths$D3_dir,"tmp", paste0(varnames[var], "_", current_table,".rds")))
      
    } else {
      
      cat(red(paste0("No matching records found for: ", varnames[var]), "\n"))
      
    }
  }
}


# <<< CONCATENATE SUBSETS AND SAVE IN FOLDERS >>>

# Build table of tmp RDS files
file_info <- data.table(
  # Get full file paths for all .rds files inside the "tmp" folder under paths$D3_dir
  path = list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE),
  # Get just the base filenames (without the directory) of those same .rds files
  file = basename(list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE))
)

# Create a new column 'varname' by cleaning up the file names:
file_info[, Varname := sub("_EVENTS.*", "", sub("\\.rds$", "", file))]

# Loop over each unique Varname found in the file_info table
for (varname in unique(file_info$Varname)) {
  
  # Get all file paths corresponding to the current varname
  files_to_bind <- file_info[varname == Varname, path]
  
  # Read all RDS files for this varname and combine them into one data.table
  combined_dt <- rbindlist(lapply(files_to_bind, readRDS), use.names = TRUE, fill = TRUE)
  
  # Remove true duplicates
  combined_dt <- unique(combined_dt)
  
  # Save the combined data.table back to disk (overwrite or new file)
  saveRDS(combined_dt, file.path(paths$D3_dir, paste0(pop_prefix, "_", varname, ".rds")))
  
}

# Clean up temp folder 
invisible(file.remove(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)))

