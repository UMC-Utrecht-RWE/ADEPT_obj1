#################################################################
# Create Diagnosis Subsets
################################################################

print("=======================================================================")
print("========================= CREATING DX SUBSETS =========================")
print("=======================================================================")

# Load all concept sets and bind them. Remove all true duplicates
dx_concept_sets <- unique(rbindlist(lapply(list.files(path = file.path(paths$D3_dir, "concept_sets"), pattern = "dx", ignore.case = TRUE, full.names = TRUE), fread), use.names = TRUE, fill = TRUE))

# create subsets of dx_concept_sets
dx_concept_set_nodot      <- dx_concept_sets[coding_system %in% c("ICD10", "ICD10CM", "ICD10DA", "ICD9CM", "ICD9CMP", "ICPC", "ICPC2P","MTHICD9", "ICPC2EENG")]
dx_concept_set_startswith <- dx_concept_sets[coding_system %in% c("RCD","RCD2")]
dx_concept_set_exact      <- dx_concept_sets[coding_system %in% c("MEDCODEID", "SCTSPA", "SNOMEDCT_US", "SNM")]

# Initialize unmatched code tracker
unmatched_log <- data.table(med_file = character(), varname = character())

#<<< LOAD EVENTS FILES >>> 
event_files <- list.files(path = CDM_dir, pattern = "EVENTS", ignore.case = TRUE) # Checks for EVENTS Tables present

# Are events tables present
if(length(event_files)>0){
  
  # Read in each event table at a time
  for (event in seq_along(event_files)) {
    
    # Extract the event file name 
    current_table <- gsub(".csv", "", event_files[event])

    # print message
    message(blue$bold("searching in: ",  current_table))
    
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
    
    # Exclusion of meanings ### for BIFAP
    # PC: Meanings to be limited/restricted to: "primary_care_events_BIFAP" (or "procedure_primary_care", where applicable); 
    # excludes "primary_care_conditionants_BIFAP", "primary_care_antecedents_BIFAP", "hospitalisation_primary" and "hospitalisation_secundary"
    if(pop_prefix == "PC_F" | pop_prefix == "PC_M") dt <- dt[meaning=="primary_care_events_BIFAP",]
    # PC_HOSP: Meanings to be limited/restricted to:  "primary_care_events_BIFAP" and "hospitalisation_primary" (or "procedure_primary_care" and  "procedure_during_hospitalisation" where applicable); 
    # excludes "primary_care_conditionants_BIFAP", "primary_care_antecedents_BIFAP" and "hospitalisation_secundary".
    if(pop_prefix == "PC_HOSP_F" | pop_prefix == "PC_HOSP_M") dt <- dt[meaning=="primary_care_events_BIFAP" | meaning=="hospitalisation_primary",]
    
    # Remove any true duplicates 
    dt <- unique(dt)
    
    # If no records for dt, then skip to the next Medicines table 
    if (nrow(dt) == 0) next  
    
    # Adds column with Vocabulary main type i.e. start, READ, SNOMED
      dt[, vocab := fifelse(grepl("ICD|ICPC|CIAP", coding_system, ignore.case = TRUE), "start",
                            fifelse(grepl("READ|RCD", coding_system, ignore.case = TRUE), "READ",
                                    fifelse(grepl("SNOMED|SCTSPA|MEDCODEID|SNM", coding_system, ignore.case = TRUE), "SNOMED", 
                                            "UNKNOWN")))]
      
      # get unique vocab type of patient data 
      unique_dt_vocab <- unique(dt$vocab)
      
      # for each unique vocab group in dt -shouldn't be more than one but just in case 
      for (voc in seq_along(unique_dt_vocab)) {
        
        # create subset of dataset with the vocabulary
        subset_dt <- dt[vocab == unique_dt_vocab[voc]]
        
        # Check which vocab type 
        
        # if SNOMED, match exactly 
        if (unique_dt_vocab[voc] == "SNOMED") {
          
          # Loop over each unique Varname in the exact concept set
          for (varname in unique(dx_concept_set_exact$Varname)) {
            
            # print message
            message("looking for: ", varname)
            
            # For each varname, create a subset of all the codes belonging to the varname 
            concept_subset <- dx_concept_set_exact[Varname == varname]
            
            # Match codes exactly
            matched <- subset_dt[code %chin% concept_subset$code]
            matched <- unique(matched)
            
            # Save if there's any match
            if (nrow(matched) > 0) {
              matched[, event_definition := varname]
              saveRDS(matched,file.path(paths$D3_dir, "tmp", paste0(pop_prefix, "_", varname, "_", unique_dt_vocab[voc], "_", current_table, ".rds")))
              
            } else {
              
              message(red("no matching records found for: ", varname))
              # If no matches found, log the unmatched code
              unmatched_log <- rbind(unmatched_log, data.table(
                med_file = current_table,
                varname = varname
              ), use.names = TRUE, fill = TRUE)
            }
          }
        }
        
        # If ICD or ICPC, remove dot before matching 
        if (unique_dt_vocab[voc] == "start") {
          
          for (varname in unique(dx_concept_set_nodot$Varname)) {
            
            # print message
            message("looking for : ", varname)
            
            # For each varname, create a subset of all the codes belonging to the varname 
            concept_subset <- dx_concept_set_nodot[Varname == varname]
            
            # create column in subset_dt with no dot 

            subset_dt_tmp <- copy(subset_dt)
            subset_dt_tmp[, code_nodot := gsub("\\.", "", code)]

            # create column in concept_subset with not dot 
            concept_subset_tmp <- copy(concept_subset)
            concept_subset_tmp[, code_nodot := gsub("\\.", "", code)]

            # check for matches 
            matched <- subset_dt_tmp[code_nodot %chin% concept_subset_tmp$code_nodot]
            matched <- unique(matched)
            # drop column nodot 
            matched[, code_nodot := NULL]
            
            # Save if there's any match
            if (nrow(matched) > 0) {
              matched[, event_definition := varname]
              saveRDS(matched,file.path(paths$D3_dir, "tmp", paste0(pop_prefix, "_", varname, "_", unique_dt_vocab[voc], "_", current_table, ".rds")))
            } else {
              message(red("no matching records found for: ", varname))
              # If no matches found, log the unmatched code
              unmatched_log <- rbind(unmatched_log, data.table(
                med_file = current_table,
                varname = varname
              ), use.names = TRUE, fill = TRUE)
            }
          }
        }
        
        # If Read Code 
        if(unique_dt_vocab[voc] == "READ") {
          
          for (varname in unique(dx_concept_set_startswith$Varname)) {
            # print message
            message("looking for: ", varname)
            
            # For each varname, create a subset of all the codes belonging to the varname 
            concept_subset <- dx_concept_set_startswith[Varname == varname]
            
            # create column in concept_subset with escaped dots - literal dot
            concept_subset_tmp <- copy(concept_subset)
            concept_subset_tmp[, code_regex := gsub("\\.", "\\\\.", code)]
            
            combined_pattern <- paste0("^(", paste(concept_subset_tmp$code_regex, collapse = "|"), ")")
            
            matched <- subset_dt[grepl(combined_pattern, code)]
            
            # Save if there's any match
            if (nrow(matched) > 0) {
              matched[, event_definition := varname]
              saveRDS(matched,file.path(paths$D3_dir, "tmp", paste0(pop_prefix, "_", varname, "_", unique_dt_vocab[voc], "_", current_table, ".rds")))
            } else {
              
              message(red("no matching records found for: ", varname))
              
              # If no matches found, log the unmatched code
              unmatched_log <- rbind(unmatched_log, data.table(
                med_file = current_table,
                varname = varname
              ), use.names = TRUE, fill = TRUE)
            }
          }
        }
        
        if(unique_dt_vocab[voc] == "UNKNOWN"){
          message(red("Vocabulary Type is Unknown!"))
          next
        }
      }
  }
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


