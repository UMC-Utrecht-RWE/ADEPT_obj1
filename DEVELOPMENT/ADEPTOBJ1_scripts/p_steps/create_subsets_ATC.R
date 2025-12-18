#################################################################
# Create Medication Subsets
################################################################
print("===============================================================================")
print("========================= CREATING MEDICATION SUBSETS =========================")
print("===============================================================================")

# Load all concept sets and bind them. Remove all true duplicates
ATC_concept_sets <- unique(rbindlist(lapply(list.files(path = file.path(paths$D3_dir, "concept_sets"), pattern = "meds", ignore.case = TRUE, full.names = TRUE), fread), use.names = TRUE, fill = TRUE))

# Unlist any rows that have multiple codes per Varname (to be )
ATC_concept_set_unlisted <- ATC_concept_sets[, .(code = unlist(strsplit(`ATC codes`, ",\\s*"))), by = Varname]

# Create a list to store the codes + match_type
ATC_codelist <- list()

for (i in seq_len(nrow(ATC_concept_sets))) {
  # Extract the ATC codes string from the i-th row, split by commas, trim spaces, convert to vector
  codes <- unlist(strsplit(ATC_concept_sets[i, `ATC codes`], ",\\s*"))
  # Get the EXACT_MATCH flag (TRUE/FALSE) for the i-th row
  exact <- ATC_concept_sets[i, EXACT_MATCH]
  # For each code in the split list, create a new entry in ATC_codelist
  for (code in codes) ATC_codelist[[code]] <- list(match_type = exact)
}

# Initialize unmatched code tracker
unmatched_log <- data.table(med_file = character(), code = character(), match_type = character())

#<<< LOAD MEDICINES FILES >>>
med_files <- list.files(path = CDM_dir, pattern = "MEDICINES", ignore.case = TRUE)

for (med in seq_along(med_files)) {
  # Extract the medicine file name
  current_table <- gsub(".csv", "", med_files[med])
  # print message
  message(blue$bold("searching in: ",  current_table))
  # Read Current Medicines file
  dt <- fread(file.path(CDM_dir, med_files[med]), stringsAsFactors = FALSE)
  if (nrow(dt) == 0) {
    message(red("Skipping empty MEDICINES file: ", current_table))
    next
  }
  
  # Keep only needed columns
  dt <- dt[, .(person_id, medicinal_product_atc_code, date_dispensing, date_prescription, meaning_of_drug_record, presc_duration_days, disp_number_medicinal_product, presc_quantity_per_day, medicinal_product_id)]
  # Rename columns
  setnames(dt, c("meaning_of_drug_record", "medicinal_product_atc_code"), c("meaning", "code"))
  # Create rx_date column (equal to date_dispensing, unless that is missing, then equal to date_prescription)
  dt[, rx_date := ifelse(!is.na(date_dispensing), date_dispensing, date_prescription)][, rx_date := as.IDate(as.character(rx_date), format = "%Y%m%d")]
  # Make sure person_id is character type in both data sets before merging
  dt[, person_id := as.character(person_id)]
  study_population[, person_id := as.character(person_id)]
  if (deap_flags$is_EFEMERIS || deap_flags$is_FIN_REG) {
    # add interval columns
    study_population[, `:=`(start_window = op_start_date, end_window = op_end_date)]
    dt[, `:=`(start_event = rx_date, end_event = rx_date)]
    # set keys
    setkey(study_population, person_id, start_window, end_window)
    setkey(dt, person_id, start_event, end_event)
    # foverlaps join: match prescription dates to pregnancy windows
    dt <- foverlaps(
      dt, study_population,
      by.x = c("person_id", "start_event", "end_event"),   # columns in dt
      by.y = c("person_id", "start_window", "end_window"), # columns in study_population
      type = "within",
      nomatch = 0L
    )
    # clean up helper columns
    dt[, c("start_event", "end_event", "start_window", "end_window") := NULL]
    study_population[, c("start_window", "end_window") := NULL]
  } else {
    # merge dt with study population. Keep only those in study population
    dt <- dt[study_population, on = .(person_id), allow.cartesian = TRUE]
  }
  # If no records for dt, then skip to the next Medicines table
  if (nrow(dt) == 0) next
  # For each ATC code, subset matching rows and append to codes_list
  for (current_code in names(ATC_codelist)) {
    # SIDIAP - skip legacy GABA codes 
    if (deap_flags$is_SIDIAP &&
        current_code %in% c("N03AX12", "N03AX16", "N02BG11", "N03AX")) {
      next
    }
    # print message
    message("looking for: ", current_code)
    # Check for match_type
    exact <- ATC_codelist[[current_code]]$match_type
    # If exact = TRUE, then we do an exact match. If exact = FALSE, then we look for any codes that start with
    if (exact) {
      subset_dt <- dt[code == current_code]
    } else {
      subset_dt <- dt[startsWith(code, current_code)]
    }
    # Check if the subset data table has any rows. Proceed only if it's not empty.
    if (nrow(subset_dt) > 0) {
      # Remove any true duplicates
      subset_dt <- unique(subset_dt)
      # Retrieve the 'Varname' corresponding to the current ATC code from the unlisted concept set table
      varname <- ATC_concept_set_unlisted[code == current_code, Varname]
      # Add or update the 'Varname' column in subset_dt with the retrieved value
      subset_dt[, Varname := varname]
      # Save the subset_dt to file
      saveRDS(subset_dt, file.path(paths$D3_dir, "tmp", paste0(varname, "-",current_table,"_",current_code,".rds")))
    } else {
      # print message
      message(red("no matching records found for: ", current_code))
      # If no matches found, log the unmatched code
      unmatched_log <- rbind(unmatched_log, data.table(
        med_file = current_table,
        code = current_code,
        match_type = ifelse(exact, "exact", "prefix")
      ), use.names = TRUE, fill = TRUE)
    }
  }
}


#################################################
# Save unmatched codes
fwrite(unmatched_log, file.path(paths$D5_dir, paste0(pop_prefix, "_unmatched_ATC_codes.csv")))

# <<< CONCATENATE SUBSETS AND SAVE IN FOLDERS >>>

# Build table of tmp RDS files
file_info <- data.table(
  # Get full file paths for all .rds files inside the "tmp" folder under paths$D3_dir
  path = list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE),
  # Get just the base filenames (without the directory) of those same .rds files
  file = basename(list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE))
)

# Create a new column 'varname' by cleaning up the file names:
file_info[, Varname := sub("-.*", "", sub("\\.rds$", "", file))]

# Loop over each unique Varname found in the file_info table
for (varname in unique(file_info$Varname)) {
  # Get all file paths corresponding to the current varname
  files_to_bind <- file_info[varname == Varname, path]
  # Read all RDS files for this varname and combine them into one data.table
  combined_dt <- rbindlist(lapply(files_to_bind, readRDS), use.names = TRUE, fill = TRUE)
  # Remove any true duplicates
  combined_dt <- unique(combined_dt)
  # Save the combined data.table back to disk (overwrite or new file)
  saveRDS(combined_dt, file.path(paths$D3_dir, paste0(pop_prefix, "_", varname, ".rds")))
}

# Clean up temp folder
invisible(file.remove(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)))
