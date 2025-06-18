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

#<<< LOAD MEDICINES FILES >>> 
med_files <- list.files(path = CDM_dir, pattern = "MEDICINES", ignore.case = TRUE) 

for (med in seq_along(med_files)) {
  
  # Extract the medicine file name 
  current_table <- gsub(".csv", "", med_files[med],)
  
  # Print message
  cat(blue$bold(paste0("searching in: ", current_table)), "\n")
  
  # Read Current Medicines file
  dt <- fread(file.path(CDM_dir, med_files[med]), stringsAsFactors = FALSE)
  
  # Keep only needed columns
  dt <- dt[, .(person_id, medicinal_product_atc_code, date_dispensing, date_prescription, 
               meaning_of_drug_record, presc_duration_days, disp_number_medicinal_product, 
               presc_quantity_per_day, medicinal_product_id)]
  
  # Rename columns
  setnames(dt, c("meaning_of_drug_record", "medicinal_product_atc_code"), c("meaning", "code"))
  
  # Create rx_date column (equal to date_dispensing, unless that is missing, then equal to date_prescription)
  dt<-dt[,rx_date:= ifelse(!is.na(date_dispensing), date_dispensing, date_prescription)][, rx_date := as.IDate(as.character(rx_date), format = "%Y%m%d")]
  
  # Make sure person_id is character type in both data sets before merging
  dt[, person_id := as.character(person_id)]
  study_population[, person_id := as.character(person_id)]
  
  # merge dt with study population. Keep only those in study population
  dt <- dt[study_population,on=.(person_id)]
  
  # Drop any records that fall outside entry and exit dates
  # dt <- dt[rx_date >= entry_date & rx_date <= exit_date]
  
  # If no records for dt, then skip to the next Medicines table 
  if (nrow(dt) == 0) next  
  
  # For each ATC code, subset matching rows and append to codes_list
  for (current_code in names(ATC_codelist)) {
    
    # print message
    cat(paste0("looking for: ", current_code), "\n")
    
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
      # Retrieve the 'Varname' corresponding to the current ATC code from the unlisted concept set table
      varname <- ATC_concept_set_unlisted[code == current_code, Varname]
      # Add or update the 'Varname' column in subset_dt with the retrieved value
      subset_dt[, Varname := varname]
      # Save the subset_dt to file
      saveRDS(subset_dt, file.path(paths$D3_dir,"tmp", paste0(varname, "-", current_table,".rds")))
    } else {
      cat(red(paste0("No matching records found for: ", current_code)), "\n")
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
file_info[, Varname := sub("-.*", "", sub("\\.rds$", "", file))]  

# Loop over each unique Varname found in the file_info table
for (varname in unique(file_info$Varname)) {
  
  # Get all file paths corresponding to the current varname
  files_to_bind <- file_info[varname == Varname, path]
  
  # Read all RDS files for this varname and combine them into one data.table
  combined_dt <- rbindlist(lapply(files_to_bind, readRDS), use.names = TRUE, fill = TRUE)
 
  # Save the combined data.table back to disk (overwrite or new file)
  saveRDS(combined_dt, file.path(paths$D3_dir, paste0(pop_prefix, "_", varname, ".rds")))

}

# Clean up temp folder 
invisible(file.remove(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)))

# <<< MOVE FILES TO CORRESPONDING FOLDERS >>> 

# Get names of all immediate sub-folders inside the main D3 directory
# Use basename to extract folder names (not full paths)
subfolder_names <- basename(list.dirs(paths$D3_dir, full.names = TRUE, recursive = FALSE))
# Get a list of all .rds files inside D3 directory, return their full file paths
subset_files <- list.files(paths$D3_dir, pattern = ".rds$", full.names = TRUE)

# Load functions 
source(file.path(thisdir, "scripts", "functions","extract_var_name.R"), local = TRUE)
source(file.path(thisdir, "scripts", "functions","find_matching_row.R"), local = TRUE)

# Track files that were successfully copied
copied_files <- character()

# Loop through each file path in the subset_files vector
for (fpath in subset_files) {
  
  # Extract the variable name from the filename using the helper function
  varname <- extract_varname(fpath)
  
  # Find the matching row in the ATC_concept_sets table for this variable name
  cs_row <- find_matching_row(varname, ATC_concept_sets)
  
  # If no unique matching row was found, print a message and skip to next file
  if (is.null(cs_row)) {
    message("No unique matching row found for: ", varname)
    next
  }
  
  # Define which columns/flags we want to check for TRUE values
  flags <- c("exposure", "cov", "algorithm_input", "dp")
  
  # Initialize a flag to track if the file was copied to any subfolder
  file_copied <- FALSE  
  
  # Iterate through each subfolder name (these correspond to the flags)
  for (sf_name in subfolder_names) {
    
    # Find the flag name matching the folder name, case-insensitive
    flag_col <- flags[tolower(flags) == tolower(sf_name)]
    
    # If the flag column exists and the corresponding flag in cs_row is TRUE
    if (length(flag_col) == 1 && isTRUE(cs_row[[flag_col]])) {
      
      # Print a message indicating the file is being copied and to which folder
      message("Copying ", basename(fpath), " → ", sf_name, " [✓ ", flag_col, "]")
      
      # Construct the destination path combining base directory, subfolder, and filename
      dest_path <- file.path(paths$D3_dir, sf_name, basename(fpath))
      
      # Copy the file to the destination folder, allowing overwrite
      success <- file.copy(fpath, dest_path, overwrite = TRUE)
      
      # If copy was successful, set the file_copied flag to TRUE
      if (success) file_copied <- TRUE
    }
  }
  
  # If file was copied at least once, append its path to the copied_files vector
  if (file_copied) copied_files <- c(copied_files, fpath)
}

# Delete only successfully copied files
if (length(copied_files) > 0) invisible(file.remove(copied_files))
