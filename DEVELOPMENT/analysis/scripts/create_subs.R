# Read in all concept sets for ATC codes and bind them
ATC_concept_sets <- rbindlist(lapply(list.files(path = file.path(paths$D3_dir, "concept_sets"), pattern = "meds", ignore.case = TRUE, full.names = TRUE), fread), use.names = TRUE, fill = TRUE)
ATC_concept_sets <- unique(ATC_concept_sets)
ATC_code_map <- ATC_concept_sets[, .(code = unlist(strsplit(`ATC codes`, ",\\s*"))), by = Varname]

# Create a list to store the codes 
ATC_codelist <- list()

for (i in seq_len(nrow(ATC_concept_sets))) {
  codes <- unlist(strsplit(ATC_concept_sets[i, `ATC codes`], ",\\s*"))
  exact <- ATC_concept_sets[i, EXACT_MATCH]
  for (code in codes) {ATC_codelist[[code]] <- list(match_type = exact, data = list())
  }
}

# Prepare code groups
exact_codes  <- names(Filter(function(x) x$match_type, ATC_codelist))
prefix_codes <- names(Filter(function(x) !x$match_type, ATC_codelist))

# Load Medicines Files 
med_files <- list.files(path=CDM_dir, pattern = "MEDICINES", ignore.case = TRUE) 

for (med in seq_along(med_files)) {
  # Print message
  print(paste("searching in: ", med_files[med]))
  
  # Read Current Medicines file
  df <- fread(file.path(CDM_dir, med_files[med]), stringsAsFactors = FALSE)
  
  # Keep only needed columns
  df <- df[, .(person_id, medicinal_product_atc_code, date_dispensing, date_prescription, 
               meaning_of_drug_record, presc_duration_days, disp_number_medicinal_product, 
               presc_quantity_per_day, medicinal_product_id)]
  
  # Rename columns
  setnames(df, c("meaning_of_drug_record", "medicinal_product_atc_code"), c("meaning", "code"))
  
  # Create rx_date column (equal to date_dispensing, unless that is missing, then equal to date_prescription)
  df<-df[,rx_date:= ifelse(!is.na(date_dispensing), date_dispensing, date_prescription)][, rx_date := as.IDate(as.character(rx_date), format = "%Y%m%d")]
  
  # Make sure person_id is character type in both data sets before merging
  df[, person_id := as.character(person_id)]
  study_population[, person_id := as.character(person_id)]
  
  # merge df with study population. Keep only those in study population
  df <- df[study_population,on=.(person_id)]
  
  # If no records for df, then skip to the next Medicines table 
  if (nrow(df) == 0) next  
  
  # For each ATC code, subset matching rows and append to codes_list
  for (current_code in names(ATC_codelist)) {
    # print message
    print(paste("looking for: ", current_code))
    
    # Check for match_type
    exact <- ATC_codelist[[current_code]]$match_type
    
    if (exact) {
      
      subset_dt <- df[code == current_code] 
      
    } else {
      
      subset_dt <- df[startsWith(code, current_code)]
    }
    
    if (nrow(subset_dt) > 0) {
      
      varname <- ATC_code_map[code == current_code, Varname]
      subset_dt[, Varname := varname]
      
      subset_dt[, Varname := varname]
      
      ATC_codelist[[current_code]]$data[[length(ATC_codelist[[current_code]]$data) + 1]] <- subset_dt
      
    }
  }
  
}

# Save all subsets 
for (current_code in names(ATC_codelist)) {
  
  combined_dt <- rbindlist(ATC_codelist[[current_code]]$data, use.names = TRUE, fill = TRUE)
  
  # Split combined_dt by Varname
  varname_splits <- split(combined_dt, by = "Varname", keep.by = TRUE)
  
  for (varname in names(varname_splits)) {
    file_path <- file.path(paths$D3_dir, paste0(pop_prefix, "_", varname, ".rds"))
    
    if (file.exists(file_path)) {
      # Load existing data, append new data, then save
      existing_data <- readRDS(file_path)
      new_data <- rbind(existing_data, varname_splits[[varname]])
      saveRDS(new_data, file_path)
    } else {
      # Save new data
      saveRDS(varname_splits[[varname]], file_path)
    }
  }
}

# Move files to folders 

subfolder_names <- basename(list.dirs(paths$D3_dir, full.names = TRUE, recursive = FALSE))
# Get list of files to move
subset_files <- list.files(paths$D3_dir, pattern = ".rds$", full.names = TRUE)

# Helper: Extract variable name 
extract_varname <- function(filepath) {
  prefix <- get("pop_prefix", envir = .GlobalEnv)
  fname <- basename(filepath)
  sub(paste0("^", prefix, "_(.*)\\.rds$"), "\\1", fname)
}

# Helper: Check if variable name matches concept_sets_ATC row either exactly on Varname or inside ATC codes list
find_matching_row <- function(varname, dt) {
  # Exact match on Varname
  exact_row <- dt[Varname == varname]
  
  if (nrow(exact_row) == 1) return(exact_row)
  
  # If no exact match, try to find variable name inside comma-separated ATC codes column (partial matching with trimming spaces)
  # Split each row's ATC codes by comma, trim spaces, then check if variable name is in any
  dt[, ATC_codes_list := strsplit(`ATC codes`, ",\\s*")]
  
  row_idx <- dt[sapply(ATC_codes_list, function(codes) varname %in% codes), which = TRUE]
  
  if (length(row_idx) == 1) return(dt[row_idx])
  
  # No or multiple matches:
  return(NULL)
}

# Track files that were successfully copied
copied_files <- character()

# Loop through files
for (fpath in subset_files) {
  
  varname <- extract_varname(fpath)
  
  cs_row <- find_matching_row(varname, ATC_concept_sets)
  
  if (is.null(cs_row)) {
    message("No unique matching row found for: ", varname)
    next
  }
  
  # Columns to check for TRUE
  flags <- c("EXPOSURE", "COV", "Algorithm_input", "DP")
  
  file_copied <- FALSE  # Flag to track whether file was copied anywhere
  
  # For each folder name that matches a flag AND flag is TRUE, move file there
  for (sf_name in subfolder_names) {
    # Match folder name ignoring case
    flag_col <- flags[tolower(flags) == tolower(sf_name)]
    
    if (length(flag_col) == 1 && isTRUE(cs_row[[flag_col]])) {
      
      message("Copying ", basename(fpath), " → ", sf_name, " [✓ ", flag_col, "]")
      
      dest_path <- file.path(paths$D3_dir, sf_name, basename(fpath))
      
      success <- file.copy(fpath, dest_path, overwrite = TRUE)
      
      if (success) file_copied <- TRUE
    }
  }
  
  # Only add to copied list if copied at least once
  if (file_copied) copied_files <- c(copied_files, fpath)
}

# Safely delete only successfully copied files
if (length(copied_files) > 0) invisible(file.remove(copied_files))

