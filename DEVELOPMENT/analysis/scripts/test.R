# === 1. Load ATC Concept Sets ===
ATC_concept_sets <- rbindlist(
  lapply(
    list.files(
      path = file.path(paths$D3_dir, "concept_sets"),
      pattern = "meds",
      ignore.case = TRUE,
      full.names = TRUE
    ),
    fread
  ),
  use.names = TRUE,
  fill = TRUE
)

ATC_concept_sets <- unique(ATC_concept_sets)

# === 2. Create ATC Lookup: code → list of varname + exact match flag ===
ATC_lookup <- list()
for (i in seq_len(nrow(ATC_concept_sets))) {
  codes <- unlist(strsplit(ATC_concept_sets[i, `ATC codes`], ",\\s*"))
  varname <- ATC_concept_sets[i, Varname]
  exact <- ATC_concept_sets[i, EXACT_MATCH]
  
  for (code in codes) {
    if (!code %in% names(ATC_lookup)) {
      ATC_lookup[[code]] <- list()
    }
    ATC_lookup[[code]][[length(ATC_lookup[[code]]) + 1]] <- list(varname = varname, exact = exact)
  }
}

# === 3. Initialize result list: one data.table per Varname ===
ATC_varname_list <- list()

# === 4. Get list of ATC codes to search ===
all_codes <- names(ATC_lookup)

# === 5. Loop through each MEDICINES file ===
med_files <- list.files(path = CDM_dir, pattern = "MEDICINES", ignore.case = TRUE)

for (med in seq_along(med_files)) {
  print(paste("Searching in:", med_files[med]))
  
  # Read the medicines file
  df <- fread(file.path(CDM_dir, med_files[med]))
  df <- df[, .(person_id, medicinal_product_atc_code, date_dispensing, date_prescription,
               meaning_of_drug_record, presc_duration_days, disp_number_medicinal_product,
               presc_quantity_per_day, medicinal_product_id)]
  
  setnames(df, c("meaning_of_drug_record", "medicinal_product_atc_code"), c("meaning", "code"))
  
  # Compute rx_date
  df[, rx_date := ifelse(!is.na(date_dispensing), date_dispensing, date_prescription)]
  df[, rx_date := as.IDate(as.character(rx_date), format = "%Y%m%d")]
  
  # Harmonize ID format
  df[, person_id := as.character(person_id)]
  study_population[, person_id := as.character(person_id)]
  
  # Merge with study population
  df <- df[study_population, on = .(person_id)]
  if (nrow(df) == 0) next
  
  # === 6. Match codes and assign to Varname groups ===
  for (current_code in all_codes) {
    # Create a lookup: code -> varname(s)
    code_to_varname <- list()
    for (i in seq_len(nrow(ATC_concept_sets))) {
      codes <- unlist(strsplit(ATC_concept_sets[i, `ATC codes`], ",\\s*"))
      varname <- ATC_concept_sets[i, Varname]
      for (code in codes) {
        code_to_varname[[code]] <- unique(c(code_to_varname[[code]], varname))
      }
    }
    
    # Prepare a list to store results by varname
    varname_data <- list()
    
    # Main loop over medicines
    for (med in seq_along(med_files)) {
      print(paste("searching in: ", med_files[med]))
      df <- fread(file.path(CDM_dir, med_files[med]), stringsAsFactors = FALSE)
      df <- df[, .(person_id, medicinal_product_atc_code, date_dispensing, date_prescription,
                   meaning_of_drug_record, presc_duration_days, disp_number_medicinal_product,
                   presc_quantity_per_day, medicinal_product_id)]
      setnames(df, c("meaning_of_drug_record", "medicinal_product_atc_code"), c("meaning", "code"))
      df <- df[, rx_date := ifelse(!is.na(date_dispensing), date_dispensing, date_prescription)][, rx_date := as.IDate(as.character(rx_date), format = "%Y%m%d")]
      df[, person_id := as.character(person_id)]
      study_population[, person_id := as.character(person_id)]
      df <- df[study_population, on = .(person_id)]
      if (nrow(df) == 0) next
      
      for (code in names(code_to_varname)) {
        exact_match <- ATC_concept_sets[`ATC codes` %like% code & Varname %in% code_to_varname[[code]], EXACT_MATCH][1]
        subset_dt <- if (isTRUE(exact_match)) {
          df[code == !!code]
        } else {
          df[startsWith(code, !!code)]
        }
        
        if (nrow(subset_dt) > 0) {
          for (varname in code_to_varname[[code]]) {
            varname_data[[varname]] <- rbindlist(list(varname_data[[varname]], subset_dt), use.names = TRUE, fill = TRUE)
          }
        }
      }
    }
    
  }
}

# === 7. Combine list elements for each Varname into single data.tables ===
for (vname in names(ATC_varname_list)) {
  ATC_varname_list[[vname]] <- rbindlist(ATC_varname_list[[vname]], use.names = TRUE, fill = TRUE)
}
