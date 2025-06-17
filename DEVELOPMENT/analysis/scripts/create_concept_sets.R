# Load Data
bridge        <- unique(as.data.table(read_excel(file.path(thisdir, "definitions", "bridge", "ADEPT_O1_BRIDGE_19Mayo25.xlsx"), sheet = "OBJ1")))
algorithm_map <- unique(as.data.table(read_excel(file.path(thisdir, "definitions", "bridge", "ADEPT_O1_BRIDGE_19Mayo25.xlsx"), sheet = "ALG")))
codelist_meds <- unique(as.data.table(read_excel(file.path(thisdir, "definitions", "codelists", "20250515_ADEPT_medicines.xlsx"))))
codelist_dx   <- unique(as.data.table(read_csv  (file.path(thisdir, "definitions", "codelists", "20250429_ADEPT_full_codelist.csv"), show_col_types = FALSE)))

# Codelists not participating in algorithms 
not_algorithms <- bridge[(EXPOSURE == TRUE | COV == TRUE | INDICATION == TRUE | ALTERNATIVES == TRUE | Algorithm_input == TRUE | DP == TRUE) & Algorithm == FALSE, ]
algorithms     <- bridge[Algorithm == TRUE, ]

# Get codes for not_algorithms: 
# Cols to loop over
defined_cols <- c("EXPOSURE", "COV", "INDICATION", "ALTERNATIVES", "Algorithm_input", "DP")

# Loop through each column
for (col in defined_cols) {
  
  if (col %in% names(not_algorithms)) {
    
    # Subset bridge where the column is TRUE
    subset_dt <- not_algorithms[get(col) == TRUE]
    
    if (nrow(subset_dt) > 0) {
      
      # DRUG subset
      drug_dt <- subset_dt[Type_codelist == "drug"]
      
      if (nrow(drug_dt) > 0) {
        
        merged_drug <- merge(drug_dt, codelist_meds, by.x = "Varname", by.y = "Drug_abbreviation", all.x = TRUE)
        
        # Check for unmatched variables
        unmatched_drugs <- unique(merged_drug[is.na(`ATC codes`), Varname])
        
        if (length(unmatched_drugs) > 0) message("No matching drug codes found for: ", paste(unmatched_drugs, collapse = ", "))
        
        fwrite(merged_drug, file = file.path(paths$D3_dir, "concept_sets", paste0(col, "_meds.csv")))
      }
      
      # DIAGNOSIS subset
      dx_dt <- subset_dt[Type_codelist == "diagnosis"]
      
      if (nrow(dx_dt) > 0) {
        
        merged_dx <- merge(dx_dt, codelist_dx, by.x = "Varname", by.y = "variable_name", all.x = TRUE)
        
        unmatched_dx <- unique(merged_dx[is.na(code), Varname])
        
        if (length(unmatched_dx) > 0) {message("No matching diagnosis codes found for: ", paste(unmatched_dx, collapse = ", "))}
        
        fwrite(merged_dx, file = file.path(paths$D3_dir, "concept_sets", paste0(col, "_dx.csv")))
      }
    }
  }
}

# Algorithm Value Check
# If Algorithm is TRUE in bridge, are all the variables == to all the variables in the Algorithm column of algorithm_map and vice versa
alg_in_bridge_not_in_map <-  setdiff(bridge[Algorithm == TRUE]$Varname, algorithm_map$Algorithm)
alg_in_map_not_in_bridge <-  setdiff(algorithm_map$Algorithm, bridge[Algorithm == TRUE]$Varname)

# If Algorithm input is TRUE in bridge, are all the variables == to all the variables in the variable name column of algorithm_map and vice versa
alg_input_in_bridge_not_in_map <- setdiff(bridge[Algorithm_input == TRUE]$Varname, algorithm_map$VariableName)
alg_input_in_map_not_in_bridge <- setdiff(algorithm_map$VariableName, bridge[Algorithm_input == TRUE]$Varname)


cat("Algorithm Values in bridge but not in algorithm map:\n")
print(alg_in_bridge_not_in_map)

cat("Algorithm Values in algorithm map but not in bridge:\n")
print(alg_in_map_not_in_bridge)

cat("Algorithm Input Values in bridge but not in algorithm map:\n")
print(alg_input_in_bridge_not_in_map)

cat("Algorithm Input Values in algorithm map but not in bridge:\n")
print(alg_input_in_map_not_in_bridge)


