#################################################################
# Create Concept Sets
################################################################

print("Creating Concept Sets...")

# Load Data
bridge           <- unique(as.data.table(read_excel(file.path(thisdir, "definitions", "bridge", "ADEPT_O1_BRIDGE_19Mayo25.xlsx"), sheet = "OBJ1")))
algorithm_map    <- unique(as.data.table(read_excel(file.path(thisdir, "definitions", "bridge", "ADEPT_O1_BRIDGE_19Mayo25.xlsx"), sheet = "ALG")))
codelist_meds    <- unique(as.data.table(read_excel(file.path(thisdir, "definitions", "codelists", "20250515_ADEPT_medicines.xlsx"))))
codelist_dx      <- fread(file.path(thisdir,"definitions", "codelists", "20260119_ADEPT_full_codelist_clean.csv"), colClasses = list(character = c("code")), stringsAsFactors = FALSE)

# Remove duplicates 
codelist_dx <- unique(codelist_dx, by = c("variable_name", "coding_system", "code"))

# Code lists not participating in algorithms
not_algorithms <- bridge[(exposure == TRUE | cov == TRUE | indication == TRUE | algorithm_input == TRUE | dp == TRUE) & algorithm == FALSE, ]
algorithms     <- bridge[algorithm == TRUE, ]

# Get codes for not_algorithms:
# Cols to loop over
defined_cols <- c("exposure", "cov", "indication", "algorithm_input")

# Loop through each column
for (col in defined_cols) {
  
  if (col %in% names(not_algorithms)) {
    
    # Subset bridge where the column is TRUE
    subset_dt <- not_algorithms[get(col) == TRUE]
    
    if (nrow(subset_dt) > 0) {
      
      # DRUG subset
      drug_dt <- subset_dt[Type_codelist == "drug"]
      
      if (nrow(drug_dt) > 0) {
        
        # Merge variable names with codelist 
        merged_drug <- merge(drug_dt, codelist_meds, by.x = "Varname", by.y = "Drug_abbreviation", all.x = TRUE)
        
        # Check for unmatched variables
        unmatched_drugs <- unique(merged_drug[is.na(`ATC codes`), Varname])
        
        # Print message 
        if (length(unmatched_drugs) > 0) message("No matching drug codes found for: ", paste(unmatched_drugs, collapse = ", "))
        
        # Save file
        fwrite(merged_drug, file = file.path(paths$D3_dir, "concept_sets", paste0(col, "_meds.csv")))
      }
      
      # DIAGNOSIS subset
      dx_dt <- subset_dt[Type_codelist == "diagnosis"]
      
      if (nrow(dx_dt) > 0) {
        
        # Merge variable names with codelist
        merged_dx <- merge(dx_dt, codelist_dx, by.x = "Varname", by.y = "variable_name", all.x = TRUE)
        
        # Check for unmatched variables
        unmatched_dx <- unique(merged_dx[is.na(code), Varname])
        
        # Print message
        if (length(unmatched_dx) > 0) message("No matching diagnosis codes found for: ", paste(unmatched_dx, collapse = ", "))
        
        # Save file
        fwrite(merged_dx, file = file.path(paths$D3_dir, "concept_sets", paste0(col, "_dx.csv")))
      }
    }
  }
}

# --- ALTERNATIVES BLOCK ---
# Get all alternatives from bridge
alternatives_list <- unique(bridge[alternatives == TRUE, Varname])

# Get all component variable names from algorithm_map (for alternatives)
alt_components <- algorithm_map[Algorithm %in% alternatives_list, unique(VariableName)]

# Subset bridge to get rows where Varname is one of the components
alt_component_bridge <- bridge[Varname %in% alt_components]

# Join to codelist_meds to get ATC codes
alt_with_atc <- merge(
  alt_component_bridge,
  codelist_meds,
  by.x = "Varname",
  by.y = "Drug_abbreviation",
  all.x = TRUE
)

# Deduplicate
alt_with_atc <- unique(alt_with_atc)

# Warn if some didn't match
unmatched_alt <- unique(alt_with_atc[is.na(`ATC codes`), Varname])

if (length(unmatched_alt) > 0) {
  message("No matching drug codes found for components: ", paste(unmatched_alt, collapse = ", "))
}

# Save file
fwrite(
  alt_with_atc,
  file = file.path(paths$D3_dir, "concept_sets", "alternatives_meds.csv")
)
