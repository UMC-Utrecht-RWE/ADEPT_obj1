setDT(bridge)
setDT(codelist_meds)
setDT(codelist_dx)

# Create drug/diagnosis subsets
bridge_meds <- bridge[Type_codelist=="drug",]
bridge_dx   <- bridge[Type_codelist=="diagnosis",]

# Extract unique values
bridge_vars_meds   <- unique(bridge_meds$Varname)
bridge_vars_dx     <- unique(bridge_dx$Varname)
codelist_vars_ATC  <- unique(codelist_meds$Drug_abbreviation)
codelist_vars_dx   <- unique(codelist_dx$variable_name)

### ATC 
# Values in bridge missing in codelist
in_bridge_not_codelist_meds <- setdiff(bridge_vars_meds, codelist_vars_ATC)

# Check which values in codelist are missing in bridge
in_codelist_meds_not_bridge <- setdiff(codelist_vars_ATC, bridge_vars_meds)

# Print results
cat("Values in bridge but not in codelist_meds:\n")
print(in_bridge_not_codelist_meds)

cat("\nValues in codelist_meds but not in bridge:\n")
print(in_codelist_meds_not_bridge)

### Diagnosis 

# Values in bridge missing in codelist
in_bridge_not_codelist_dx <- setdiff(bridge_vars_dx, codelist_vars_dx)

# Check which values in codelist are missing in bridge
in_codelist_dx_not_bridge <- setdiff(codelist_vars_dx, bridge_vars_dx)

# Print results
cat("Values in bridge but not in codelist_dx:\n")
print(in_bridge_not_codelist_dx)

cat("\nValues in codelist_dx but not in bridge:\n")
print(in_codelist_dx_not_bridge)

