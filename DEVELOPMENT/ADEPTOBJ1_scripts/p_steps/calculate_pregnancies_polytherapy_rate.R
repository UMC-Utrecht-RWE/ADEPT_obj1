###############################################################################################################################################################################
# <<< Sub-objective 1.4: Polytherapy rate during pregnancy >>>
# Measure: Polytherapy rate during pregnancy
# Numerator: The number of pregnancies with >=2 distinct ASM treatment episodes taken concurrently for >=3 months during the pregnancy period
# Denominator: Total number of pregnancies (changed from pre pregnancy users!!!)
# Stratification by: Overall, individual drug substance, drug sub-groups, indication, calendar year, data source

# Conditions:
### Pre-pregnancy users
###
###############################################################################################################################################################################
print("==================================================================================================")
print("========================= CALCULATING POLYTHERAPY RATES DURING PREGNANCY =========================")
print("==================================================================================================")

# Subgroups to be excluded
exclude <- paste(c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS"), collapse = "|")

# Pre-pregnancy data and counts
files_prepregnancy <- list.files(file.path(paths$D4_dir, "1.3_pre-pregnancy_use"))
files_prepregnancy <- files_prepregnancy[!grepl(exclude, files_prepregnancy)] # Apply exclusions

# Polytherapy episodes
files_polytherapy_episodes <- list.files(file.path(paths$D4_dir, "1.2_polytherapy"), pattern = "\\.rds$")
if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) files_polytherapy_episodes[grepl("_F_", files_polytherapy_episodes)]

# Read in and bind all files and remove true duplicates
dt_prepreg <- unique(rbindlist(lapply(file.path(paths$D4_dir, "1.3_pre-pregnancy_use", files_prepregnancy), readRDS), fill = TRUE))
dt_poly    <- unique(rbindlist(lapply(file.path(paths$D4_dir, "1.2_polytherapy",       files_polytherapy_episodes), readRDS), fill = TRUE))

# Keep only columns you need: pre-pregnancy
dt_prepreg <- dt_prepreg[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, preg_year)]

# Keep only columns you need: polytherapy
dt_poly <- dt_poly[, .(person_id, sex_at_instance_creation, birth_date, start_follow_up, end_follow_up, atc_group, i.atc_group, overlap_start, overlap_end, overlap_days)]
setnames(dt_poly, c("atc_group", "i.atc_group"), c("atc_group_poly1", "atc_group_poly2"))

# Overlap should be between start and end fu - (not for EFEMERIS or FINLAND)
if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) dt_poly <- dt_poly[overlap_start >= start_follow_up & overlap_start <= end_follow_up & overlap_end >= start_follow_up & overlap_end <= end_follow_up]

# Keep polytherapy rows only if overlap >= 3 months
dt_poly <- dt_poly[overlap_days >= 91, ]

# Merge poly records with pre-pregnancy records
dt <- merge(dt_prepreg, dt_poly, by = "person_id", all = FALSE, allow.cartesian = TRUE)

# Check if overlap is within pregnancy
if (nrow(dt) > 0) {
  
  # Calculate overlap within pregnancy period
  dt[, overlap_days_within_pregnancy := as.numeric(pmin(overlap_end, pregnancy_end_date) - pmax(overlap_start, pregnancy_start_date) + 1)]
  
  # If no overlap set overlap days to 0
  dt[overlap_days_within_pregnancy < 0, overlap_days_within_pregnancy := 0]
  
  # flag if at least 3 months overlap
  dt[, overlap_3months := overlap_days_within_pregnancy >= 91]
  
  # Create a subset where the overlap within pregnancy is >= 91 days
  dt_subset <- dt[overlap_3months == TRUE, ]
  
  if (nrow(dt_subset) > 0) {
    
    # Keep one pregnancy per year
    dt_subset <- unique(dt_subset, by = c("pregnancy_id", "preg_year"))
    
    # Count by pregnancy
    poly_counts <- dt_subset[, .(N = .N), by = preg_year]
    
    # Merge with template to get all years
    poly_counts <- merge(empty_dt[, .(preg_year)], poly_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    poly_all <- merge(poly_counts, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    poly_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate polytherapy as a rate (*1000)
    poly_all[, rate := round(1000 * N / Freq, 3)]
    poly_all[N == 0 & Freq == 0, rate := 0]
    
    # Create column marking if rate is computable
    poly_all[, rate_computable := Freq > 0]
    
    # Warnings
    if (nrow(poly_all[N > Freq]) > 0) warning(red("Warning: Numerator > Denominator"))
    if (nrow(poly_all[Freq == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator"))
    
    # Save odd cases
    if (nrow(poly_all[N > Freq]) > 0) fwrite(poly_all[N > Freq], file.path(paths$D5_dir, "1.4_pregnancy_polytherapy", paste0(treatment, "_num_gt_denominator.csv")))
    if (nrow(poly_all[Freq == 0 & N != 0]) > 0) fwrite(poly_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.4_pregnancy_polytherapy", paste0(treatment, "_denominator_zero_numerator_nonzero.csv")))
    
    # Rename columns
    setnames(poly_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save output
    saveRDS(dt_subset, file.path(paths$D4_dir, "1.4_pregnancy_polytherapy", paste0(pop_prefix, "_polytherapy_in_pregnancy_data.rds")))
    saveRDS(poly_all, file.path(paths$D5_dir, "1.4_pregnancy_polytherapy", paste0(pop_prefix, "_polytherapy_in_pregnancy_counts.rds")))
    
  } else {
    
    # Print message
    message("Prepregnancy users with overlap of 3 months or more during pregnancy period found")
  }
  
} else {
  
  # Print message
  message(red("No polytherapy episodes found during pregnancy period"))
}
