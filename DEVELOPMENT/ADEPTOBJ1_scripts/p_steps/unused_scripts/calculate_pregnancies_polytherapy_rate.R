###############################################################################################################################################################################
# <<< Sub-objective 1.4: Polytherapy rate during pregnancy >>> 
# Measure: Polytherapy rate during pregnancy
# Numerator: The number of pregnancies with ≥2 distinct ASM treatment episodes taken concurrently for ≥3 months during the pregnancy period
# Denominator: Total number of pre-pregnancy users of an ASM in a calendar year in the data source 
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
files_counts       <- list.files(file.path(paths$D5_dir, "1.3_pre-pregnancy_use"))

# Apply exclusions
files_prepregnancy <- files_prepregnancy[!grepl(exclude, files_prepregnancy)]
files_counts       <- files_counts[!grepl(exclude, files_counts)]

# Polytherapy episodes 
files_polytherapy_episodes <- list.files(file.path(paths$D4_dir, "1.2_polytherapy"), pattern = "\\.rds$")

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  files_polytherapy_episodes <- files_polytherapy_episodes[grepl("_F_", files_polytherapy_episodes)] # Females only
  if(pop_prefix == "PC") files_prepregnancy  <- files_prepregnancy[!grepl("PC_HOSP", files_prepregnancy)] #BIFAP
  if(pop_prefix == "PC") files_counts <- files_counts[!grepl("PC_HOSP", files_counts)] #BIFAP
  if(pop_prefix=="PC") files_polytherapy_episodes <- files_polytherapy_episodes[!grepl("PC_HOSP", files_polytherapy_episodes)] # BIFAP
}

# Read in all files 
dt_prepreg <- rbindlist(lapply(file.path(paths$D4_dir, "1.3_pre-pregnancy_use", files_prepregnancy), readRDS), fill = TRUE)
dt_counts  <- rbindlist(lapply(file.path(paths$D5_dir, "1.3_pre-pregnancy_use", files_counts), readRDS), fill = TRUE)
dt_poly    <- rbindlist(lapply(file.path(paths$D4_dir, "1.2_polytherapy",       files_polytherapy_episodes), readRDS), fill = TRUE)

if (nrow(dt_prepreg) > 0 && nrow(dt_counts) > 0 && nrow(dt_poly) > 0) {
  # All three have rows, so proceed...
  message("All datasets have data, proceeding...")
  
  if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
    # drop unneeded columns
    dt_prepreg <- dt_prepreg[, .(person_id, atc_group, episode.start, episode.end, pregnancy_start_date, pregnancy_end_date)]
    dt_poly <- dt_poly[, .(person_id, atc_group, i.atc_group, overlap_start, overlap_end)]
    # rename cols
    setnames(dt_prepreg, "atc_group", "atc_group_prepreg")
    setnames(dt_poly, c("atc_group", "i.atc_group"), c("atc_group_poly1", "atc_group_poly2"))
    # remove duplicates
    dt_prepreg <- unique(dt_prepreg)
    dt_poly <- unique(dt_poly)
    # merge prepregnancy data with polytherapy file
    dt <- merge(dt_prepreg, dt_poly, by = "person_id", all = FALSE, allow.cartesian = TRUE)
    
  } else {
    # drop unneeded columns
    dt_prepreg <- dt_prepreg[, .(person_id, pregnancy_id, atc_group, episode.start, episode.end, pregnancy_start_date, pregnancy_end_date)]
    dt_poly <- dt_poly[, .(person_id, pregnancy_id, atc_group, i.atc_group, overlap_start, overlap_end)]
    # rename cols
    setnames(dt_prepreg, "atc_group", "atc_group_prepreg")
    setnames(dt_poly, c("atc_group", "i.atc_group"), c("atc_group_poly1", "atc_group_poly2"))
    # remove duplicates
    dt_prepreg <- unique(dt_prepreg)
    dt_poly <- unique(dt_poly)
    # merge prepregnancy data with polytherapy file
    dt <- merge(dt_prepreg, dt_poly, by = "pregnancy_id", all = FALSE, allow.cartesian = TRUE)
    
  }
  

  if (nrow(dt) > 0) {
    
    # Calculate intersection between overlap period and pregnancy period
    dt[, overlap_days_within_pregnancy := as.numeric(pmin(overlap_end, pregnancy_end_date) - pmax(overlap_start, pregnancy_start_date) + 1)]
    
    # If no overlap set overlap days to 0
    dt[overlap_days_within_pregnancy < 0, overlap_days_within_pregnancy := 0]
    
    # flag if at least 90 days overlap 
    dt[, overlap_3months := overlap_days_within_pregnancy >= 90]
    
    dt_subset <- dt[overlap_3months==TRUE,]
    
    if (nrow(dt_subset) > 0) {
      
      message("Prepregnancy users with overlap of 90 days or more during pregnancy period found")
      
      # assign year to count in
      dt_subset[, preg_year := year(pregnancy_start_date)]
      
      # keep one person per year
      if(deap_flags$is_EFEMERIS || deap_flags$is_FIN_REG) {
        dt_subset <- unique(dt_subset, by = c("pregnancy_id", "preg_year"))
      } else {
        dt_subset <- unique(dt_subset, by = c("person_id", "preg_year"))
      }
      # count by pregnancy
      poly_counts <- dt_subset[, .(N = .N), by = preg_year]
      
      # prepare denominator
      dt_counts_copy <- copy(dt_counts)
      dt_counts_copy[, c("n_total", "rate", "rate_computable") := NULL]
      setnames(dt_counts_copy, "n_treated", "n_total")
      # add all pre-pregnancy users across all exposures 
      dt_counts_all <- dt_counts_copy[, .(n_total = sum(n_total)), by = preg_year]
      
      # merge numerator and denominator
      poly_all <- merge(poly_counts, dt_counts_all, by = "preg_year", all.y = TRUE)
      poly_all[is.na(N), N := 0]
      
      # Calculate switcher as a rate (*1000)
      poly_all[, rate := round(1000 * N / n_total, 3)]
      poly_all[N == 0 & n_total == 0, rate := 0]
      
      # warnings
      if (nrow(poly_all[N > n_total]) > 0) warning(red("Warning: Numerator > Denominator"))
      if (nrow(poly_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator"))
      
      # save odd cases
      if (nrow(poly_all[N > n_total]) > 0) fwrite(poly_all[N > n_total], file.path(paths$D5_dir, "1.4_pregnancy_polytherapy", paste0(treatment, "_num_gt_denominator.csv")))
      if (nrow(poly_all[n_total == 0 & N != 0]) > 0) fwrite(poly_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.4_pregnancy_polytherapy", paste0(treatment, "_denominator_zero_numerator_nonzero.csv")))
      
      # Create column marking if rate is computable
      poly_all[, rate_computable := n_total > 0]
      
      # rename columns
      setnames(poly_all, "N", "n_treated")
      
      # save output
      saveRDS(dt_subset, file.path(paths$D4_dir, "1.4_pregnancy_polytherapy", paste0(pop_prefix, "_polytherapy_in_pregnancy_data.rds")))
      saveRDS(poly_all, file.path(paths$D5_dir, "1.4_pregnancy_polytherapy", paste0(pop_prefix, "_polytherapy_in_pregnancy_counts.rds")))
      
    } else {
      
      message(red("No prepregnancy users have an overlap of 90 days or more during pregnancy period"))
      
    }
    
  } else {
    
    message(red("No polytherapy records found"))
    
  }
  
} else {
  # One or more are empty
  message(red("No polytherapy episodes found"))
}



















