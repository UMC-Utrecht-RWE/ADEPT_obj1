###############################################################################################################################################################################
# <<< Sub-objective 1.3: Initiation rate during pregnancy >>>
# Measure: Annual initiation rate of ASM during pregnancy
# Numerator: Number of pregnancies in a calendar year with ≥1 treatment episode of an ASM during any trimester, but no treatment episode in the 12 months prior to pregnancy start
# Denominator: Total number of pregnancies in that calendar year in the data source
# Stratification by: Overall, individual drug substance, drug sub-groups, age groups, indication, calendar year, data source

###############################################################################################################################################################################

print("==========================================================================================")
print("==================== CALCULATING ASM INITIATION RATE DURING PREGNANCY ====================")
print("==========================================================================================")

# Create vector of study years from study dates (exist in environment)
study_years <- seq(year(as.IDate(as.Date(start_study_date) + lookback_period)), year(as.IDate(end_study_date)))

# Create template table with all years zeroed
empty_dt <- data.table(preg_year = study_years)

# Calculate total pregnancies per year (denominator)
total_preg_by_year <- pregnancies[, .(Freq = uniqueN(pregnancy_id)), by = preg_year]

# List overlaps
files_overlaps  <- list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE)

# For each overlapping dataset, look for pre-pregnancy users (any12_6 and any 6_0 should both be TRUE)
for (overlap in seq_along(files_overlaps)) {
  
  # Assign overlap name
  overlap_name <- prefix <- sub("\\.rds$", "", basename(files_overlaps[overlap]))
  
  # Load episode 
  dt_overlap <- as.data.table(readRDS(files_overlaps[overlap]))
  
  # Get incident users during pregnancy 
  if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) dt_incident <- dt_overlap[((any_12_6 == FALSE & any_6_0 == FALSE) & (any_t1 == TRUE | any_t2 == TRUE| any_t3 == TRUE)),]
  if( deap_flags$is_EFEMERIS ||  deap_flags$is_FIN_REG) dt_incident <- dt_overlap[(any_before == FALSE & (any_t1 == TRUE | any_t2 == TRUE| any_t3 == TRUE)),]
  
  if (nrow(dt_incident) > 0) {
    
    # Print message
    message(paste0("Found ASM initiation of ", overlap_name))
    
    # Get list of unique ids
    preg_ids <- unique(dt_incident$pregnancy_id)
    
    # Count the number of pregnancies grouped by pregnancy year
    initiation_rate_counts <- pregnancies[pregnancy_id %in% preg_ids, .N, by = preg_year]
    
    # Merge with template to get all years
    initiation_rate_all <- merge(empty_dt[, .(preg_year)], initiation_rate_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    initiation_rate_all <- merge(initiation_rate_all, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    initiation_rate_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate rates
    initiation_rate_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Create column marking if rate is computable
    initiation_rate_all[, rate_computable := Freq > 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(initiation_rate_all[N > Freq]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
    if (nrow(initiation_rate_all[Freq == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
    
    # Save data where odd values
    if (nrow(initiation_rate_all[N > Freq]) > 0) fwrite(initiation_rate_all[N > Freq], file.path(paths$D5_dir, "1.3_initiation_rate_during_pregnancy", overlap_name, "_num_gt_denominator.csv"))
    if (nrow(initiation_rate_all[Freq == 0 & N != 0]) > 0) fwrite(initiation_rate_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_initiation_rate_during_pregnancy", overlap_name, "_denominator_zero_numerator_nonzero.csv"))
    
    # Rename columns
    setnames(initiation_rate_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save files
    saveRDS(dt_incident, file = file.path(paths$D4_dir, "1.3_pregnancy_initiation", paste0(overlap_name, "_initiation_rates_in_pregnancy_data.rds")))
    saveRDS(initiation_rate_all, file = file.path(paths$D5_dir, "1.3_pregnancy_initiation", paste0(overlap_name, "_initiation_rates_in_pregnancy_counts.rds")))
    
  } else {
    
    # Print Message 
    message(red(paste0("There was no ASM initiation of ", overlap_name)))
    
  }  
}