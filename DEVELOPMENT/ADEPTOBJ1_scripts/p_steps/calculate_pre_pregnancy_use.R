###############################################################################################################################################################################
# <<< Sub-objective 1.3: Pre-pregnancy use rate >>>
# Measure: Annual pre-pregnancy ASM use rate
# Numerator: The number of unique pregnancies with >=1 treatment episode in the year prior to pregnancy, falling within both 12-6 months before pregnancy and 6-0 months before pregnancy.
# Denominator: Total number of unique pregnancies in that calendar year in the data source
# Stratification by: Overall, individual drug substance, drug sub-groups, calendar year, data source

###############################################################################################################################################################################

print("=====================================================================================")
print("========================= CALCULATING PRE-PREGNANCY ASM USE =========================")
print("=====================================================================================")

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
  
  # Get pre-pregnancy users 
  if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) dt_pre_pregnancy <- dt_overlap[any_12_6 == TRUE & any_6_0 == TRUE,]
  if( deap_flags$is_EFEMERIS ||  deap_flags$is_FIN_REG) dt_pre_pregnancy <- dt_overlap[any_before == TRUE,]
  
  # Perform counts
  if(nrow(dt_pre_pregnancy)>0){
    
    # Print Message
    message(paste0("There is pre-pregnancy use of ", overlap_name))
    
    # Get list of unique ids
    preg_ids_12_0 <- unique(dt_pre_pregnancy$pregnancy_id)
    
    # Count the number of pregnancies with ASM use in the 6-0 month and 12-6 month window, grouped by pregnancy year
    pre_pregnancy_counts <- pregnancies[pregnancy_id %in% preg_ids_12_0, .N, by = preg_year]
    
    # Merge with template to get all years
    pre_pregnancy_all <- merge(empty_dt[, .(preg_year)], pre_pregnancy_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    pre_pregnancy_all <- merge(pre_pregnancy_all, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    pre_pregnancy_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate rates
    pre_pregnancy_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Create column marking if rate is computable
    pre_pregnancy_all[, rate_computable := Freq > 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(pre_pregnancy_all[N > Freq]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
    if (nrow(pre_pregnancy_all[Freq == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
    
    # Save data where odd values
    if (nrow(pre_pregnancy_all[N > Freq]) > 0) fwrite(pre_pregnancy_all[N > Freq], file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", paste0(overlap_name, "_all_num_gt_denominator.csv")))
    if (nrow(pre_pregnancy_all[Freq == 0 & N != 0]) > 0) fwrite(pre_pregnancy_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", paste0(overlap_name, "_all_denominator_zero_numerator_nonzero.csv")))
    
    # Rename columns
    setnames(pre_pregnancy_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save files
    saveRDS(dt_pre_pregnancy, file = file.path(paths$D4_dir, "1.3_pre-pregnancy_use", paste0(overlap_name, "_pre_pregnancy_data.rds")))
    saveRDS(pre_pregnancy_all, file = file.path(paths$D5_dir, "1.3_pre-pregnancy_use", paste0(overlap_name, "_pre_pregnancy_counts.rds")))
    
  } else {
    
    # Print Message
    message(paste0("There is NO pre-pregnancy use of ", overlap_name))
    
  }
  
}