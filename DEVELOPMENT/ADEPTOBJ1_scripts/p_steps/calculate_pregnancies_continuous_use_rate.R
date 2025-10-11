###############################################################################################################################################################################
# <<< Sub-objective 1.3: Continuous use rate >>> 
# Measure: Annual continuous rate of ASM use during pregnancy
# Numerator: The number of pre-pregnancy users of an ASM within a calendar year that also runs into the first, second and third trimester of pregnancy 
# Denominator: Total number of pregnancies in that calendar year in the data source
# Stratification by: Individual drug substance, drug sub-groups, indication, calendar year, data source

###############################################################################################################################################################################

print("===============================================================================================")
print("========================= CALCULATING CONTINUOUS USE DURING PREGNANCY =========================")
print("===============================================================================================")

# List all episodes of pre pregnancy use 
files_episodes <- list.files(file.path(paths$D4_dir, "1.3_pre-pregnancy_use"))

# Loop through each treatment episode file
for (episode in seq_along(files_episodes)) {
  
  # Get name of current ASM
  treatment_name <- gsub("_pre_pregnancy_data\\.rds$", "", files_episodes[episode])
  
  # Print Message
  message("Processing: ", treatment_name)
  
  # Load treatment episodes
  dt <- readRDS(file.path(paths$D4_dir, "1.3_pre-pregnancy_use", files_episodes[episode]))
  
  # Remove true duplicates
  dt <- unique(dt)
  
  # Convert episode dates to IDate
  dt[, episode.start := as.IDate(episode.start)][, episode.end := as.IDate(episode.end)]
  dt[, pregnancy_start_date := as.IDate(pregnancy_start_date)][, pregnancy_end_date := as.IDate(pregnancy_end_date)]
  
  # Add trimester windows
  dt[, t1_start := pregnancy_start_date]
  dt[, t1_end   := pmin(pregnancy_start_date + 90, pregnancy_end_date)]
  dt[, t2_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 91, pregnancy_start_date + 91, as.IDate(NA))]
  dt[, t2_end   := fifelse(!is.na(t2_start), pmin(pregnancy_start_date + 180, pregnancy_end_date), as.IDate(NA))]
  dt[, t3_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 181, pregnancy_start_date + 181, as.IDate(NA))]
  dt[, t3_end   := fifelse(!is.na(t3_start), pregnancy_end_date, as.IDate(NA))]
  
  # create flags for all trimesters
  # T1 overlap (always exists if pregnancy_start_date is valid)
  dt[, overlap_t1 := fifelse(!is.na(t1_start) & !is.na(t1_end) & episode.start <= t1_end & episode.end >= t1_start, TRUE, FALSE)]
  # T2 overlap (only check if t2_start and t2_end are not NA)
  dt[, overlap_t2 := fifelse(!is.na(t2_start) & !is.na(t2_end) & episode.start <= t2_end & episode.end >= t2_start, TRUE, FALSE)]
  # T3 overlap (only check if t3_start and t3_end are not NA)
  dt[, overlap_t3 := fifelse(!is.na(t3_start) & !is.na(t3_end) & episode.start <= t3_end & episode.end >= t3_start, TRUE, FALSE)]
  
  # Flag if episode overlaps **all three trimesters**
  dt_all_trimester_overlap <- dt[overlap_t1 & overlap_t2 & overlap_t3]
  
  # Get list of unique ids 
  preg_ids_allt <- unique(dt_all_trimester_overlap$pregnancy_id)
  
  # Check if any continuous use was found
  if(nrow(dt_all_trimester_overlap)>0){
    
    # Count the number of pregnancies with ASM use in first trimester, grouped by pregnancy year
    continuous_rate_counts <- pregnancies[pregnancy_id %in% preg_ids_allt, .N, by = preg_year]
    
    # Merge with template to get all years 
    continuous_rate_all <- merge(empty_dt[, .(preg_year)], continuous_rate_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    continuous_rate_all <- merge(continuous_rate_all, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    continuous_rate_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate rates
    continuous_rate_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Create column marking if rate is computable 
    continuous_rate_all[, rate_computable := Freq > 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(continuous_rate_all[N > Freq]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
    if (nrow(continuous_rate_all[Freq == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
    
    # Save data where odd values 
    if (nrow(continuous_rate_all[N > Freq]) > 0) fwrite(continuous_rate_all[N > Freq], file.path(paths$D5_dir, "1.3_pregnancy_continuous", paste0(treatment_name, "_t1_num_gt_denominator.csv")))
    if (nrow(continuous_rate_all[Freq == 0 & N != 0]) > 0) fwrite(continuous_rate_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_pregnancy_continuous", paste0(treatment_name, "_t1_denominator_zero_numerator_nonzero.csv")))
    
    # Rename columns 
    setnames(continuous_rate_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save files 
    saveRDS(dt_all_trimester_overlap, file = file.path(paths$D4_dir, "1.3_pregnancy_continuous", paste0(treatment_name, "_continuous_use_rate_in_pregnancy_data.rds")))
    saveRDS(continuous_rate_all, file = file.path(paths$D5_dir, "1.3_pregnancy_continuous", paste0(treatment_name, "_continuous_use_rate_in_pregnancy_counts.rds")))
    
  } else {
    
    message(red(paste0("There was no continuous use of ", treatment_name)))
    
  }
}

