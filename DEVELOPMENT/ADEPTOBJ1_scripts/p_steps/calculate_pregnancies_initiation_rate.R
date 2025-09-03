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

# List all treatment episodes
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes"), pattern = "\\.rds$")

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  
  files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes) & grepl("_F_", files_episodes)] # Picks only Female records
  if(pop_prefix == "PC") files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)] # BIFAP
  
  # Pregnancy file loaded and cleaned in pre-pregnancy script
  
  # Loop through each treatment episode file
  for (episode in seq_along(files_episodes)) {
    
    # Get name of current ASM
    treatment_name <- gsub("_treatment_episode\\.rds$", "", files_episodes[episode])
    
    # Load treatment episodes
    dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", files_episodes[episode]))
    
    # Convert episode dates to IDate
    dt[, episode.start := as.IDate(episode.start)][, episode.end := as.IDate(episode.end)]
    
    # Remove duplicates
    dt <- unique(dt, by = c("person_id", "episode.start", "episode.end","start_follow_up"))
    
    # Set key for joining
    setkey(dt, person_id)
    
    # Merge treatment episode with pregnancies file on person id
    dt_all <- dt[pregnancies, on = .(person_id), nomatch = 0, allow.cartesian = TRUE]
    
    # First, sort by person and episode.start to ensure order
    setorder(dt_all, person_id, start_follow_up, episode.start)
    
    # Create a new column with the previous episode.end per person
    dt_all[, prior_episode_end := shift(episode.end), by = .(person_id, start_follow_up)]
    
    # Keep pregnancies where prior treatment episode end is more than 365 days before current episode start or there is no prior episode 
    # Convert lookback_period (lubridate Period) to integer days
    lookback_days <- as.integer((Sys.Date() + lookback_period) - Sys.Date())
    dt_all <- dt_all[((pregnancy_start_date - prior_episode_end) > lookback_days) | (is.na(prior_episode_end))]
    
    # Keep pregnancies only if start is between start and end follow up
    dt_all <- dt_all[pregnancy_start_date >= start_follow_up & pregnancy_start_date <= end_follow_up, ]
    
    # Keep episodes only if they start after pregnancy
    dt_all <- dt_all[episode.start >= pregnancy_start_date & episode.start <= pregnancy_end_date, ]
    
    # Get list of unique ids 
    preg_ids_all <- unique(dt_all$pregnancy_id)
    
    # Check if any pre-pregnancy ASM use was found 
    if(nrow(dt_all)>0){
      
      message(paste0("Found ASM initiation of ", treatment_name))
      
      # Count the number of pregnancies with ASM use in the third trimester, grouped by pregnancy year
      initiation_rate_counts <- pregnancies[pregnancy_id %in% preg_ids_all, .N, by = preg_year]
      
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
      if(nrow(initiation_rate_all[N > Freq])>0) fwrite(initiation_rate_all[N > Freq], file.path(paths$D5_dir, "1.3_initiation_rate_during_pregnancy", treatment_name, "_num_gt_denominator.csv"))
      if(nrow(initiation_rate_all[Freq == 0 & N != 0])>0) fwrite(initiation_rate_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_initiation_rate_during_pregnancy", treatment_name, "_denominator_zero_numerator_nonzero.csv"))
      
      # Rename columns 
      setnames(initiation_rate_all, c("N", "Freq"), c("n_treated", "n_total"))
      
      # Save files 
      saveRDS(dt_all, file = file.path(paths$D4_dir, "1.3_pregnancy_initiation", paste0(treatment_name, "_initiation_rates_in_pregnancy_data.rds")))
      saveRDS(initiation_rate_all, file = file.path(paths$D5_dir, "1.3_pregnancy_initiation", paste0(treatment_name, "_initiation_rates_in_pregnancy_counts.rds")))
      
    } else {
      message(red(paste0("There was no ASM initiation of ", treatment_name)))
    } 
  }
  
} else {

  # Loop through each treatment episode file
  for (episode in seq_along(files_episodes)) {
    
    # Get name of current ASM
    treatment_name <- gsub("_treatment_episode\\.rds$", "", files_episodes[episode])
    
    # Load treatment episodes
    dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", files_episodes[episode]))
    
    # Convert episode dates to IDate
    dt[, episode.start := as.IDate(episode.start)][, episode.end := as.IDate(episode.end)]
    dt[, pregnancy_start_date := as.IDate(pregnancy_start_date)][, pregnancy_end_date := as.IDate(pregnancy_end_date)]
    
    # Remove duplicates
    dt <- unique(dt, by = c("pregnancy_id", "episode.start", "episode.end"))
    
    # Sort by pregnancy id and episode.start to ensure order
    setorder(dt, pregnancy_id, episode.start)
    
    # Criteria: Treatment episode needs to start in pregnancy, and there can be no other treatment episode in the lookback period
    # Create a new column with the previous episode.end per pregnancy
    dt[, prior_episode_end := shift(episode.end), by = .(pregnancy_id)]
    
    # Keep only if no episode (end) in the lookback period  
    dt <- dt[((pregnancy_start_date - prior_episode_end) > lookback_period) | (is.na(prior_episode_end))]
    
    # Keep pregnancies only if start is between start and end follow up
    dt <- dt[pregnancy_start_date >= start_follow_up & pregnancy_start_date <= end_follow_up, ]
    
    # Keep episodes only if they start after pregnancy
    dt <- dt[episode.start >= pregnancy_start_date & episode.start <= pregnancy_end_date, ]
    
    # Get list of unique ids
    preg_ids_all <- unique(dt$pregnancy_id)
    
    # Check if any pre-pregnancy ASM use was found
    if(nrow(dt)>0){
      
      message(paste0("Found ASM initiation of ", treatment_name))
      
      # Count the number of pregnancies with ASM use in the third trimester, grouped by pregnancy year
      initiation_rate_counts <- pregnancies[pregnancy_id %in% preg_ids_all, .N, by = preg_year]
      
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
      if(nrow(initiation_rate_all[N > Freq])>0) fwrite(initiation_rate_all[N > Freq], file.path(paths$D5_dir, "1.3_initiation_rate_during_pregnancy", treatment_name, "_num_gt_denominator.csv"))
      if(nrow(initiation_rate_all[Freq == 0 & N != 0])>0) fwrite(initiation_rate_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_initiation_rate_during_pregnancy", treatment_name, "_denominator_zero_numerator_nonzero.csv"))
      
      # Rename columns
      setnames(initiation_rate_all, c("N", "Freq"), c("n_treated", "n_total"))
      
      # Save files
      saveRDS(dt_all, file = file.path(paths$D4_dir, "1.3_pregnancy_initiation", paste0(treatment_name, "_initiation_rates_in_pregnancy_data.rds")))
      saveRDS(initiation_rate_all, file = file.path(paths$D5_dir, "1.3_pregnancy_initiation", paste0(treatment_name, "_initiation_rates_in_pregnancy_counts.rds")))
      
    } else {
      message(red(paste0("There was no ASM initiation of ", treatment_name)))
    }
  }
}