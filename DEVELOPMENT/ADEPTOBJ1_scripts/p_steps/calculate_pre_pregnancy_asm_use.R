print("=====================================================================================")
print("========================= CALCULATING PRE-PREGNANCY ASM USE =========================")
print("=====================================================================================")

# List all treatment episode files matching population prefix
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = "\\.rds$")

# Keep only files that match population prefix AND contain "_F_" (female patients)
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes) & grepl("_F_", files_episodes)]

# Drop PC_HOSP files if pop_prefix is PC
if(pop_prefix == "PC") files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]

# Load pregnancies file
load(file.path(preg_dir, "D3_pregnancy_final.RData"))
pregnancies <- as.data.table(D3_pregnancy_final)

# Remove duplicates
pregnancies <- unique(pregnancies)

# Convert pregnancy dates to IDate
pregnancies[, pregnancy_start_date := as.IDate(pregnancy_start_date)]
pregnancies[, pregnancy_end_date   := as.IDate(pregnancy_end_date)]

# Add pre-pregnancy windows
pregnancies[, window_12_6_start := pregnancy_start_date - 365]
pregnancies[, window_12_6_end   := pregnancy_start_date - 183]
pregnancies[, window_6_0_start  := pregnancy_start_date - 182]
pregnancies[, window_6_0_end    := pregnancy_start_date - 1]

# Add pregnancy start year
pregnancies[, preg_year := year(pregnancy_start_date)]

# Set key for joining
setkey(pregnancies, person_id)

# Create vector of study years from your study dates (must exist in environment)
study_years <- seq(year(start_study_date), year(end_study_date))

# Create template table with all years zeroed
template_years <- data.table(preg_year = study_years)

# Calculate total pregnancies per year (denominator)
total_preg_by_year <- pregnancies[, .(Freq = uniqueN(pregnancy_id)), by = preg_year]

# Loop through each treatment episode file
for (episode in seq_along(files_episodes)) {
  
  # Get name of current ASM
  treatment_name <- gsub("_treatment_episode\\.rds$", "", files_episodes[episode])
  
  # Print Message
  message("Processing: ", treatment_name)
  
  # Load treatment episodes
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", "individual", files_episodes[episode]))
  
  # Convert episode dates to IDate
  dt[, episode.start := as.IDate(episode.start)][, episode.end := as.IDate(episode.end)]

  # Remove true duplicates
  dt <- unique(dt)
  
  # Set key for joining
  setkey(dt, person_id)
  
  # <<< 12-6 months window overlap >>> # 
  
  # Merge treatment episode with pregnancies file on person id
  dt_12_6 <- dt[pregnancies, on = .(person_id), nomatch = 0]
  
  # Filter for episodes that fall within the 12-6 month period before pregnancy start 
  dt_12_6 <- dt_12_6[episode.start <= window_12_6_end & episode.end >= window_12_6_start]
  
  # Get list of unique ids 
  preg_ids_12_6 <- unique(dt_12_6$pregnancy_id)
  
  # Check if any pre-pregnancy ASM use was found 
  if(nrow(dt_12_6)>0){
    
    # Count the number of pregnancies with ASM use in the 12–6 month window, grouped by pregnancy year
    pre_pregnancy_counts <- pregnancies[pregnancy_id %in% preg_ids_12_6, .N, by = preg_year]
    
    # Merge with template to get all years 
    pre_pregnancy_all <- merge(template_years[, .(preg_year)], pre_pregnancy_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    pre_pregnancy_all <- merge(pre_pregnancy_all, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    pre_pregnancy_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate rates
    pre_pregnancy_all[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
   
    # Create column marking if rate is computable 
    pre_pregnancy_all[, rate_computable := Freq > 0]

    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(pre_pregnancy_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(pre_pregnancy_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Save data where odd values 
    if(nrow(pre_pregnancy_all[N > Freq])>0) fwrite(pre_pregnancy_all[N > Freq], file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", treatment_name, "_12_6_num_gt_denominator.csv"))
    if(nrow(pre_pregnancy_all[Freq == 0 & N != 0])>0) fwrite(pre_pregnancy_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", treatment_name, "_12_6_denominator_zero_numerator_nonzero.csv"))
    
    # Rename columns 
    setnames(pre_pregnancy_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save files 
    saveRDS(dt_12_6, file = file.path(paths$D4_dir, "1.3_pre-pregnancy_use_rate", paste0(treatment_name, "_pre_pregnancy_use_rate_12_6_window_data.rds")))
    saveRDS(pre_pregnancy_all, file = file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", paste0(treatment_name, "_pre_pregnancy_use_rate_12_6_window_counts.rds")))
    
  } else {
    message(red(paste0("There was no pre-pregnancy use of ", treatment_name, " in the 12-6 month window")))
  }
  
  # <<< 6-0 months window overlap >>> # 
  
  # Merge treatment episode with pregnancies file on person id
  dt_6_0 <- dt[pregnancies, on = .(person_id), nomatch = 0]
  
  # Filter for episodes that fall within the 6-0 month period before pregnancy start 
  dt_6_0 <- dt_6_0[episode.start <= window_6_0_end & episode.end >= window_6_0_start]
  
  # Get list of unique ids 
  preg_ids_6_0 <- unique(dt_6_0$pregnancy_id)
  
  # Check if any pre-pregnancy ASM use was found 
  if(nrow(dt_6_0)>0){
    
    # Count the number of pregnancies with ASM use in the 6-0 month window, grouped by pregnancy year
    pre_pregnancy_counts <- pregnancies[pregnancy_id %in% preg_ids_6_0, .N, by = preg_year]
    
    # Merge with template to get all years 
    pre_pregnancy_all <- merge(template_years[, .(preg_year)], pre_pregnancy_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    pre_pregnancy_all <- merge(pre_pregnancy_all, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    pre_pregnancy_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate rates
    pre_pregnancy_all[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Create column marking if rate is computable 
    pre_pregnancy_all[, rate_computable := Freq > 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(pre_pregnancy_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(pre_pregnancy_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Save data where odd values 
    if(nrow(pre_pregnancy_all[N > Freq])>0) fwrite(pre_pregnancy_all[N > Freq], file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", treatment_name, "_6_0_num_gt_denominator.csv"))
    if(nrow(pre_pregnancy_all[Freq == 0 & N != 0])>0) fwrite(pre_pregnancy_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", treatment_name, "_6_0_denominator_zero_numerator_nonzero.csv"))
    
    # Rename columns 
    setnames(pre_pregnancy_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save files 
    saveRDS(dt_6_0, file = file.path(paths$D4_dir, "1.3_pre-pregnancy_use_rate", paste0(treatment_name, "_pre_pregnancy_use_rate_6_0_window_data.rds")))
    saveRDS(pre_pregnancy_all, file = file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", paste0(treatment_name, "_pre_pregnancy_use_rate_6_0_window_counts.rds")))
    
  } else {
    message(red(paste0("There was no pre-pregnancy use of ", treatment_name, " in the 6-0 month window")))
  }

  # <<< 12-0 window overlap >>> # 
  
  # Merge treatment episode with pregnancies file on person id
  dt_12_0 <- dt[pregnancies, on = .(person_id), nomatch = 0]
  
  # Filter for episodes that fall within the 12-0 month period before pregnancy start 
  dt_12_0 <- dt_12_0[episode.start <= window_6_0_end & episode.end >= window_12_6_start]
  
  # Get list of unique ids 
  preg_ids_12_0 <- unique(dt_12_0$pregnancy_id)
  
  # Check if any pre-pregnancy ASM use was found 
  if(nrow(dt_12_0)>0){
    
    # Count the number of pregnancies with ASM use in the 6-0 month window, grouped by pregnancy year
    pre_pregnancy_counts <- pregnancies[pregnancy_id %in% preg_ids_12_0, .N, by = preg_year]
    
    # Merge with template to get all years 
    pre_pregnancy_all <- merge(template_years[, .(preg_year)], pre_pregnancy_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    pre_pregnancy_all <- merge(pre_pregnancy_all, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    pre_pregnancy_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate rates
    pre_pregnancy_all[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Create column marking if rate is computable 
    pre_pregnancy_all[, rate_computable := Freq > 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(pre_pregnancy_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(pre_pregnancy_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Save data where odd values 
    if(nrow(pre_pregnancy_all[N > Freq])>0) fwrite(pre_pregnancy_all[N > Freq], file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", treatment_name, "_all_num_gt_denominator.csv"))
    if(nrow(pre_pregnancy_all[Freq == 0 & N != 0])>0) fwrite(pre_pregnancy_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", treatment_name, "_all_denominator_zero_numerator_nonzero.csv"))
    
    # Rename columns 
    setnames(pre_pregnancy_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save files 
    saveRDS(dt_12_0, file = file.path(paths$D4_dir, "1.3_pre-pregnancy_use_rate", paste0(treatment_name, "_pre_pregnancy_use_rate_12_0_window_data.rds")))
    saveRDS(pre_pregnancy_all, file = file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", paste0(treatment_name, "_pre_pregnancy_use_rate_12_0_window_counts.rds")))
    
  } else {
    message(red(paste0("There was no pre-pregnancy use of ", treatment_name, " in the 12-0 month window")))
  }  
}


