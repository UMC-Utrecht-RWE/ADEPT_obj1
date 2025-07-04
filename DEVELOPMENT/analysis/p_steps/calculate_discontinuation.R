print("===============================================================================")
print("========================= CALCULATING DISCONTINUATION =========================")
print("===============================================================================")

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]}

# Load prevalence files
files_prevalence_counts <- list.files(file.path(paths$D5_dir, "1.1_prevalence"), pattern = "\\.rds$")

# Filter prevalence counts for current pop_prefix only
files_prevalence_counts <- files_prevalence_counts[grepl(paste0("^", pop_prefix, "_"), files_prevalence_counts)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_prevalence_counts <- files_prevalence_counts[!grepl("PC_HOSP", files_prevalence_counts)]}

# Loop through each treatment episode file
for (episode in seq_along(files_episodes)) {
  
  # Read the treatment episode file
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", "individual", files_episodes[episode]))
  
  # Print Message
  message("Processing: ", gsub("_treatment_episode\\.rds$", "", files_episodes[episode]))
  
  # Order episodes by person & start date
  setorder(dt, person_id, episode.start)
  
  # Get next episode start date
  dt[, next_start := shift(episode.start, type = "lead"), by = .(person_id)]
  
  # Convert all dates to IDate
  dt[, c("episode.start", "episode.end", "exit_date", "next_start") := lapply(.SD, as.IDate), .SDcols = c("exit_date", "episode.start", "episode.end", "next_start")]
  
  # Flag discontinuation events
  dt[, discontinuer_flag := fifelse(is.na(next_start), (exit_date - episode.end > 120), (next_start - episode.end > 120))]
  
  # Keep only the discontinued episodes
  discontinuers <- dt[discontinuer_flag == TRUE]
  
  # Keep only episodes between entry and exit date
  discontinuers <- discontinuers[episode.end >= start_follow_up & episode.end <= end_follow_up,]
  
  if(nrow(discontinuers)>0){
    
    # Assign calendar year of each discontinued episode
    discontinuers[, year := year(episode.end)]
    
    # Remove duplicates: Keep only one person id per year
    discontinuers <- unique(discontinuers, by = c("person_id", "year"))
    
    # Count number of discontinuers per year
    discontinued_counts <- discontinuers[, .("N" = .N), by = year]
    
    # Match corresponding prevalence file
    matched_prevalence_file <- files_prevalence_counts[gsub("_prevalence\\.rds$", "", files_prevalence_counts) == gsub("_treatment_episode\\.rds$", "", files_episodes[episode])]
    
    if (length(matched_prevalence_file) == 1) {
      
      # Read in Prevalence file if file found 
      prev_counts <- readRDS(file.path(paths$D5_dir, "1.1_prevalence", matched_prevalence_file))
      
      # Prepare prevalence counts
      prev_counts[,c("n_total", "rate", "rate_computable") := NULL]
      setnames(prev_counts, "n_treated", "n_total")
      
      # Merge discontinued with prevalence
      discontinued_all <- merge(discontinued_counts, prev_counts, by = "year", all.y = TRUE)
      
      # Set N = 0 for years with no treatments
      discontinued_all[is.na(N), N := 0]
      
      # Calculate discontinued as a rate (*100)
      discontinued_all[, rate := round(100 * N / n_total, 3)][N == 0 & n_total == 0, rate := 0]
      
      # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
      if (nrow(discontinued_all[N > n_total]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
      if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
      
      # Save data where odd values 
      if(nrow(discontinued_all[N > n_total])>0) fwrite(discontinued_all[N > n_total], file.path(paths$D5_dir, "1.2_discontinued", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_num_gt_denominator.csv")))
      if(nrow(discontinued_all[n_total == 0 & N != 0])>0) fwrite(discontinued_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.2_discontinued", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_denominator_zero_numerator_nonzero.csv")))
      
      # Create column marking if rate is computable 
      discontinued_all[, rate_computable := !(n_total == 0 & N > 0)]
      
      # rename columns
      setnames(discontinued_all, "N", "n_treated")
      
      # Save dataset 
      saveRDS(discontinuers, file.path(paths$D4_dir, "1.2_discontinued", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_discontinued_data.rds")))
      
      # Save results 
      saveRDS(discontinued_all, file.path(paths$D5_dir, "1.2_discontinued", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_discontinued.rds")))
      
    } else {
      
      warning("No matching prevalence file found for: ", gsub("_treatment_episode\\.rds$", "", files_episodes[episode]))
      
    }
  } else {
    
    message(red(paste("No discontinuers were found for:", gsub("_treatment_episode\\.rds$", "", files_episodes[episode]))))
    
  }
}
