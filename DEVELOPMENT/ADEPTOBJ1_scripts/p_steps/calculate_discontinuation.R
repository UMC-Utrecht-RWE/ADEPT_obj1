###############################################################################################################################################################################
# <<< Sub-objective 1.2: Discontinuation rate >>> 
# Measure: Annual discontinuation rate of ASM
# Numerator: Number of individuals who have a gap of ≥120 days between treatment episodes of an ASM in each calendar year
# Denominator: The number of prevalent ASM users in that calendar year in the data source 
# Stratification by: Individual drug substance, drug sub-groups, age groups, calendar year, data source

###############################################################################################################################################################################

print("===============================================================================")
print("========================= CALCULATING DISCONTINUATION =========================")
print("===============================================================================")

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes"), pattern = "\\.rds$")

if(!deap_flags$is_EFEMERIS){
  
  files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)] # only current pop_prefix
  if(pop_prefix=="PC") files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)] # BIFAP
  # Prevalence files
  files_prevalence_counts <- list.files(file.path(paths$D5_dir, "1.1_prevalence"), pattern = "\\.rds$")
  files_prevalence_counts <- files_prevalence_counts[grepl(paste0("^", pop_prefix, "_"), files_prevalence_counts)]# only current pop_prefix
  if(pop_prefix=="PC") files_prevalence_counts <- files_prevalence_counts[!grepl("PC_HOSP", files_prevalence_counts)] #BIFAP
}

# Loop through each treatment episode file
for (episode in seq_along(files_episodes)) {
  
  episode_name <- gsub("_treatment_episode\\.rds$", "", files_episodes[episode])
  
  # Read the treatment episode file
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", files_episodes[episode]))
  
  # Print Message
  message("Processing: ", episode_name)
  
  if(!deap_flags$is_EFEMERIS){
    
    # Remove duplicates
    dt <- unique(dt, by = c("person_id", "episode.start", "episode.end", "start_follow_up", "end_follow_up"))
    
    # Order episodes by person & start date
    setorder(dt, person_id, start_follow_up, episode.start)
    
    # Get next episode start date
    dt[, next_start := shift(episode.start, type = "lead"), by = .(person_id, start_follow_up)]
    
    # Flag discontinuation events
    dt[, discontinuer_flag := fifelse(is.na(next_start), (exit_date - episode.end >= 120), (next_start - episode.end >= 120))]
    
    # Keep only the discontinued episodes
    discontinuers <- dt[discontinuer_flag == TRUE,]
    
    # Keep only episodes between entry and exit date
    discontinuers <- discontinuers[episode.end >= start_follow_up & episode.end <= end_follow_up,]  
    
    # Print message if no discontinuers found
    if (nrow(discontinuers) == 0) {
      message(red(paste("No discontinuers were found for:", episode_name)))
      next
    }
    
    # Save discontinued data if present
    if (nrow(discontinuers) > 0) saveRDS(discontinuers, file.path(paths$D4_dir, "1.2_discontinued", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_discontinued_data.rds")))
      
  } else {
    
    # Remove duplicates
    dt <- unique(dt, by = c("pregnancy_id", "episode.start", "episode.end"))
    
    # Order episodes by person & start date
    setorder(dt, pregnancy_id,  episode.start)
    
    # Get next episode start date
    dt[, next_start := shift(episode.start, type = "lead"), by = .(pregnancy_id)]
    
    # Flag discontinuation events
    dt[, discontinuer_flag := fifelse(is.na(next_start), (end_follow_up - episode.end >= 120), (next_start - episode.end >= 120))]
    
    # Keep only the discontinued episodes
    discontinuers <- dt[discontinuer_flag == TRUE,]
    
    # Keep only episodes between entry and exit date
    discontinuers <- discontinuers[episode.end >= start_follow_up & episode.end <= end_follow_up,]
    
    # Print message if no discontinuers found
    if (nrow(discontinuers) == 0) {
      message(red(paste("No discontinuers were found for:", episode_name)))
      next
      }
    
    # Save discontinued data if present
    if (nrow(discontinuers) > 0) saveRDS(discontinuers, file.path(paths$D4_dir, "1.2_discontinued", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_discontinued_data.rds")))
    next
  }
  
  # For DEAPS participating in Obj 1.1, 1.2
    # Assign calendar year of each discontinued episode
    discontinuers[, year := year(episode.end)]
    
    # Remove duplicates: Keep only one person id per year
    discontinuers <- unique(discontinuers, by = c("person_id", "year"))
    
    # Count number of discontinuers per year
    discontinued_counts <- discontinuers[, .("N" = .N), by = year]
    
    # Match corresponding prevalence file
    matched_prevalence_file <- files_prevalence_counts[gsub("_prevalence_counts\\.rds$", "", files_prevalence_counts) == gsub("_treatment_episode\\.rds$", "", files_episodes[episode])]
    
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
      if (nrow(discontinued_all[N > n_total]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
      if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
      
      # Save data where odd values 
      if(nrow(discontinued_all[N > n_total])>0) fwrite(discontinued_all[N > n_total], file.path(paths$D5_dir, "1.2_discontinued", paste0(episode_name, "_num_gt_denominator.csv")))
      if(nrow(discontinued_all[n_total == 0 & N != 0])>0) fwrite(discontinued_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.2_discontinued", paste0(episode_name, "_denominator_zero_numerator_nonzero.csv")))
      
      # Create column marking if rate is computable 
      discontinued_all[, rate_computable := n_total > 0]
      
      # rename columns
      setnames(discontinued_all, "N", "n_treated")
      # Save results 
      saveRDS(discontinued_all, file.path(paths$D5_dir, "1.2_discontinued", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_discontinued_counts.rds")))
      
    } else {
      
      warning("No matching prevalence file found for: ", gsub("_treatment_episode\\.rds$", "", files_episodes[episode]))
      
    }
  } 
