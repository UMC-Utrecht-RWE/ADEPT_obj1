###############################################################################################################################################################################
# <<< Sub-objective 1.2: Treatment duration >>> 
# Measure: Annual treatment duration mean & median of ASM
# Calculation: Mean, median, minimum, maximum, interquartile range, and SD (in months) of treatment episodes for all individuals with ≥1 treatment episode of an ASM within a calendar year in the data source
# Stratification by: Individual drugs, calendar year, data source

###############################################################################################################################################################################

print("=========================================================================")
print("========================= TREATMENT DURATION ============================")
print("=========================================================================")

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix == "PC") files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]

# Prepare list to collect per-drug summaries
all_drug_stats <- list()

# Loop over each file
for (episode in seq_along(files_episodes)) {
  
  # Read the treatment episode file
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", files_episodes[episode]))
  
  # Remove duplicates
  dt <- unique(dt, by = c("person_id", "episode.start", "episode.end"))
  
  # Extract drug name from file name
  episode_name <- gsub(paste0("^", pop_prefix, "_"), "", files_episodes[episode])
  episode_name <- gsub("_treatment_episode\\.rds$", "", episode_name)
  
  message("Processing: ", pop_prefix, "_", episode_name)
  
  # Convert dates to IDate
  dt[, episode.start := as.IDate(episode.start)]
  dt[, episode.end   := as.IDate(episode.end)]
  
  # Filter and trim to follow-up period
  dt <- dt[!(episode.end < start_follow_up | episode.start > end_follow_up),]
  dt[episode.start < start_follow_up, episode.start := start_follow_up]
  dt[episode.end > end_follow_up, episode.end := end_follow_up]
  dt <- dt[episode.end >= episode.start,]  # drop invalid records

  if (nrow(dt) > 0) {
    
    # Calculate episode duration
    dt[, total_months := (as.numeric(episode.end - episode.start) + 1) / 30.44]
    
    # Calculate overall treatment stats
    overall_stats <- dt[, {
      q <- quantile(total_months, probs = c(0.25, 0.75), na.rm = TRUE)
      .(
        drug           = episode_name,
        n_persons      = uniqueN(person_id),
        mean_months    = mean(total_months, na.rm = TRUE),
        median_months  = median(total_months, na.rm = TRUE),
        min_months     = min(total_months, na.rm = TRUE),
        max_months     = max(total_months, na.rm = TRUE),
        iqr_months     = IQR(total_months, na.rm = TRUE),
        p25_months     = q[1],
        p75_months     = q[2],
        sd_months      = sd(total_months, na.rm = TRUE)
      )
    }]
    
    # Append to summary list
    all_drug_stats[[episode_name]] <- overall_stats
    
    # Save files 
    saveRDS(dt, file.path(paths$D4_dir, "1.2_treatment_duration", paste0(pop_prefix, "_", episode_name, "_treatment_duration_months.rds")))
    
  } else {
    message(red(paste0("No Treatment Duration can be calculated for: ", pop_prefix, "_", episode_name)))
  }
}

# After loop: bind all stats into one table
final_summary <- rbindlist(all_drug_stats)

# Save combined summary
saveRDS(final_summary, file.path(paths$D5_dir, "1.2_treatment_duration", paste0(pop_prefix, "_treatment_duration_months.rds")))

