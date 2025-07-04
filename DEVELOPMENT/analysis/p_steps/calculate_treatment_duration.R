print("=========================================================================")
print("========================= TREATMENT DURATION ============================")
print("=========================================================================")

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]}

# Create empty list to store overall stats from all files
all_overall_stats <- list()

for (episode in seq_along(files_episodes)) {
  
  # Read the treatment episode file
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", "individual", files_episodes[episode]))
  
  # Extract short name from file, e.g. DP_AMINOBUTYRIC
  episode_name <- gsub(paste0("^", pop_prefix, "_"), "", files_episodes[episode])
  episode_name <- gsub("_treatment_episode\\.rds$", "", episode_name)
  
  # Print Message
  message("Processing: ", episode_name)
  
  # Convert episode start and end to date type (IDate)
  dt[, episode.start := as.IDate(episode.start)]
  dt[, episode.end := as.IDate(episode.end)]
  
  # If episode.end is before startfu then delete
  # If episode.start is before start_fu, change episode.start to start_fu
  # If episode.end is after end_fu, change episode.end to end_fu
  dt <- dt[!(episode.end<start_follow_up),]
  dt <-dt[episode.start<start_follow_up, episode.start := start_follow_up]
  dt <-dt[episode.end > end_follow_up, episode.end := end_follow_up]
  
  if(nrow(dt)>0){
    
    # Create columns with just the year part of start and end dates
    dt[, year_start := year(episode.start)]
    dt[, year_end := year(episode.end)]
    
    # If episode.end is before startfu then delete
    # If episode.start is before start_fu, change episode.start to start_fu
    # If episode.end is after end_fu, change episode.end to end_fu
    dt <- dt[!(episode.end<start_follow_up),]
    dt <-dt[episode.start<start_follow_up, episode.start := start_follow_up]
    dt <-dt[episode.end > end_follow_up, episode.end := end_follow_up]
    
    # Expand each row into multiple rows, one for each year spanned by the episode
    dt_expanded <- dt[ , .(year = seq(year_start, year_end)), by = .(person_id, episode.start, episode.end)]
    
    # Merge back all the other columns from dt so no info is lost
    dt_expanded <- merge(dt_expanded, dt, by = c("person_id", "episode.start", "episode.end"), all.x = TRUE)
    
    # Convert to IDate
    dt_expanded[, c("start_follow_up", "end_follow_up", "episode.start", "episode.end") := lapply(.SD, as.IDate), .SDcols = c("start_follow_up", "end_follow_up", "episode.start", "episode.end")]
    
    
    # Calculate, for each expanded row, the start date in that calendar year (max of episode start and Jan 1 of the year)
    dt_expanded[, treatment_start := as.IDate(pmax(episode.start, as.IDate(paste0(year, "-01-01")), start_follow_up))]
    
    # Calculate the end date in that calendar year (min of episode end and Dec 31 of the year)
    dt_expanded[, treatment_end   := as.IDate(pmin(episode.end, as.IDate(paste0(year, "-12-31")), end_follow_up))]
    
    # Calculate how many days fall within that calendar year interval (inclusive)
    dt_expanded[, duration_in_year := as.numeric(treatment_end - treatment_start) + 1]  
    
    neg_durations <- dt_expanded[duration_in_year < 0]
    
    # Sanity Check!!!
    if (nrow(neg_durations) > 0) {
      warning("Negative duration_in_year found in these rows:")
      print(neg_durations[, .(person_id, episode.start, episode.end, year, treatment_start, treatment_end, duration_in_year)])
    }
    
    # Sum total treatment months per person per year
    summary_by_person_year <- dt_expanded[, .(total_days = sum(duration_in_year)), by = .(person_id, year)]
    
    # Stats by Year
    stats_by_year <- summary_by_person_year[, .(
      mean_days = mean(total_days),
      median_days = median(total_days),
      min_days = min(total_days),
      max_days = max(total_days),
      iqr_days = IQR(total_days),
      sd_days = sd(total_days)
    ), by = year]
    
    # Overall Stats
    overall_stats <- summary_by_person_year[, .(
      mean_days = mean(total_days),
      median_days = median(total_days),
      min_days = min(total_days),
      max_days = max(total_days),
      iqr_days = IQR(total_days),
      sd_days = sd(total_days)
    )]
    
    # Add episode name column
    overall_stats <- cbind(exposure = episode_name, overall_stats)
    
    # Collect overall_stats
    all_overall_stats[[episode]] <- overall_stats
    
    # Save stats_by_year for each exposure
    saveRDS(stats_by_year, file.path(paths$D5_dir, "1.2_treatment_duration", paste0(pop_prefix, "_", episode_name, "_treatment_duration_by_year.rds")))
    
    saveRDS(dt_expanded, file.path(paths$D4_dir, "1.2_treatment_duration", paste0(pop_prefix, "_", episode_name, "_treatment_duration_by_year_data.rds")))
    
  } else { 
    
    # Print Message
    message("No Treatment Duration can be calculated for: ", episode_name)
  }
}

if(length(all_overall_stats) > 0) {
  # Combine all overall stats into one table
  combined_overall_stats <- rbindlist(all_overall_stats, use.names = TRUE, fill = TRUE)
  # Save combined overall stats table
  saveRDS(combined_overall_stats, file.path(paths$D5_dir, "1.2_treatment_duration", paste0(pop_prefix, "_overall_treatment_duration.rds")))
  
}


