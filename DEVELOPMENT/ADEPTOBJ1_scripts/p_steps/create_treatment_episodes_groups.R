print("======================================================================================")
print("========================= CREATING TREATMENT EPISODES GROUPS =========================")
print("======================================================================================")

# Create folder for group treatment episodes
dir.create(file.path(paths$D3_dir, "tx_episodes", "groups"), recursive = TRUE, showWarnings = FALSE)

# Group folders
group_folders <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")


for (group in seq_along(group_folders)) {
  
  message("Processing group: ", pop_prefix ,"_", group_folders[group])
  
  # List all files in exposure folder
  files_exposures <- list.files(file.path(paths$D3_dir, "algorithm_input", group_folders[group]), pattern = "\\.rds$")
  
  # Filter exposures for current pop_prefix only
  files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)]
  
  # If pop_prefix is PC, then drop any that are PC_HOSP
  if(pop_prefix=="PC"){files_exposures <- files_exposures[!grepl("PC_HOSP", files_exposures)]}
  
  # For each folder read in files and bind them 
  dt_combined <- rbindlist(lapply(files_exposures, function(f) readRDS(file.path(file.path(paths$D3_dir, "algorithm_input", group_folders[group]), f))), use.names = TRUE, fill = TRUE)
  
  # Remove duplicates
  dt_combined <- unique(dt_combined)
  
  # If there is at least one row of data
  if(nrow(dt_combined)>0){
    
    # Set assumed duration if missing: If duration is missing or is less than 30, then make it 30 
    dt_combined[, assumed_duration := ifelse(is.na(presc_duration_days) | presc_duration_days < 30, 30, presc_duration_days)]
    
    # Add the atc_group column 
    dt_combined[, atc_group := group_folders[group]]
    
    # Create treatment episodes (NO carry-over)
    treat_episode <- compute.treatment.episodes(
      data = dt_combined,
      ID.colname = "person_id",
      event.date.colname = "rx_date",
      event.duration.colname = "assumed_duration",
      medication.class.colname = "atc_group",
      carryover.within.obs.window = FALSE,
      carry.only.for.same.medication = FALSE,
      consider.dosage.change = FALSE,
      medication.change.means.new.treatment.episode = FALSE,
      dosage.change.means.new.treatment.episode = FALSE,
      maximum.permissible.gap = 30,
      maximum.permissible.gap.unit = "days",
      maximum.permissible.gap.append.to.episode = FALSE,
      followup.window.start = 0,
      followup.window.start.unit = "days",
      followup.window.duration = 365 * 27,
      followup.window.duration.unit = "days",
      event.interval.colname = "event.interval",
      gap.days.colname = "gap.days",
      date.format = "%Y-%m-%d",
      parallel.backend = "none",
      return.data.table = TRUE
    )
    
    # Add the atc_group column to treatment episode 
    treat_episode[, atc_group := group_folders[group]]
    
    # Merge with dt to get unique ATC
    treat_episode <- merge(treat_episode, dt_combined[, .(person_id, rx_date, code)], by.x = c("person_id", "episode.start"), by.y = c("person_id", "rx_date"), all.x = TRUE)
    
    # Merge with study population to get start/end follow up and entry/exit dates, birthdates
    treat_episode <- merge(treat_episode, study_population[, .(person_id, sex_at_instance_creation, birth_date, start_follow_up, end_follow_up, entry_date, exit_date)], by = "person_id")
    
    # Apply episode validity filters
    treat_episode <- treat_episode[episode.end > entry_date - 90]
    treat_episode[episode.end > end_follow_up, episode.end := end_follow_up]
    treat_episode <- treat_episode[episode.start < end_follow_up]
    treat_episode <- treat_episode[episode.end > episode.start]
    
    # Remove duplicates
    treat_episode <- unique(treat_episode)
    
    # Save output if treatment episode has at least 1 row
    if(nrow(treat_episode)>0) saveRDS(treat_episode, file = file.path(paths$D3_dir, "tx_episodes", "groups", paste0(pop_prefix, "_", group_folders[group], "_treatment_episode.rds")))
  }
}
