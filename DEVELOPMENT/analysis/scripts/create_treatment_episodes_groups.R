print("=========================================================================================")
print("========================= CREATING TREATMENT EPISODES SUBGROUPS =========================")
print("=========================================================================================")

# Target folders
target_folders <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# Create folder for treatment episodes
dir.create(file.path(paths$D3_dir, "tx_episodes", "groups"), recursive = TRUE, showWarnings = FALSE)

for (group in target_folders) {
  
  message("Processing group: ", pop_prefix ,"_",group)
  
  folder_path <- file.path(paths$D3_dir, "algorithm_input", group)
  
  # List all .rds files in the folder
  list_exposures <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
  
  list_exposures <- list_exposures[grepl(paste0("/", pop_prefix, "_"), list_exposures) & (pop_prefix != "PC" | !grepl("/PC_HOSP_", list_exposures))]

  # Read and combine all files into one data.table
  dt_combined <- rbindlist(lapply(list_exposures, readRDS), use.names = TRUE, fill = TRUE)
  
  if(nrow(dt_combined)>0){
  # Add assumed duration if needed
  # Set assumed duration if missing
  dt_combined[, assumed_duration := ifelse(is.na(presc_duration_days) | presc_duration_days < 30, 30, presc_duration_days)]
  
  # Add the atc_group column (if not retained)
  dt_combined[, atc_group := atc_group]
  
  
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
    
    # Add the atc_group column (if not retained)
    treat_episode[, atc_group := atc_group]
    
    # merge to get unique ATC
    treat_episode <- merge(
      treat_episode,
      dt_combined[, .(person_id, rx_date, code)],
      by.x = c("person_id", "episode.start"),
      by.y = c("person_id", "rx_date"),
      all.x = TRUE
    )
    # Merge with study population
    treat_episode <- merge(
      treat_episode,
      study_population[, .(person_id, sex_at_instance_creation, birth_date, start_follow_up, end_follow_up, entry_date, exit_date)],
      by = "person_id"
    )
    
    # Apply episode validity filters
    treat_episode <- treat_episode[episode.end > entry_date - 90]
    treat_episode[episode.end > end_follow_up, episode.end := end_follow_up]
    treat_episode <- treat_episode[episode.start < end_follow_up]
    treat_episode <- treat_episode[episode.end > episode.start]
    
    if(nrow(treat_episode)>0){
      # Save output
      saveRDS(treat_episode, file = file.path(paths$D3_dir, "tx_episodes", "groups", paste0(pop_prefix, "_", group, "_treatment_episode.rds")))
      
       }
  }
}
