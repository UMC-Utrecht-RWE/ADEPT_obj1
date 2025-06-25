# Read bridge file into a data.table
altmeds <- as.data.table(read_excel(file.path(thisdir, "definitions", "bridge", "ADEPT_O1_BRIDGE_19Mayo25.xlsx")))[alternatives == TRUE,]

# Move each folder that matches an algorithm name
for (algo in unique(altmeds[["Varname"]])) {
  
  from <- file.path(paths$D3_dir, "algorithm_input", algo)
  
  to   <- file.path(paths$D3_dir, "alternatives", algo)
  
  if (dir.exists(from)) {
    file_move(from, to)
  } else {
    message(sprintf("Folder %s does not exist in algorithm_input.", algo))
  }
}

# List all alternative algorithm folders
altmed_folders <- list.dirs(file.path(paths$D3_dir, "alternatives"), recursive = FALSE, full.names = TRUE)

# Create folder for storing tx_episodes
dir.create(file.path(paths$D3_dir, "tx_episodes", "alternatives"), recursive = TRUE, showWarnings = FALSE)

# Loop through each algorithm folder
for (alg_folder in altmed_folders) {
  alg_name <- basename(alg_folder)
  cat("\n--- Processing alternative group:", alg_name, "---\n")
  
  # List all .rds files inside the folder
  files_in_folder <- list.files(alg_folder, pattern = "\\.rds$", full.names = TRUE, ignore.case = TRUE)
  
  # List to collect treatment episodes for this algorithm
  episode_list <- list()
  
  # Loop through each file in the folder
  for (f in files_in_folder) {
    dt <- readRDS(f)
    
    # Process each ATC code within the file
    for (current_code in unique(dt$code)) {
      dt_sub <- dt[code == current_code]
      if (nrow(dt_sub) == 0) next  # Skip if nothing
      
      dt_sub[, assumed_duration := fifelse(is.na(presc_duration_days), 30, presc_duration_days)]
      
      treat_episode <- compute.treatment.episodes(
        data = dt_sub,
        ID.colname = "person_id",
        event.date.colname = "rx_date",
        event.duration.colname = "assumed_duration",
        medication.class.colname = "code",
        carryover.within.obs.window = TRUE,
        carry.only.for.same.medication = TRUE,
        medication.change.means.new.treatment.episode = TRUE,
        maximum.permissible.gap = 30,
        maximum.permissible.gap.unit = "days",
        followup.window.duration = 365 * 12,
        followup.window.duration.unit = "days",
        return.data.table = TRUE
      )
      
      treat_episode[, `:=`(ATC = current_code, alternative_med = alg_name)]
      episode_list[[paste0(basename(f), "_", current_code)]] <- treat_episode
    }
  }
  
  # Combine all episodes across files + codes
  combined_episodes <- rbindlist(episode_list, use.names = TRUE, fill = TRUE)
  # merge with study population
  combined_episodes <- merge(combined_episodes, study_population[,c("person_id", "entry_date","exit_date", "start_follow_up", "end_follow_up")], by = "person_id")
  
  # # TODO
  # combined_episodes <- combined_episodes[episode.end > start_follow_up - 90,]
  # TODO 
  combined_episodes <- combined_episodes[episode.end > end_follow_up, episode.end:= end_follow_up]
  # TODO 
  combined_episodes <- combined_episodes[episode.start < end_follow_up,]
  # TODO 
  combined_episodes <- combined_episodes[episode.end > episode.start,]
  
  # Save combined result per algorithm
  saveRDS(
    combined_episodes,
    file = file.path(paths$D3_dir, "tx_episodes", "alternatives", paste0(alg_name, "_treatment_episode.rds"))
  )
}

