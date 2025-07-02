print("===============================================================================")
print("========================= CREATING TREATMENT EPISODES =========================")
print("===============================================================================")


# Get list of all datasets in exposure folder
list_exposures <- list.files(file.path(paths$D3_dir, "exposure"))
# For each one, create treatment episodes and save in treatment episodes folder with the same name + suffix treatment_episode
# Create folder for treatment episodes
dir.create(file.path(paths$D3_dir, "tx_episodes", "individual"), recursive = TRUE, showWarnings = FALSE)


for (exposure in list_exposures) {
  
  # Extract ATC group from file name: remove prefix and .rds
  atc_group <- gsub(paste0("^", pop_prefix, "_"), "", gsub("\\.rds$", "", exposure))
  
  # Read the current file
  dt <- readRDS(file.path(paths$D3_dir, "exposure", exposure))
  
  # Set assumed duration if missing
  dt[, assumed_duration := ifelse(is.na(presc_duration_days) | presc_duration_days < 30, 30, presc_duration_days)]
  
  # Add atc_group column
  dt[, atc_group := atc_group]
  
  # Print message
  message("Processing: ", gsub(".rds", "", exposure))
  
  # Create treatment episodes for the entire group
  treat_episode <- compute.treatment.episodes(
    data = dt,
    ID.colname = "person_id",
    event.date.colname = "rx_date",
    event.duration.colname = "assumed_duration",
    medication.class.colname = "atc_group",  
    carryover.within.obs.window = TRUE,
    carry.only.for.same.medication = TRUE,
    consider.dosage.change = FALSE,
    medication.change.means.new.treatment.episode = TRUE,
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
    parallel.threads = "auto",
    suppress.warnings = FALSE,
    return.data.table = TRUE
  )
  
  # Add the atc_group column (if not retained)
  treat_episode[, atc_group := atc_group]
  
  # merge to get unique ATC
  treat_episode <- merge(
    treat_episode,
    dt[, .(person_id, rx_date, code)],
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
  
  if(nrow(treat_episode)>0) {
    # Save output
    saveRDS(treat_episode, file = file.path(paths$D3_dir, "tx_episodes", "individual", paste0(gsub("\\.rds$", "", exposure), "_treatment_episode.rds")))
  }
}


