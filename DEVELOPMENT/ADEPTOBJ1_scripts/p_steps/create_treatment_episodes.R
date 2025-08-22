#################################################################
# Create Treatment Episodes - Individual Medicines 
#################################################################

print("===============================================================================")
print("========================= CREATING TREATMENT EPISODES =========================")
print("===============================================================================")

# List all files in exposure folder
files_exposures <- list.files(file.path(paths$D3_dir, "exposure"))

# Filter exposures for current pop_prefix only
files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_exposures <- files_exposures[!grepl("PC_HOSP", files_exposures)]

# For each one, create treatment episodes and save in treatment episodes folder with the same name + suffix treatment_episode
for (exposure in seq_along(files_exposures)) {
  
  # Extract ATC group from file name: remove prefix and .rds
  # Remove prefix and either '_algo_med.rds' or '.rds'
  atc_group <- gsub(paste0("^", pop_prefix, "_"), "", gsub("_algo_med\\.rds$|\\.rds$", "", files_exposures[exposure]))
  
  # Print message
  message("Processing: ", paste0(pop_prefix, "_", atc_group))
  
  # Read the current file
  dt <- readRDS(file.path(paths$D3_dir, "exposure", files_exposures[exposure]))
  
  # Only keep records between entry and exit dates 
  dt <- dt[rx_date >= entry_date & rx_date <= exit_date]
  
  # Get assumed durations per DAP 
  source(file.path(thisdir, "p_steps", "calculate_DAP_specific_assumed_durations.R"), local = TRUE) 
  
  # If assumed duration >100, then change it to median 
  med_val <- median(dt$assumed_duration, na.rm = TRUE)
  dt[assumed_duration > 100, assumed_duration := med_val]
  
  # Add atc_group column
  dt[, atc_group := atc_group]
  
  # save the file back so we have assumed duration
  saveRDS(dt, file.path(paths$D3_dir, "exposure", files_exposures[exposure]))
  
  # Remove true duplicates
  dt <- unique(dt, by = c("person_id", "atc_group", "rx_date"))
  
  if(nrow(dt)>0){
    # Create Treatment Episode
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
    
    # Add the atc_group column to treatment episode
    treat_episode[, atc_group := atc_group]
    
    # Merge with dt to get unique ATC
    treat_episode <- merge(treat_episode, dt[, .(person_id, rx_date, code)], by.x = c("person_id", "episode.start"), by.y = c("person_id", "rx_date"), all.x = TRUE, allow.cartesian = TRUE)
    
    # Merge with study population to get start, end follow ups, entry/exit dates
    treat_episode <- merge(treat_episode, study_population[, .(person_id, sex_at_instance_creation, birth_date, start_follow_up, end_follow_up, entry_date, exit_date)], by = "person_id", allow.cartesian = TRUE)
    
    # Convert date columns to IDate
    treat_episode[, `:=`(episode.start = as.IDate(episode.start), episode.end = as.IDate(episode.end))]
    
      # Apply episode validity filters
      treat_episode <- treat_episode[episode.end > entry_date - 90,] 
      treat_episode <- treat_episode[episode.start < exit_date,] 
      treat_episode[episode.end > exit_date, episode.end := exit_date] 
      treat_episode <- treat_episode[episode.end > episode.start,] 
    
    # Remove duplicates
    treat_episode <- unique(treat_episode)
    
    # Save output if treatment episode has at least one row
    if(nrow(treat_episode)>0) saveRDS(treat_episode, file = file.path(paths$D3_dir, "tx_episodes", paste0(pop_prefix, "_", atc_group, "_treatment_episode.rds")))
  }
}

