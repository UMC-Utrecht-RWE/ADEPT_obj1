# Get list of all datasets in exposure folder
list_exposures <- list.files(file.path(paths$D3_dir, "exposure"))
# For each one, create treatment episodes and save in treatment episodes folder with the same name + suffix treatment_episode
# Create folder for treatment episodes
dir.create(file.path(paths$D3_dir, "tx_episodes", "exposures"), recursive = TRUE, showWarnings = FALSE)

for(exposure in list_exposures){

  # read in current file
  dt <- readRDS(file.path(paths$D3_dir, "exposure", paste0(exposure)))
  
  # Prepare empty list to collect treatment episodes by code
  episode_list <- list()
  
  for (current_code in unique(dt$code)) {
    
    # Print message
    cat(paste("Creating Treatment Episode for:", gsub(".rds", "", exposure), " - ATC:", current_code), "\n")
    
    # If more than one code in dataset, create treatment episode for each code 
    dt_sub <- dt[code == current_code]
    
    # Set duration if missing
    dt_sub[,assumed_duration:= ifelse(is.na(presc_duration_days), 30, presc_duration_days)]

    # Creates treatment episodes
    treat_episode<-compute.treatment.episodes(data= dt_sub,
                                                 ID.colname = "person_id",
                                                 event.date.colname = "rx_date",
                                                 event.duration.colname = "assumed_duration",
                                                 event.daily.dose.colname = NA,
                                                 medication.class.colname = "code",
                                                 carryover.within.obs.window = TRUE,
                                                 carry.only.for.same.medication = TRUE,
                                                 consider.dosage.change =FALSE,
                                                 #change between retinoids counts as a new treatment episode
                                                 medication.change.means.new.treatment.episode = TRUE,
                                                 dosage.change.means.new.treatment.episode = FALSE,
                                                 maximum.permissible.gap = 30,
                                                 maximum.permissible.gap.unit = c("days", "weeks", "months", "years", "percent")[1],
                                                 maximum.permissible.gap.append.to.episode = FALSE,
                                                 followup.window.start = 0,
                                                 followup.window.start.unit = c("days", "weeks", "months", "years")[1],
                                                 followup.window.duration = 365 * 12,
                                                 followup.window.duration.unit = c("days", "weeks", "months", "years")[1],
                                                 event.interval.colname = "event.interval",
                                                 gap.days.colname = "gap.days",
                                                 date.format = "%Y-%m-%d",
                                                 parallel.backend = c("none", "multicore", "snow", "snow(SOCK)", "snow(MPI)",
                                                                      "snow(NWS)")[1],
                                                 parallel.threads = "auto",
                                                 suppress.warnings = FALSE,
                                                 return.data.table = TRUE
    ) 
    # Add column ATC code 
    treat_episode <- treat_episode[, ATC:=current_code]

    # Store in list
    episode_list[[current_code]] <- treat_episode
  }
  
  # Combine all codes into one data.table for this exposure
  combined_episodes <- rbindlist(episode_list, use.names = TRUE, fill = TRUE)
  # merge with study population
  combined_episodes <- merge(combined_episodes, study_population[,c("person_id", "sex_at_instance_creation", "birth_date", "entry_date","exit_date", "start_follow_up", "end_follow_up")], by = "person_id")
  # # TODO
  # combined_episodes <- combined_episodes[episode.end > start_follow_up - 90,]
  # TODO 
  combined_episodes <- combined_episodes[episode.end > end_follow_up, episode.end:= end_follow_up]
  # TODO 
  combined_episodes <- combined_episodes[episode.start < end_follow_up,]
  # TODO 
  combined_episodes <- combined_episodes[episode.end > episode.start,]
   
  # Save using original exposure name + suffix
  saveRDS(combined_episodes, file = file.path(paths$D3_dir, "tx_episodes","exposures" , paste0(gsub(".rds", "", exposure), "_treatment_episode.rds")))
  
}
