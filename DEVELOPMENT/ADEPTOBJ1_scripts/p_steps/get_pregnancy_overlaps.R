print("============================================================================================") 
print("========================= Data Prep for Pre-Pregnancy Calculations =========================")
print("============================================================================================")

if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  
  # Prepare data for pre-pregnancy, discontinued and continuous counts
  # List treatment episodes (female only)
  files_episodes <- list.files(path = paste0(paths$D3_dir, "/tx_episodes"), pattern = "_F_.*\\.rds$", full.names = TRUE)
  
  # Load pregnancies
  load(file.path(preg_dir, "D3_pregnancy_final.RData"))
  pregnancies <- as.data.table(D3_pregnancy_final)
  
  # Remove duplicates
  pregnancies <- unique(pregnancies) 
  
  # Convert pregnancy dates to IDate
  pregnancies[, pregnancy_start_date := as.IDate(pregnancy_start_date)][, pregnancy_end_date := as.IDate(pregnancy_end_date)] #convert to IDate
  
  # Drop columns you will not need 
  pregnancies <- as.data.table(pregnancies[,.(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, highest_quality)])
  
  # Merge pregnancies with study population to get start and end follow up. We want to keep only pregnancy starts within this period
  pregnancies <- merge(pregnancies, study_population[, .(person_id, start_follow_up, end_follow_up)], by = "person_id", allow.cartesian = TRUE)
  
  # Keep pregnancies with at least 1 year of lookback
  pregnancies <- pregnancies[pregnancy_start_date >= start_follow_up, ]
  
  # Keep pregnancies whose preg_start_date is before endfu
  pregnancies <- pregnancies[pregnancy_start_date < end_follow_up, ]
  
  # Censor pregnancy ends for pregnancies that continue after end fu
  setnames(pregnancies, "pregnancy_end_date", "pregnancy_end_date_original")
  pregnancies <- pregnancies[, pregnancy_end_date := pmin(pregnancy_end_date_original, end_follow_up) ]
  
  # Drop the start and end follow up columns as these will be available again when merged with treatment episodes
  pregnancies[, c("start_follow_up", "end_follow_up") := NULL]
  
  # Add windows
  pregnancies[, window_12_6_start := pregnancy_start_date - 365]
  pregnancies[, window_12_6_end   := pregnancy_start_date - 183]
  pregnancies[, window_6_0_start  := pregnancy_start_date - 182]
  pregnancies[, window_6_0_end    := pregnancy_start_date - 1]
  pregnancies[, window_t1_start   := pregnancy_start_date]
  pregnancies[, window_t1_end     := pmin(pregnancy_start_date + 90, pregnancy_end_date)]
  pregnancies[, window_t2_start   := fifelse(pregnancy_end_date >= pregnancy_start_date + 91, pregnancy_start_date + 91, as.IDate(NA))]
  pregnancies[, window_t2_end     := fifelse(!is.na(window_t2_start), pmin(pregnancy_start_date + 180, pregnancy_end_date), as.IDate(NA))]
  pregnancies[, window_t3_start   := fifelse(pregnancy_end_date >= pregnancy_start_date + 181, pregnancy_start_date + 181, as.IDate(NA))]
  pregnancies[, window_t3_end     := fifelse(!is.na(window_t3_start), pregnancy_end_date, as.IDate(NA))]
  pregnancies[, preg_year         := year(pregnancy_start_date)]
  
  # Calculate total pregnancies per year (denominator)
  total_preg_by_year <- pregnancies[, .(Freq = uniqueN(pregnancy_id)), by = preg_year]
  
  # Prepare for join - keep all records that fall between 12 months before pregnancy start date and pregnancy end date
  setkey(pregnancies, person_id, window_12_6_start, pregnancy_end_date)
  
  # Process each episode separately
  for (episode in seq_along(files_episodes)) {
    
    # Assign treatment name
    treat_name <- sub("_treatment_episode\\.rds$", "", basename(files_episodes[episode]))
    
    # Print Message
    message("Processing: ", treat_name)
    
    # Load episode 
    dt_episode <- as.data.table(readRDS(files_episodes[episode]))
    
    # Remove duplicates
    dt_episode <- unique(dt_episode, by = c("person_id", "episode.start", "atc_group"))
    
    # Convert episode dates to IDate
    dt_episode[, episode.start := as.IDate(episode.start)][, episode.end := as.IDate(episode.end)]
    
    # Prepare for join
    setkey(dt_episode, person_id, episode.start, episode.end)
    
    # Get all episodes that overlap from 12 months before pregnancy start to pregnancy end 
    dt_overlap <- foverlaps(
      dt_episode[,.(person_id, episode.start, episode.end, atc_group, code, start_follow_up, end_follow_up, birth_date)],
      pregnancies,
      by.x = c("person_id", "episode.start", "episode.end"),
      by.y = c("person_id", "window_12_6_start", "pregnancy_end_date"),
      type = "any",
      nomatch = 0L
    )

    # Check for overlaps 
    dt_overlap[, overlap_12_6 := episode.start <= window_12_6_end & episode.end  >= window_12_6_start]
    dt_overlap[, overlap_6_0  := episode.start <= window_6_0_end  & episode.end  >= window_6_0_start]
    dt_overlap[, overlap_t1   := episode.start <= window_t1_end   & episode.end  >= window_t1_start]
    dt_overlap[, overlap_t2   := !is.na(window_t2_start) & episode.start <= window_t2_end & episode.end   >= window_t2_start]
    dt_overlap[, overlap_t3   := !is.na(window_t3_start) & episode.start <= window_t3_end & episode.end   >= window_t3_start]
    
    # Overlaps per pregnancy id 
    dt_overlap_per_pregid <- dt_overlap[
      ,
      .(
        any_12_6 = any(overlap_12_6),
        any_6_0  = any(overlap_6_0),
        any_t1   = any(overlap_t1),
        any_t2   = any(overlap_t2),
        any_t3   = any(overlap_t3)
      ),
      by = .(pregnancy_id)
    ]
    
    # Merge back 
    dt_overlap <- merge(dt_overlap, dt_overlap_per_pregid, by="pregnancy_id")
    
    # Drop cols
    cols_to_drop <- grep("^(window|overlap)", names(dt_overlap), value = TRUE)
    dt_overlap[, (cols_to_drop) := NULL]
    
    # Save to temp folder 
    if(nrow(dt_overlap)>0){
      
      # Print message
      message("Patients with overlap found for ", treat_name)
      
      # Save file to temp folder 
      saveRDS(dt_overlap, file = file.path(paths$D3_dir, "tmp", paste0(treat_name, ".rds")))
      
    } else {
      
      # Print message
      message("No patients with overlap found for ", treat_name)
    }
  }
} else {
  
  # Prepare data for pre-pregnancy, discontinued and continuous counts
  # List treatment episodes (female only)
  files_episodes <- list.files(path = paste0(paths$D3_dir, "/tx_episodes"), full.names = TRUE)
  
  # Study population is the pregnancies
  pregnancies <- copy(study_population) # pregnancies and study population are the same
  pregnancies <- unique(pregnancies)   # Remove true duplicates
  
  # Convert pregnancy dates to IDate
  pregnancies[, pregnancy_start_date := as.IDate(pregnancy_start_date)][, pregnancy_end_date := as.IDate(pregnancy_end_date)]
  
  # Drop columns you will not need 
  pregnancies <- as.data.table(pregnancies[,.(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date, highest_quality, op_start_date, op_end_date)])
  
  # Add windows
  pregnancies[, window_before_preg_start := op_start_date]
  pregnancies[, window_before_preg_end   := pregnancy_start_date - 1]
  pregnancies[, window_t1_start          := pregnancy_start_date]
  pregnancies[, window_t1_end            := pmin(pregnancy_start_date + 90, pregnancy_end_date)]
  pregnancies[, window_t2_start          := fifelse(pregnancy_end_date >= pregnancy_start_date + 91, pregnancy_start_date + 91, as.IDate(NA))]
  pregnancies[, window_t2_end            := fifelse(!is.na(window_t2_start), pmin(pregnancy_start_date + 180, pregnancy_end_date), as.IDate(NA))]
  pregnancies[, window_t3_start          := fifelse(pregnancy_end_date >= pregnancy_start_date + 181, pregnancy_start_date + 181, as.IDate(NA))]
  pregnancies[, window_t3_end            := fifelse(!is.na(window_t3_start), pregnancy_end_date, as.IDate(NA))]
  pregnancies[, preg_year                := year(pregnancy_start_date)]
  
  # Calculate total pregnancies per year (denominator)
  total_preg_by_year <- pregnancies[, .(Freq = uniqueN(pregnancy_id)), by = preg_year]
  
  # Prepare for join - keep all records that fall between 12 months before pregnancy start date and pregnancy end date
  setkey(pregnancies, pregnancy_id, window_before_preg_start, pregnancy_end_date)
  
  # Process each episode separately
  for (episode in seq_along(files_episodes)) {
    
    # Assign treatment name
    treat_name <- sub("_treatment_episode\\.rds$", "", basename(files_episodes[episode]))
    
    # Print Message
    message("Processing: ", treat_name)
    
    # Load episode 
    dt_episode <- as.data.table(readRDS(files_episodes[episode]))
    
    # Remove duplicates - treatment episodes based on pregnancy id not person id
    dt_episode <- unique(dt_episode, by = c("pregnancy_id", "episode.start", "atc_group")) 
    
    # Convert episode dates to IDate
    dt_episode[, episode.start := as.IDate(episode.start)][, episode.end := as.IDate(episode.end)]
    
    # Prepare for join
    setkey(dt_episode, pregnancy_id, episode.start, episode.end)
    
    # Get all episodes that overlap from 12 months before pregnancy start to pregnancy end 
    dt_overlap <- foverlaps(
      dt_episode[,.(person_id, pregnancy_id, episode.start, episode.end, atc_group, code, op_start_date, op_end_date, birth_date)],
      pregnancies,
      by.x = c("pregnancy_id", "episode.start", "episode.end"),
      by.y = c("pregnancy_id", "window_before_preg_start", "pregnancy_end_date"),
      type = "any",
      nomatch = 0L
    )
    
    # Check for overlaps 
    dt_overlap[, overlap_before_pregnancy := episode.start <= window_before_preg_end & episode.end >= window_before_preg_start]
    dt_overlap[, overlap_t1               := episode.start <= window_t1_end   & episode.end  >= window_t1_start]
    dt_overlap[, overlap_t2               := !is.na(window_t2_start) & episode.start <= window_t2_end & episode.end   >= window_t2_start]
    dt_overlap[, overlap_t3               := !is.na(window_t3_start) & episode.start <= window_t3_end & episode.end   >= window_t3_start]
    
    # Overlaps per pregnancy id 
    dt_overlap_per_pregid <- dt_overlap[
      ,
      .(
        any_before = any(overlap_before_pregnancy),
        any_t1     = any(overlap_t1),
        any_t2     = any(overlap_t2),
        any_t3     = any(overlap_t3)
      ),
      by = .(pregnancy_id)
    ]
    
    # Merge back 
    dt_overlap <- merge(dt_overlap, dt_overlap_per_pregid, by="pregnancy_id")
    
    # Drop cols
    cols_to_drop <- grep("^(window|overlap)", names(dt_overlap), value = TRUE)
    dt_overlap[, (cols_to_drop) := NULL]
    
    # Save to temp folder 
    if(nrow(dt_overlap)>0){
      
      # Print message
      message("Patients with overlap found for ", treat_name)
      
      # Save file
      saveRDS(dt_overlap, file = file.path(paths$D3_dir, "tmp", paste0(treat_name, ".rds"))) 
      
    } else {
      
      # Print message
      message("No patients with overlap found for ", treat_name)
    }
  }
  
  
}

