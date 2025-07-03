print("===============================================================================")
print("========================= CALCULATE SWITCHING =================================")
print("===============================================================================")

### Load All Required Data Sets ###

### List Discontinued Exposure Treatment Episodes
discontinued_episode_files <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), full.names = TRUE)

# Your exclude patterns
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# List all files with full paths
discontinued_episode_files <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), full.names = TRUE)

# Logical vector: TRUE if filename contains any of the exclude patterns
to_exclude <- grepl(paste(exclude, collapse = "|"), basename(discontinued_episode_files))

# Keep only files NOT matching exclude patterns
discontinued_episode_files <- discontinued_episode_files[!to_exclude]

# Keep only files that start with the correct prefix
discontinued_episode_files <- discontinued_episode_files[grepl(paste0("^", pop_prefix, "_"), basename(discontinued_episode_files))]

# If population = PC, exclude any PC_HOSP files
if(pop_prefix == "PC"){discontinued_episode_files<-discontinued_episode_files[!grepl("PC_HOSP", discontinued_episode_files)]}


if(length(discontinued_episode_files)>0){
  
  ### List Exposure Prescriptions
  exposure_meds_files <- list.files(file.path(paths$D3_dir, "exposure"), full.names = TRUE)
  
  # Keep only files starting with exact pop_prefix followed by an underscore
  exposure_meds_files <- exposure_meds_files[grepl(paste0("^", pop_prefix, "_"), basename(exposure_meds_files))]
  
  # If population = PC, exclude any PC_HOSP files
  if(pop_prefix=="PC"){exposure_meds_files<-exposure_meds_files[!grepl("PC_HOSP", exposure_meds_files)]}
  
  # List Alternative Medication Prescriptions - bind by group
  altmed_folders <- list.files(file.path(paths$D3_dir, "alternatives"), full.names = TRUE)
  
  ### Denominator
  prevalence_count_files <- list.files(file.path(paths$D5_dir, "1.1_prevalence"), full.names = TRUE)
  
  # Only those corresponding to pop_prefix
  prevalence_count_files <- prevalence_count_files[grepl(paste0("^", pop_prefix, "_"), basename(prevalence_count_files))]
  
  # If population = PC, exclude any PC_HOSP files
  if(pop_prefix == "PC"){prevalence_count_files <- prevalence_count_files[!grepl("PC_HOSP", prevalence_count_files)]}
  
  # Exclude all files whose *basename* contains any of the excluded terms
  prevalence_count_files <- prevalence_count_files[!grepl(paste(exclude, collapse = "|"), basename(prevalence_count_files))]
  
  # Extract prefix from prevalence filenames (before '_prevalence.rds')
  prevalence_prefixes <- sub("_prevalence\\.rds$", "", basename(prevalence_count_files))
  
  
  ### Compare Exposure to Exposure
  for (epi in seq_along(discontinued_episode_files)){
    
    # Read in episode
    dt_discontinued <- as.data.table(readRDS(discontinued_episode_files[epi]))
    
    # Drop unnecessary columns
    dt_discontinued[,c("episode.ID", "end.episode.gap.days", "episode.duration", "next_start", "discontinuer_flag", "year"):= NULL]
    
    # Create window start and window end columns - period where switcher could be found
    dt_discontinued[, window_start := episode.end][, window_end := episode.end + 120]
    
    # Set on key for faster searches
    setkey(dt_discontinued, person_id, window_start, window_end)
    
    ### Discontinued to Exposure
    for (exp in 1:length(exposure_meds_files)){
      
      message("Checking: ",
              gsub("_discontinued_data\\.rds$", "", basename(discontinued_episode_files[epi])),
              " and ",
              paste0(pop_prefix, "_", gsub(paste0("^", pop_prefix, "_"), "", gsub("\\.rds$", "", basename(exposure_meds_files[exp])))))
      
      # Read in data
      dt_exp <- as.data.table(readRDS(exposure_meds_files[exp]))
      
      # Keep only needed columns
      dt_exp <- dt_exp[, .(person_id, code, Varname, rx_date)]
      
      # Create window start and window end columns
      dt_exp[, window_start := rx_date][, window_end := rx_date]
      
      # Set on key for faster searches
      setkey(dt_exp, person_id, window_start, window_end)
      
      # Find overlaps within 120 days after discontinuation
      switchers <- foverlaps(dt_exp, dt_discontinued, type = "within", nomatch = 0)
      
      # Remove results where codes of discontinued episode matches rx that falls in window
      switchers <- switchers[code != i.code]
      
      # Drop columns window_start and window_end for the prescription as we have rx_date
      switchers[, c("i.window_start", "i.window_end"):=NULL]
      
      # Rename columns
      setnames(switchers, c("i.code", "Varname"), c("code_switched_to", "atc_group_switched_to"))
      
      if (nrow(switchers)>0){
        # Save file
        saveRDS(switchers, file = file.path(paths$D3_dir, "tmp", paste0(pop_prefix, "_", unique(switchers$atc_group)[1], "_to_", unique(switchers$atc_group_switched_to)[1], ".rds")))
      } else {
        # Print Message
        message(red(paste0(
          "No Switches between: ",
          gsub("_discontinued_data\\.rds$", "", basename(discontinued_episode_files[epi])),
          " and ",
          paste0(pop_prefix, "_", gsub(paste0("^", pop_prefix, "_"), "", gsub("\\.rds$", "", basename(exposure_meds_files[exp]))))
        )))
        
      }
    }
    
    ### Exposure to Alternative Meds
    for (altmed in seq_along(altmed_folders)) {
      
      # List all .rds files in the folder
      altmed_files <- list.files(altmed_folders[altmed], pattern = "\\.rds$", full.names = TRUE)
      
      # Keep only files starting with correct prefix
      altmed_files <- altmed_files[grepl(paste0("^", pop_prefix, "_"), basename(altmed_files))]
      
      # If population is PC, explicitly exclude PC_HOSP
      if (pop_prefix == "PC") {
        altmed_files <- altmed_files[!grepl("PC_HOSP", altmed_files)]
      }
      
      # Skip folder if no matching files
      if (length(altmed_files) == 0) next
      
      # Bind all matching altmed files in this folder
      dt_altmed <- rbindlist(
        lapply(altmed_files, function(f) as.data.table(readRDS(f))),
        use.names = TRUE, fill = TRUE
      )
      
      # Use the folder name and prefix to construct alt_group name
      alt_group_name <- paste0(pop_prefix, "_", basename(altmed_folders[altmed]))
      dt_altmed[, alt_group := alt_group_name]
      
      # Keep only needed columns
      dt_altmed <- dt_altmed[, .(person_id, code, alt_group, rx_date)]
      
      # Create window start and window end columns
      dt_altmed[, window_start := rx_date][, window_end := rx_date]
      
      # Set on key for faster searches
      setkey(dt_altmed, person_id, window_start, window_end)
      
      # Find overlaps within 120 days after discontinuation
      switchers <- foverlaps(dt_altmed, dt_discontinued, type = "within", nomatch = 0)
      
      # Remove results where codes of discontinued episode matches rx that falls in window
      switchers <- switchers[code != i.code]
      
      # Drop columns window_start and window_end for the prescription as we have rx_date
      switchers[, c("i.window_start", "i.window_end") := NULL]
      
      # Rename columns
      setnames(switchers, c("i.code", "alt_group"), c("code_switched_to", "atc_group_switched_to"))
      
      if (nrow(switchers) > 0) {
        # Save file
        saveRDS(switchers, file = file.path(paths$D3_dir, "tmp", paste0(pop_prefix, "_", unique(switchers$atc_group)[1], "_to_", unique(switchers$atc_group_switched_to)[1], ".rds")))
        
      } else {
        # Print Message
        message(red(paste0("No Switchers between: ", gsub("_discontinued_data\\.rds$", "", basename(discontinued_episode_files[epi])), " and ", alt_group_name)))
      }
    }
  }
  
  
  ### Counts
  
  # List all .rds files in the folder
  all_switchers <- list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE)
  all_switchers <- all_switchers[grepl(paste0("^", pop_prefix, "_"), basename(all_switchers))]
  # If population = PC, exclude any PC_HOSP files
  if(pop_prefix == "PC"){all_switchers<-all_switchers[!grepl("PC_HOSP", all_switchers)]}
  
  # Extract prefix for each file (full vector, one per file)
  all_prefixes <- sub("_to_.*", "", basename(all_switchers))
  
  # Unique prefixes
  unique_prefixes <- unique(all_prefixes)
  
  for (pfx in unique_prefixes) {
    
    # Read all and bind
    switchers <- rbindlist(lapply(all_switchers[all_prefixes == pfx], function(f) as.data.table(readRDS(f))), use.names = TRUE, fill = TRUE)
    
    # Make sure switchers are between start and end follow up
    switchers <- switchers[rx_date >= start_follow_up & rx_date <= end_follow_up,]
    
    if(nrow(switchers)>0){
      
      # Assign calendar year of each switch
      switchers[, year := year(rx_date)]
      
      # Count number of unique treated persons per year (numerator for prevalence)
      switcher_counts <- switchers[, .("N" = .N), by = year]
      
      # Find matching prevalence file index
      idx <- which(prevalence_prefixes == pfx)
      
      if (nrow(switchers) > 0) {
        
        # Assign calendar year of each switch
        switchers[, year := year(rx_date)]
        
        # Count number of unique treated persons per year (numerator for prevalence)
        switcher_counts <- switchers[, .("N" = .N), by = year]
        
        # Find matching prevalence file index
        idx <- which(prevalence_prefixes == pfx)
        
        if (length(idx) == 1) {
          # Load and process prevalence, calculate rates as before...
          
          prev_counts <- as.data.table(readRDS(prevalence_count_files[idx]))
          prev_counts[, c("n_total", "rate", "rate_computable") := NULL]
          setnames(prev_counts, "n_treated", "n_total")
          
          switchers_all <- merge(switcher_counts, prev_counts, by = "year", all.y = TRUE)
          switchers_all[is.na(N), N := 0]
          switchers_all[, rate := round(100 * N / n_total, 3)][N == 0 & n_total == 0, rate := 0]
      
          # Save files
          saveRDS(switchers, file.path(paths$D4_dir, "1.2_switching", paste0(pfx, "_switchers_data.rds")))
          saveRDS(switchers_all, file.path(paths$D5_dir, "1.2_switching", paste0(pfx, "_switched.rds")))
          
        } else if (length(idx) == 0) {
          
          warning(paste("Switcher data found but no prevalence file for prefix:", pfx))
          
        }
      }
      
    }
  }
  
} else {
  
  message(red(paste("There are no discontinued files to check for switching")))
}
