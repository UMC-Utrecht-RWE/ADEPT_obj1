###############################################################################################################################################################################
# <<< Sub-objective 1.2: Switching rate >>> 
# Measure: Annual switching rate from one ASM to another ASM or to an alternative medication
# Numerator: Total number of individuals who have ≥1 treatment episode for a specific ASM and discontinued, with ≥1 treatment episode for a different ASM or alternative medication, during the last treatment episode of the ASM or within the discontinuation period of the ASM in each calendar year
# Denominator: The number of prevalent ASM users in that calendar year in the data source 
# Stratification by: Individual drug substance, calendar year, data source

###############################################################################################################################################################################

print("===============================================================================")
print("========================= CALCULATE SWITCHING =================================")
print("===============================================================================")

# List subgroups to exclude
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# Load Data Sets
# Discontinued episodes
files_discontinued_episodes <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), pattern = "\\.rds$")
# Filter episodes for current pop_prefix only
files_discontinued_episodes <- files_discontinued_episodes[grepl(paste0("^", pop_prefix, "_"), files_discontinued_episodes)]
# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_discontinued_episodes <- files_discontinued_episodes[!grepl("PC_HOSP", files_discontinued_episodes)]
# Exclude subgroups
files_discontinued_episodes <- files_discontinued_episodes[!(gsub(paste0("^", pop_prefix, "_|_discontinued_data\\.rds$"), "", files_discontinued_episodes)) %in% exclude]

# Exposure Meds
files_exposures <- list.files(file.path(paths$D3_dir, "exposure"))
# Filter exposures for current pop_prefix only
files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)]
# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_exposures <- files_exposures[!grepl("PC_HOSP", files_exposures)]
# Exclude subgroups
files_exposures <- files_exposures[!(gsub(paste0("^", pop_prefix, "_|_algo_med\\.rds$|\\.rds$"), "", files_exposures) %in% exclude)]

# Alternative Meds
files_altmeds <- list.files(file.path(paths$D4_dir, "1.2_altmeds"))
# Filter exposures for current pop_prefix only
files_altmeds <- files_altmeds[grepl(paste0("^", pop_prefix, "_"), files_altmeds)]
# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_altmeds <- files_altmeds[!grepl("PC_HOSP", files_altmeds)]

# Prevalence counts 
files_prevalence_counts <- list.files(file.path(paths$D5_dir, "1.1_prevalence"), pattern = "\\.rds$")
# Filter prevalence counts for current pop_prefix only
files_prevalence_counts <- files_prevalence_counts[grepl(paste0("^", pop_prefix, "_"), files_prevalence_counts)]
# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_prevalence_counts <- files_prevalence_counts[!grepl("PC_HOSP", files_prevalence_counts)]
# Exclude subgroups
files_prevalence_counts <- files_prevalence_counts[!(gsub(paste0("^", pop_prefix, "_|_prevalence_counts\\.rds$"), "", files_prevalence_counts) %in% exclude)]


for(episode in seq_along(files_discontinued_episodes)){
  
  # Get name of episode
  discontinued_episode_name <- gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode])
  
  # Read in Episode 
  dt_discontinued <- readRDS(file.path(paths$D4_dir, "1.2_discontinued", files_discontinued_episodes[episode]))
  
  # Drop unnecessary columns
  dt_discontinued[,c("episode.ID", "end.episode.gap.days", "episode.duration", "next_start", "discontinuer_flag", "year"):= NULL]
  
  # Create window start and window end columns - period where switcher could be found
  dt_discontinued[, window_start := pmax(episode.start, episode.end - 92)][, window_end := episode.end + 120]
  
  # Comparing Discontinued Episodes to Exposure prescriptions
  for(exposure in seq_along(files_exposures)){
    
    # Get name of exposure drug
    exposure_name <- gsub("\\.rds$", "", files_exposures[exposure])
    
    if(discontinued_episode_name == exposure_name) next
    
    # Load exposure prescriptions
    dt_exposures <- as.data.table(readRDS(file.path(paths$D3_dir, "exposure", files_exposures[exposure])))
    
    # Remove duplicates
    dt_exposures <- unique(dt_exposures, by = c("person_id", "code", "rx_date"))

    # Keep needed cols only 
    dt_exposures <- dt_exposures[, .(person_id, code, Varname, rx_date)]
    
    # Create window start and window end columns
    dt_exposures[, window_start := rx_date][, window_end := rx_date]
    
    # Set on keys
    setkey(dt_exposures, person_id, window_start, window_end)
    setkey(dt_discontinued, person_id, window_start, window_end)
    
    # Find overlaps within 120 days after discontinuation
    switchers <- foverlaps(dt_exposures, dt_discontinued, type = "within", nomatch = 0)
    
    # Remove results where its the same exposure
    switchers <- switchers[code != i.code]
    
    # remove any results where switch date is outside study period
    switchers <- switchers[rx_date >= start_follow_up & rx_date <= end_follow_up,]
    
    if (nrow(switchers)>0){
      
      message("Switchers found for: ", discontinued_episode_name, " and ", exposure_name)
      
      # Drop columns window_start and window_end for the prescription as we have rx_date
      switchers[, c("i.window_start", "i.window_end"):=NULL]
      
      # Rename columns
      setnames(switchers, c("i.code", "Varname"), c("code_switched_to", "atc_group_switched_to"))
      
      # Save file
      saveRDS(switchers, file = file.path(paths$D3_dir, "tmp", paste0(discontinued_episode_name, "_to_", exposure_name, ".rds")))
      
    } else {
      
      message(red(paste0("No switchers: ", discontinued_episode_name, " and ", exposure_name)))
      
    }
  }
  
  # Comparing Discontinued Episodes to Alternative Medications
  for (altmed in seq_along(files_altmeds)){
    
    # Get name of exposure drug
    altmed_name <- gsub("_algo_med_altmed_data\\.rds$", "", files_altmeds[altmed])
    
    # Load altmed
    dt_altmeds <- as.data.table(readRDS(file.path(paths$D4_dir, "1.2_altmeds", files_altmeds[altmed])))
    
    # Keep needed cols only 
    dt_altmeds <- dt_altmeds[, .(person_id, code, Varname, rx_date)]
    
    # Create window start and window end columns
    dt_altmeds[, window_start := rx_date][, window_end := rx_date]
    
    # Set on key for faster searches
    setkey(dt_altmeds, person_id, window_start, window_end)
    setkey(dt_discontinued, person_id, window_start, window_end)
    
    # Find overlaps within 120 days after discontinuation
    switchers <- foverlaps(dt_altmeds, dt_discontinued, type = "within", nomatch = 0)
    
    # Remove results where its the same exposure
    switchers <- switchers[code != i.code]
    
    # remove any results where switch date is outside study period
    switchers <- switchers[rx_date >= start_follow_up & rx_date <= end_follow_up,]
    
    if (nrow(switchers)>0){
      
      message("Switchers found for: ", discontinued_episode_name, " and ", altmed_name)
      
      # Drop columns window_start and window_end for the prescription as we have rx_date
      switchers[, c("i.window_start", "i.window_end"):=NULL]
      
      # Rename columns
      setnames(switchers, c("i.code", "Varname"), c("code_switched_to", "atc_group_switched_to"))
      
      # Save file
      saveRDS(switchers, file = file.path(paths$D3_dir, "tmp", paste0(discontinued_episode_name, "_to_", altmed_name, ".rds")))
      
    } else {
      
      message(red(paste0("No switchers: ", discontinued_episode_name, " and ", altmed_name)))
      
    }
  }
}


####################################################################################################
####################################################################################################
####################################################################################################
# Merge back and perform counts
# list files 
files_switchers  <- list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$")
# Filter exposures for current pop_prefix only
files_switchers <- files_switchers[grepl(paste0("^", pop_prefix, "_"), files_switchers)]
# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_switchers <- files_switchers[!grepl("PC_HOSP", files_switchers)]

# Extract prefix before "_to"
prefixes <- sub("_to.*$", "", files_switchers)
unique_prefixes <- unique(prefixes)


for (pfx in seq_along(unique_prefixes)) {
  
  # get current prefix
  current_prefix <- unique_prefixes[pfx]
  
  message("Processing switchers for: ", current_prefix)
  
  # find all files matching prefix
  group <- files_switchers[prefixes == current_prefix]
  
  # Build full paths by pasting folder + file name
  switchers <- rbindlist(lapply(file.path(paths$D3_dir, "tmp", group), function(f) as.data.table(readRDS(f))), use.names = TRUE, fill = TRUE)
  
  # Order by person_id, and episode start and rx date
  setorder(switchers, person_id, episode.start, rx_date)
  
  # remove true duplicates
  switchers <- unique(switchers)
  
  # Keep only if rx_date falls between start and end follow up
  switchers <- switchers[rx_date >= start_follow_up & rx_date <= end_follow_up, ]
  
  # make copy to save
  switchers_data <- copy(switchers)
  
  # keep one switch per episode
  switchers <- switchers[, .SD[1], by = .(person_id, episode.start)]
  
  if(nrow(switchers)>0){
    
    # Assign calendar year of each switch
    switchers[, year := year(rx_date)]
    
    # Remove duplicates: Keep only one person id per year
    switchers <- unique(switchers, by = c("person_id", "year"))
    
    # Count number of discontinuers per year
    switcher_counts <- switchers[, .("N" = .N), by = year]
    
    # Match corresponding prevalence file
    matched_prevalence_file <- files_prevalence_counts[gsub("_prevalence_counts\\.rds$", "", files_prevalence_counts) == unique_prefixes[pfx]]
    
    if (length(matched_prevalence_file) == 1) {
      
      # Read in Prevalence file if file found 
      prev_counts <- readRDS(file.path(paths$D5_dir, "1.1_prevalence", matched_prevalence_file))
      
      # Prepare prevalence counts
      prev_counts[,c("n_total", "rate", "rate_computable") := NULL]
      setnames(prev_counts, "n_treated", "n_total")
      
      # Merge discontinued with prevalence
      switcher_all <- merge(switcher_counts, prev_counts, by = "year", all.y = TRUE)
      
      # Set N = 0 for years with no treatments
      switcher_all[is.na(N), N := 0]
      
      # Calculate discontinued as a rate (*100)
      switcher_all[, rate := round(100 * N / n_total, 3)][N == 0 & n_total == 0, rate := 0]
      
      # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
      if (nrow(switcher_all[N > n_total]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
      if (nrow(switcher_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
      
      # Save data where odd values 
      if(nrow(switcher_all[N > n_total])>0) fwrite(switcher_all[N > n_total], file.path(paths$D5_dir, "1.2_switching", paste0(current_prefix, "_num_gt_denominator.csv")))
      if(nrow(switcher_all[n_total == 0 & N != 0])>0) fwrite(switcher_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.2_switching", paste0(current_prefix, "_denominator_zero_numerator_nonzero.csv")))
      
      # Create column marking if rate is computable 
      switcher_all[, rate_computable := n_total > 0]
      
      # rename columns
      setnames(switcher_all, "N", "n_treated")
      
      # Save dataset 
      saveRDS(switchers_data, file.path(paths$D4_dir, "1.2_switching", paste0(unique_prefixes[pfx], "_switcher_data.rds")))
      
      # Save results 
      saveRDS(switcher_all, file.path(paths$D5_dir, "1.2_switching", paste0(unique_prefixes[pfx], "_switcher_counts.rds")))
      
    } else {
      
      message("No matching prevalence file found for ", unique_prefixes[pfx])
    }
  }
}

# Clean out tmp folder
if(length(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)) > 0) unlink(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE))










