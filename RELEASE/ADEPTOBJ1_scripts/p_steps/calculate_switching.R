print("===============================================================================")
print("========================= CALCULATE SWITCHING =================================")
print("===============================================================================")

### Load Data Sets ###
# We Exclude 
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# Discontinued Episodes
files_discontinued_episodes <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_discontinued_episodes <- files_discontinued_episodes[grepl(paste0("^", pop_prefix, "_"), files_discontinued_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_discontinued_episodes <- files_discontinued_episodes[!grepl("PC_HOSP", files_discontinued_episodes)]}

# Exclude grouped exposures
files_discontinued_episodes <- files_discontinued_episodes[!(gsub(paste0("^", pop_prefix, "_|_discontinued_data\\.rds$"), "", files_discontinued_episodes)) %in% exclude]

# All Single Exposure Medications 
files_exposures <- list.files(file.path(paths$D3_dir, "exposure"))

# Filter exposures for current pop_prefix only
files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_exposures <- files_exposures[!grepl("PC_HOSP", files_exposures)]}

# All Alternative Medications 
files_altmeds <- list.files(file.path(paths$D4_dir, "1.2_altmeds"))

# Filter exposures for current pop_prefix only
files_altmeds <- files_altmeds[grepl(paste0("^", pop_prefix, "_"), files_altmeds)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_altmeds <- files_altmeds[!grepl("PC_HOSP", files_altmeds)]}

# Prevalence Count Files 
files_prevalence_counts <- list.files(file.path(paths$D5_dir, "1.1_prevalence"), pattern = "\\.rds$")

# Filter prevalence counts for current pop_prefix only
files_prevalence_counts <- files_prevalence_counts[grepl(paste0("^", pop_prefix, "_"), files_prevalence_counts)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_prevalence_counts <- files_prevalence_counts[!grepl("PC_HOSP", files_prevalence_counts)]}


for(episode in seq_along(files_discontinued_episodes)){
  
  # Read in Episode 
  dt_discontinued <- readRDS(file.path(paths$D4_dir, "1.2_discontinued", files_discontinued_episodes[episode]))
  
  # Drop unnecessary columns
  dt_discontinued[,c("episode.ID", "end.episode.gap.days", "episode.duration", "next_start", "discontinuer_flag", "year"):= NULL]
  
  # Create window start and window end columns - period where switcher could be found
  dt_discontinued[, window_start := episode.end][, window_end := episode.end + 120]
  
  # Set on key for faster searches
  setkey(dt_discontinued, person_id, window_start, window_end)
  
  for(exposure in seq_along(files_exposures)){
    
    if(gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode]) == gsub("\\.rds$", "", files_exposures[exposure])) next
    
    message("Checking: ", gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode]), " and ", gsub("\\.rds$", "", files_exposures[exposure]))
    
    # Read in exposures
    dt_exposures <- as.data.table(readRDS(file.path(paths$D3_dir, "exposure", files_exposures[exposure])))
    
    # Keep only needed columns
    dt_exposures <- dt_exposures[, .(person_id, code, Varname, rx_date)]
    
    # Create window start and window end columns
    dt_exposures[, window_start := rx_date][, window_end := rx_date]
    
    # Set on key for faster searches
    setkey(dt_exposures, person_id, window_start, window_end)
    
    # Find overlaps within 120 days after discontinuation
    switchers <- foverlaps(dt_exposures, dt_discontinued, type = "within", nomatch = 0)
    
    # Remove results where its the same exposure
    switchers <- switchers[code != i.code]
    
    if (nrow(switchers)>0){
      
      # Drop columns window_start and window_end for the prescription as we have rx_date
      switchers[, c("i.window_start", "i.window_end"):=NULL]
      
      # Rename columns
      setnames(switchers, c("i.code", "Varname"), c("code_switched_to", "atc_group_switched_to"))
      
      # Save file
      saveRDS(switchers, file = file.path(paths$D3_dir, "tmp", paste0(gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode]), "_to_", gsub("\\.rds$", "", files_exposures[exposure]), ".rds")))
      
    } else {
      
      message(red(paste0("No switchers: ", gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode]), " and ", gsub("\\.rds$", "", files_exposures[exposure]))))
      
    }
  }
  
  for (altmed in seq_along(files_altmeds)){
    
    message("Checking: ", gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode]), " and ", gsub("_altmed_data\\.rds$", "", files_altmeds[altmed]))
    
    # Read in altmeds
    dt_altmeds <- as.data.table(readRDS(file.path(paths$D4_dir, "1.2_altmeds", files_altmeds[altmed])))
    
    # Keep only needed columns
    dt_altmeds <- dt_altmeds[, .(person_id, code, Varname, rx_date)]
    
    # Create window start and window end columns
    dt_altmeds[, window_start := rx_date][, window_end := rx_date]
    
    # Set on key for faster searches
    setkey(dt_altmeds, person_id, window_start, window_end)
    
    # Find overlaps within 120 days after discontinuation
    switchers <- foverlaps(dt_altmeds, dt_discontinued, type = "within", nomatch = 0)
    
    # Remove results where its the same exposure
    switchers <- switchers[code != i.code]
    
    if (nrow(switchers)>0){
      
      # Get group name of alt group
      alt_group_name <- gsub("_altmed_data\\.rds$", "", files_altmeds[altmed])
      alt_group_name <- gsub(paste0(pop_prefix, "_"), "", alt_group_name)
      
      # Assign that value into a new column
      switchers[, atc_group_switched_to := alt_group_name]
      print(unique(switchers$atc_group_switched_to))
      
      # Drop columns window_start and window_end for the prescription as we have rx_date
      switchers[, c("i.window_start", "i.window_end"):=NULL]
      
      # Rename columns
      setnames(switchers, c("i.code"), c("code_switched_to"))
      
      # Save file
      saveRDS(switchers, file = file.path(paths$D3_dir, "tmp", paste0(gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode]), "_to_", gsub("_altmed_data\\.rds$", "", files_altmeds[altmed]), ".rds")))
      
    } else {
      
      message(red(paste0("No switchers: ", gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode]), " and ", gsub("_altmed_data\\.rds$", "", files_altmeds[altmed]))))
      
    }
  }
}


### Now we read back all the individual files to combine them and count them 
files_switchers  <- list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_switchers <- files_switchers[grepl(paste0("^", pop_prefix, "_"), files_switchers)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_switchers <- files_switchers[!grepl("PC_HOSP", files_switchers)]}

# Extract prefix before "_to"
prefixes <- sub("_to.*$", "", files_switchers)
unique_prefixes <- unique(prefixes)

for (pfx in seq_along(unique_prefixes)) {
  
  current_prefix <- unique_prefixes[pfx]
  
  # Files matching current prefix (just filenames)
  group <- files_switchers[prefixes == current_prefix]
  
  # Build full paths by pasting folder + file name
  
  switchers <- rbindlist(lapply(file.path(paths$D3_dir, "tmp", group), function(f) as.data.table(readRDS(f))), use.names = TRUE, fill = TRUE)
  
  message("Processing switchers for: ", current_prefix)
  
  # Switchers need to be between start and end follow up
  switchers <- switchers[rx_date >= start_follow_up & rx_date <= end_follow_up,]
  
  if(nrow(switchers)>0){
    
    # Assign calendar year of each switch
    switchers[, year := year(rx_date)]
    
    # Remove duplicates: Keep only one person id per year
    switchers <- unique(switchers, by = c("person_id", "year"))
    
    # Count number of discontinuers per year
    switcher_counts <- switchers[, .("N" = .N), by = year]
    
    # Match corresponding prevalence file
    matched_prevalence_file <- files_prevalence_counts[gsub("_prevalence\\.rds$", "", files_prevalence_counts) == unique_prefixes[pfx]]
    
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
      if (nrow(switcher_all[N > n_total]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
      if (nrow(switcher_all[n_total == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
      
      # Save data where odd values 
      if(nrow(switcher_all[N > n_total])>0) fwrite(switcher_all[N > n_total], file.path(paths$D5_dir, "1.2_discontinued", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_num_gt_denominator.csv")))
      if(nrow(switcher_all[n_total == 0 & N != 0])>0) fwrite(switcher_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.2_discontinued", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_denominator_zero_numerator_nonzero.csv")))
      
      # Create column marking if rate is computable 
      switcher_all[, rate_computable := !(n_total == 0 & N > 0)]
      
      # rename columns
      setnames(switcher_all, "N", "n_treated")
      
      # Save dataset 
      saveRDS(switchers, file.path(paths$D4_dir, "1.2_switching", paste0(unique_prefixes[pfx], "_switchers_data.rds")))
      
      # Save results 
      saveRDS(switcher_all, file.path(paths$D5_dir, "1.2_switching", paste0(unique_prefixes[pfx], "_switched.rds")))
      
    } else {
      
      message("No matching prevalence file found for ", unique_prefixes[pfx])
      
    }
  }
}


# Clean out tmp folder
if(length(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)) > 0) unlink(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE))
  






















