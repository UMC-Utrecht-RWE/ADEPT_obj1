###############################################################################################################################################################################
# <<< Sub-objective 1.4: Switching rate during pregnancy >>>
# Measure: Switching rate from one ASM to a different ASM or to an alternative medication during pregnancy
# Numerator: The number of pre-pregnancy users of an ASM within a calendar year that switched to a different ASM or alternative medication during the pregnancy period
# Denominator: Total number of pre-pregnancy users of an ASM in a calendar year in the data source
# Stratification by: Overall, individual drug substance, drug sub-groups, indication, calendar year, data source

# Conditions:
### Pre-pregnancy users
###############################################################################################################################################################################
print("================================================================================================")
print("========================= CALCULATING SWITCHING RATES DURING PREGNANCY =========================")
print("================================================================================================")

# List subgroups to exclude
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# List pre-pregnancy discontinued before pregnancy start episodes
files_discontinued_episodes <- list.files(file.path(paths$D4_dir, "1.4_pregnancy_discontinuation"))
files_discontinued_episodes <- files_discontinued_episodes[!grepl(paste(exclude, collapse = "|"), files_discontinued_episodes)] # exclude subgrou[s]
files_discontinued_episodes <- files_discontinued_episodes[grepl("before", files_discontinued_episodes)] # exclude subgroup[s]

# List exposure meds 
files_exposures <- list.files(file.path(paths$D3_dir, "exposure"))
files_exposures <- files_exposures[!grepl(paste(exclude, collapse = "|"), files_exposures)] # exclude subgroups
if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) files_exposures <- files_exposures[grepl("_F_", files_exposures)] # exclude subgroups

# List altmeds
files_altmeds <- list.files(file.path(paths$D4_dir, "1.2_altmeds"))
if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) files_altmeds <- files_altmeds[grepl("_F_", files_altmeds)]

# List all pre_pregnancy counts (denominator)
files_counts   <- list.files(file.path(paths$D5_dir, "1.3_pre-pregnancy_use"))

for (episode in seq_along(files_discontinued_episodes)) {
  
  # Get name of episode
  discontinued_episode_name <- sub("_before_discontinuation_in_pregnancies_data\\.rds$", "", files_discontinued_episodes[episode])
  
  # Read in Episode
  dt_discontinued <- readRDS(file.path(paths$D4_dir, "1.4_pregnancy_discontinuation", files_discontinued_episodes[episode]))
  
  # Drop unnecessary columns
  dt_discontinued <- dt_discontinued[,.(pregnancy_id, person_id, pregnancy_start_date, pregnancy_end_date, highest_quality, preg_year, episode.start, episode.end, atc_group, code)]
  
  # Rename cols that will repeat after merge
  setnames(dt_discontinued, c("atc_group", "code"), c("atc_group1", "code1"))
  
  # Discontinued Episodes vs Exposures
  for (exposure in seq_along(files_exposures)) {
    
    # Get name of exposure drug
    exposure_name <- sub("\\.rds$", "", files_exposures[exposure])
    
    # Skip if the same exposure
    if (discontinued_episode_name == exposure_name) next
    
    # Print message
    message("Checking for switchers between: ", discontinued_episode_name, " and ", exposure_name)
    
    # Load exposure prescriptions
    dt_exposures <- as.data.table(readRDS(file.path(paths$D3_dir, "exposure", files_exposures[exposure])))
    
    # Drop unnecessary columns
    dt_exposures <- dt_exposures[,.(person_id, rx_date, atc_group, code, start_follow_up, end_follow_up)]
    
    # Rename cols that will repeat after merge
    setnames(dt_exposures, c("atc_group", "code"), c("atc_group2", "code2"))
    
    # Remove duplicates
    dt_exposures <- unique(dt_exposures)
    
    # Only keep records between entry and exit dates - this used to be done in create subsets. It is not being done there anymore
    if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) dt_exposures <- dt_exposures[rx_date >= start_follow_up & rx_date <= end_follow_up]
    
    # Merge on person_id
    dt_all <- merge(dt_discontinued, dt_exposures, by = "person_id")
    
    if (nrow(dt_all)>0){
      
      # Keep prescriptions that are between pregnancy start and pregnancy end
      switchers <- dt_all[rx_date>=pregnancy_start_date & rx_date < pregnancy_end_date,]
      
      # Print message
      if (nrow(switchers) > 0) {
        
        # Print message
        message("Switchers found for: ", discontinued_episode_name, " and ", exposure_name)
        
        # Save file 
        saveRDS(switchers, file = file.path(paths$D3_dir, "tmp", paste0(discontinued_episode_name, "_to_", exposure_name, ".rds")))
      }
    }
  }
  
  # Discontinued Episodes vs Altmeds
  for (altmed in seq_along(files_altmeds)){
    
    # Get name of altmed 
    altmed_name <- sub("\\.rds$", "", files_altmeds[altmed])
    
    # Print message
    message("Checking for switchers between: ", discontinued_episode_name, " and ", altmed_name)
    
    # Load altmed prescriptions
    dt_altmeds <- as.data.table(readRDS(file.path(paths$D4_dir, "1.2_altmeds", files_altmeds[altmed])))
    
    # Add column with altmed name
    dt_altmeds[, atc_group := sub(paste0("^", pop_prefix, "_F_(DP_.*?)_altmed_data\\.rds$"), "\\1", files_altmeds[altmed])]
    
    # Drop unnecessary columns
    dt_altmeds <- dt_altmeds[,.(person_id, rx_date, atc_group, code, start_follow_up, end_follow_up)]
    
    # Rename cols that will repeat after merge
    setnames(dt_altmeds, c("atc_group", "code"), c("atc_group2", "code2"))
    
    # Remove duplicates
    dt_altmeds <- unique(dt_altmeds)
    
    # Only keep records between entry and exit dates - this used to be done in create subsets. It is not being done there anymore
    if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) dt_exposures <- dt_altmeds[rx_date >= start_follow_up & rx_date <= end_follow_up]
    
    # Merge on person_id
    dt_all <- merge(dt_discontinued, dt_altmeds, by = "person_id")
    
    if (nrow(dt_all)>0){
      
      # Keep prescriptions that are between pregnancy start and pregnancy end
      switchers <- dt_all[rx_date>=pregnancy_start_date & rx_date < pregnancy_end_date,]
      
      # Print message
      if (nrow(switchers) > 0) {
        
        # Print message
        message("Switchers found for: ", discontinued_episode_name, " and ", altmed_name)
        
        # Save file 
        saveRDS(switchers, file = file.path(paths$D3_dir, "tmp", paste0(discontinued_episode_name, "_to_", altmed_name, ".rds")))
      }
    }
  }
}


# List files from Temp folder
files_switchers  <- list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$")

# Extract prefix before "_to"
prefixes <- sub("_to.*$", "", files_switchers)

# Get list of unique prefixes
unique_prefixes <- unique(prefixes)

for (pfx in seq_along(unique_prefixes)) {
  
  # Get current prefix
  current_prefix <- unique_prefixes[pfx]
  
  # Print Message
  message("Processing switchers for: ", current_prefix)
  
  # Find all files matching prefix
  group <- files_switchers[prefixes == current_prefix]
  
  # Build full paths by pasting folder + file name
  switchers <- rbindlist(lapply(file.path(paths$D3_dir, "tmp", group), function(f) as.data.table(readRDS(f))), use.names = TRUE, fill = TRUE)
  
  # Find matching pre-pregnancy denominator file
  denom_file <- grep(paste0("^", current_prefix, "_pre_pregnancy_counts\\.rds$"), files_counts, value = TRUE)
  
  # Skip if no pre pregnancy users
  if (length(denom_file) != 1) next
  
  # Load denominator
  dt_counts <- readRDS(file.path(paths$D5_dir, "1.3_pre-pregnancy_use", denom_file))
  
  # Order data 
  setorder(switchers, person_id, pregnancy_id, preg_year, rx_date)
  
  # remove true duplicates
  switchers <- unique(switchers)
  
  # make copy to save
  switchers_data <- copy(switchers)
  
  if (nrow(switchers_data) > 0) {
    
    # Remove duplicates: Keep only one pregnancy id per year
    switchers <- unique(switchers, by = c("pregnancy_id", "preg_year"))
    
    # Count by pregnancy
    switcher_counts <- switchers[, .(N = .N), by = preg_year]
    
    # Prepare denominator
    dt_counts_copy <- copy(dt_counts)
    dt_counts_copy <- dt_counts_copy[, .(preg_year, n_treated)]
    setnames(dt_counts_copy, "n_treated", "n_total")
    
    # Merge numerator and denominator
    switcher_all <- merge(switcher_counts, dt_counts_copy, by = "preg_year", all.y = TRUE)
    switcher_all[is.na(N), N := 0]
    
    # Calculate switcher as a rate (*100)
    switcher_all[, rate := round(100 * N / n_total, 3)]
    switcher_all[N == 0 & n_total == 0, rate := 0]
    
    # Warnings
    if (nrow(switcher_all[N > n_total]) > 0) warning(red("Warning: Numerator > Denominator"))
    if (nrow(switcher_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator"))
    
    # Save odd cases
    if (nrow(switcher_all[N > n_total]) > 0) fwrite(switcher_all[N > n_total], file.path(paths$D5_dir, "1.4_pregnancy_switching", paste0(treatment, "_num_gt_denominator.csv")))
    if (nrow(switcher_all[n_total == 0 & N != 0]) > 0) fwrite(switcher_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.4_pregnancy_switching", paste0(treatment, "_denominator_zero_numerator_nonzero.csv")))
    
    # Create column marking if rate is computable
    switcher_all[, rate_computable := n_total > 0]
    
    # Rename columns
    setnames(switcher_all, "N", "n_treated")
    
    # Save output
    saveRDS(switchers_data, file.path(paths$D4_dir, "1.4_pregnancy_switching", paste0(current_prefix, "_switching_in_pregnancies_data.rds")))
    saveRDS(switcher_all, file.path(paths$D5_dir, "1.4_pregnancy_switching", paste0(current_prefix, "_switching_in_pregnancies_counts.rds")))
  }
}


# Clean out tmp folder
if (length(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)) > 0) unlink(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE))
