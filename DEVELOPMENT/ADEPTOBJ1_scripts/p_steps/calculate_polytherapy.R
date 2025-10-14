###############################################################################################################################################################################
# <<< Sub-objective 1.2: Polytherapy rate >>> 
# Measure: Annual polytherapy rate of ASM
# Numerator: The number of individuals who use a distinct ASMs in a calendar year with >= 182 days overlap between the treatment episodes 
# Denominator: Total number of study population in that calendar year in the data source
# Stratification by: Indication, calendar year, data source

###############################################################################################################################################################################

print("================================================================================")
print("========================= CALCULATE POYTHERAPY =================================")
print("================================================================================")

# Subgroups to exclude
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes"), pattern = "\\.rds$")
files_episodes <- files_episodes[!(gsub(paste0("^", pop_prefix, "_|_treatment_episode\\.rds$"), "", files_episodes) %in% exclude)] # exclude subgroups

# Load denominator file
if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)] # Filters female vs Male
  # Load denominator file
  denominator <- readRDS(file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))
}


for (epi1 in seq_along(files_episodes)){
  
  for(epi2 in seq_along(files_episodes)){
    
    # Skip same or duplicate pairs
    if (epi1 >= epi2) next
    
    # Save names of current episodes being processed
    name_epi1 <- sub("_treatment_episode\\.rds$", "", basename(files_episodes[epi1]))
    name_epi2 <- sub("_treatment_episode\\.rds$", "", basename(files_episodes[epi2]))
    
    # Load treatment episodes
    dt1 <- as.data.table(readRDS(file.path(paths$D3_dir, "tx_episodes", files_episodes[epi1])))
    dt2 <- as.data.table(readRDS(file.path(paths$D3_dir, "tx_episodes", files_episodes[epi2])))
    
    if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
      # Remove duplicates
      dt1 <- unique(dt1, by = c("person_id", "episode.start"))
      dt2 <- unique(dt2, by = c("person_id", "episode.start"))
      
      # Drop unnecessary columns 
      dt1[,c("episode.ID", "end.episode.gap.days", "episode.duration"):= NULL]
      dt2[,c("episode.ID", "end.episode.gap.days", "episode.duration"):= NULL]
      
      # Create window start and window end columns - periods of overlap
      dt1[, window_start := episode.start][, window_end := episode.end]
      dt2[, window_start := episode.start][, window_end := episode.end]
      
      # Keep only overlapping person_ids
      common_ids <- intersect(dt1[, unique(person_id)], dt2[, unique(person_id)])
      
      if (length(common_ids) == 0) {
        message(red(paste("No Polytherapy found between:", name_epi1, "and", name_epi2)))
        next
      }
      
      dt1_sub <- dt1[person_id %in% common_ids]
      dt2_sub <- dt2[person_id %in% common_ids]
      
      # Set keys
      setkey(dt1_sub, person_id, window_start, window_end)
      setkey(dt2_sub, person_id, window_start, window_end)
      
    } else {
      
      # Remove duplicates
      dt1 <- unique(dt1, by = c("pregnancy_id", "episode.start"))
      dt2 <- unique(dt2, by = c("pregnancy_id", "episode.start"))
      
      # Drop unnecessary columns 
      dt1[,c("episode.ID", "end.episode.gap.days", "episode.duration"):= NULL]
      dt2[,c("episode.ID", "end.episode.gap.days", "episode.duration"):= NULL]
      
      # Create window start and window end columns - periods of overlap
      dt1[, window_start := episode.start][, window_end := episode.end]
      dt2[, window_start := episode.start][, window_end := episode.end]
      
      # Keep only overlapping pregnancy_ids
      common_ids <- intersect(dt1[, unique(pregnancy_id)], dt2[, unique(pregnancy_id)])
      
      if (length(common_ids) == 0) {
        message(red(paste("No Polytherapy found between:", name_epi1, "and", name_epi2)))
        next
      }
      
      dt1_sub <- dt1[pregnancy_id %in% common_ids]
      dt2_sub <- dt2[pregnancy_id %in% common_ids]
      
      # Set keys
      setkey(dt1_sub, pregnancy_id, window_start, window_end)
      setkey(dt2_sub, pregnancy_id, window_start, window_end)
    }
    
    # COMMON TO BOTH 
    # Find overlaps
    overlaps <- foverlaps(dt1_sub, dt2_sub, type = "any", nomatch = 0L)
    
    # Calculate overlap duration
    overlaps[, overlap_start := pmax(window_start, i.window_start)]
    overlaps[, overlap_end   := pmin(window_end, i.window_end)]
    overlaps[, overlap_days  := as.numeric(overlap_end - overlap_start) + 1]
    
    if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
      # The filtering is just for the message
      # We will save the dataset with all the overlaps and filter before counts or before using in for 1.4 
      if (nrow(overlaps[overlap_days >= 182, ]) == 0) {
        message(red(paste("No Polytherapy found between:", name_epi1, "and", name_epi2)))
        next
      } else {
        message("Polytherapy found between: ", name_epi1, " and ", name_epi2)
      }
      
    } else {
      # EFEMERIS AND FIN_REG - we only need this for obj 1.4, where there needs to be an overlap of 3 months with pregnancy 
      if (nrow(overlaps[overlap_days >= 91, ]) == 0) {
        message(red(paste("No Polytherapy found between:", name_epi1, "and", name_epi2)))
        next
      } else {
        message("Polytherapy found between: ", name_epi1, " and ", name_epi2)
      }
    }
    
    # Save overlapping data in temp folder - Please note that these are all overlapping records regardless of the length of the overlap
    saveRDS(overlaps, file = file.path(paths$D3_dir, "tmp", paste0(name_epi1, "_to_", name_epi2, ".rds")))
    
  }
}


### Read back all files in temp folder 
files_overlaps  <- list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$")

# ===================== OVERALL POLYTHERAPY RATE =========================
# Combine all the temp polytherapy files in one and save (all DEAP's)
if(length(files_overlaps)>0){ 
  
  # Read and combine all pairwise overlap files
  all_overlaps <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "tmp", files_overlaps), readRDS), use.names = TRUE, fill = TRUE))
  
  # Ensure overlap dates are IDate
  all_overlaps[, `:=`(overlap_start = as.IDate(overlap_start), overlap_end = as.IDate(overlap_end))]
  
  # Assign year to each overlap start
  all_overlaps[,year:= year(overlap_start)]
  
  # This saves all overlaps regardless of length of overlap - this will be used in obj 1.4
  saveRDS(all_overlaps, file.path(paths$D4_dir, "1.2_polytherapy", paste0(pop_prefix, "_polytherapy_data.rds")))
  
  # Clean out tmp folder
  if(length(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)) > 0) unlink(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE))
  
}

# COUNTS
if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  
  # Overlap should be between start and end fu
  all_overlaps <- all_overlaps[overlap_start >= start_follow_up & overlap_start <= end_follow_up & overlap_end >= start_follow_up & overlap_end <= end_follow_up]
  
  # Since we are no longer filtering in the previous part, we need to do it here!
  all_overlaps_182 <- all_overlaps[overlap_days >= 182,]
  
  # Sort by person id and overlap start
  setorder(all_overlaps_182, person_id, overlap_start)
  
  # Keep only one row per person per year
  all_overlaps_unique_182 <- unique(all_overlaps_182, by = c("person_id", "year"))
  
  # Count unique individuals per year
  overall_counts_182 <- all_overlaps_unique_182[, .N, by = year]
  
  # Merge with denominator
  overlap_all_182 <- merge(overall_counts_182, denominator, by = "year", all.y = TRUE)
  
  # Handle missing numerator values
  overlap_all_182[is.na(N), N := 0]
  
  # Compute polytherapy rate per 1000 persons
  overlap_all_182[, rate := round(1000 * N / Freq, 3)]
  overlap_all_182[N == 0 & Freq == 0, rate := 0]
  
  # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
  if (nrow(overlap_all_182[N > Freq]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
  if (nrow(overlap_all_182[Freq == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
  
  # Save data where odd values 
  if(nrow(overlap_all_182[N > Freq])>0) fwrite(overlap_all[N > Freq], file.path(paths$D5_dir, "1.2_polytherapy",  "polytherapy_num_gt_denominator.csv"))
  if(nrow(overlap_all_182[Freq == 0 & N != 0])>0) fwrite(overlap_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.2_polytherapy", "polytherapy_denominator_zero_numerator_nonzero.csv"))
  
  # Create column marking if rate is computable 
  overlap_all_182[, rate_computable := Freq > 0]
  
  # Rename columns 
  setnames(overlap_all_182, c("N", "Freq"), c("n_treated", "n_total"))
  
  # Save results
  saveRDS(overlap_all_182, file.path(paths$D5_dir, "1.2_polytherapy", paste0(pop_prefix, "_OVERALL_polytherapy_counts_182.rds")))
  
  
  # Sensitivity analysis overlap of 120 days 
  
  # Since we are no longer filtering in the previous part, we need to do it here!
  all_overlaps_120 <- all_overlaps[overlap_days >= 120,]
  
  # Sort by person id and overlap start
  setorder(all_overlaps_120, person_id, overlap_start)
  
  # Keep only one row per person per year
  all_overlaps_unique_120 <- unique(all_overlaps_120, by = c("person_id", "year"))
  
  # Count unique individuals per year
  overall_counts_120 <- all_overlaps_unique_120[, .N, by = year]
  
  # Merge with denominator
  overlap_all_120 <- merge(overall_counts_120, denominator, by = "year", all.y = TRUE)
  
  # Handle missing numerator values
  overlap_all_120[is.na(N), N := 0]
  
  # Compute polytherapy rate per 1000 persons
  overlap_all_120[, rate := round(1000 * N / Freq, 3)]
  overlap_all_120[N == 0 & Freq == 0, rate := 0]
  
  # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
  if (nrow(overlap_all_120[N > Freq]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
  if (nrow(overlap_all_120[Freq == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
  
  # Save data where odd values 
  if(nrow(overlap_all_120[N > Freq])>0) fwrite(overlap_all[N > Freq], file.path(paths$D5_dir, "1.2_polytherapy",  "polytherapy_num_gt_denominator.csv"))
  if(nrow(overlap_all_120[Freq == 0 & N != 0])>0) fwrite(overlap_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.2_polytherapy", "polytherapy_denominator_zero_numerator_nonzero.csv"))
  
  # Create column marking if rate is computable 
  overlap_all_120[, rate_computable := Freq > 0]
  
  # Rename columns 
  setnames(overlap_all_120, c("N", "Freq"), c("n_treated", "n_total"))
  
  # create folder for stratification counts
  dir.create(file.path(paths$D5_dir, "1.2_polytherapy", "sensitivity"), showWarnings = FALSE, recursive = TRUE)
  
  # Save results
  saveRDS(overlap_all_120, file.path(paths$D5_dir, "1.2_polytherapy", "sensitivity", paste0(pop_prefix, "_OVERALL_polytherapy_counts_120.rds")))  
}
