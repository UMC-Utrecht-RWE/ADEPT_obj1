###############################################################################################################################################################################
# <<< Sub-objective 1.2: Polytherapy rate >>> 
# Measure: Annual polytherapy rate of ASM
# Numerator: The number of individuals who use ≥2 distinct ASMs in a calendar year with ≥182 days overlap between the treatment episodes 
# Denominator: Total number of study population in that calendar year in the data source
# Stratification by: Age groups, indication, calendar year, data source

# Pending: Stratification by age groups
###############################################################################################################################################################################

print("================================================================================")
print("========================= CALCULATE POYTHERAPY =================================")
print("================================================================================")

# List subgroups to exclude
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes"), pattern = "\\.rds$")
# Filter exposures for current pop_prefix only
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]
# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]
# Exclude subgroups
files_episodes <- files_episodes[!(gsub(paste0("^", pop_prefix, "_|_treatment_episode\\.rds$"), "", files_episodes) %in% exclude)]

# Load denominator file
denominator <- readRDS(file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))

for (epi1 in seq_along(files_episodes)){
  
  for(epi2 in seq_along(files_episodes)){
    
    # Skip same or duplicate pairs
    if (epi1 >= epi2) next
    
    # Save names of current episodes being processed
    name_epi1 <- sub("_treatment_episode\\.rds$", "", basename(files_episodes[epi1]))
    name_epi2 <- sub("_treatment_episode\\.rds$", "", basename(files_episodes[epi2]))
    
    # message("Processing: ", name_epi1, " and ", name_epi2)
    
    # Load treatment episodes
    dt1 <- as.data.table(readRDS(file.path(paths$D3_dir, "tx_episodes", files_episodes[epi1])))
    dt2 <- as.data.table(readRDS(file.path(paths$D3_dir, "tx_episodes", files_episodes[epi2])))
    
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
    
    # Find overlaps
    overlaps <- foverlaps(dt1_sub, dt2_sub, type = "any", nomatch = 0L)
    
    # Calculate overlap duration
    overlaps[, overlap_start := pmax(window_start, i.window_start)]
    overlaps[, overlap_end   := pmin(window_end, i.window_end)]
    overlaps[, overlap_days  := as.numeric(overlap_end - overlap_start) + 1]
    
    # Filter ≥182 days and same calendar year
    overlaps <- overlaps[overlap_days >= 182]
    
    # Overlap should be between start and end fu
    overlaps <- overlaps[overlap_start >= start_follow_up & overlap_start <= end_follow_up & overlap_end >= start_follow_up & overlap_end <= end_follow_up]
    
    if(nrow(overlaps)>0){
      
      message("Polytherapy found between: ", name_epi1, " and ", name_epi2)
      
      saveRDS(overlaps, file = file.path(paths$D3_dir, "tmp", paste0(name_epi1, "_to_", name_epi2, ".rds")))
      
    } else {
      
      message(red(paste("No Polytherapy found between:", name_epi1, "and", name_epi2)))
      
    }
    
  }
}



### Now we read back all the individual files to combine them and count them 
files_overlaps  <- list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_overlaps <- files_overlaps[grepl(paste0("^", pop_prefix, "_"), files_overlaps)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_overlaps <- files_overlaps[!grepl("PC_HOSP", files_overlaps)]

# ===================== OVERALL POLYTHERAPY RATE =========================

if(length(files_overlaps)>0){
  # Read and combine all pairwise overlap files
  all_overlaps <- rbindlist(lapply(file.path(paths$D3_dir, "tmp", files_overlaps), readRDS), use.names = TRUE, fill = TRUE)
  
  
  # Ensure overlap dates are IDate
  all_overlaps[, `:=`(overlap_start = as.IDate(overlap_start), overlap_end = as.IDate(overlap_end))]
  
  # Assign year(s) to each overlap start
  all_overlaps[,year:= year(overlap_start)]
  
  # Keep only one row per person per year
  all_overlaps_unique <- unique(all_overlaps, by = c("person_id", "year"))
  
  # Count unique individuals per year
  overall_counts <- all_overlaps_unique[, .N, by = year]
  
  # Merge with denominator
  overlap_all <- merge(overall_counts, denominator, by = "year", all.y = TRUE)
  
  # Handle missing numerator values
  overlap_all[is.na(N), N := 0]
  
  # Compute polytherapy rate per 1000 persons
  overlap_all[, rate := round(1000 * N / Freq, 3)]
  overlap_all[N == 0 & Freq == 0, rate := 0]
  
  # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
  if (nrow(overlap_all[N > Freq]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
  if (nrow(overlap_all[Freq == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
  
  # Save data where odd values 
  if(nrow(overlap_all[N > Freq])>0) fwrite(overlap_all[N > Freq], file.path(paths$D5_dir, "1.2_polytherapy",  "polytherapy_num_gt_denominator.csv"))
  if(nrow(overlap_all[Freq == 0 & N != 0])>0) fwrite(overlap_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.2_polytherapy", "polytherapy_denominator_zero_numerator_nonzero.csv"))
  
  # Create column marking if rate is computable 
  overlap_all[, rate_computable := Freq > 0]
  
  # Rename columns 
  setnames(overlap_all, c("N", "Freq"), c("n_treated", "n_total"))
  
  # Save dataset 
  saveRDS(all_overlaps, file.path(paths$D4_dir, "1.2_polytherapy", paste0(pop_prefix, "_polytherapy_data.rds")))
  
  # Save results
  saveRDS(overlap_all, file.path(paths$D5_dir, "1.2_polytherapy", paste0(pop_prefix, "_OVERALL_polytherapy_counts.rds")))
  
  # Clean out tmp folder
  if(length(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)) > 0) unlink(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE))
}
