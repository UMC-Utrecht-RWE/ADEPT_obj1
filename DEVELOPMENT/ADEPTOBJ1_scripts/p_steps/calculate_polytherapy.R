print("================================================================================")
print("========================= CALCULATE POYTHERAPY =================================")
print("================================================================================")

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]}

# Load denominator file
denominator <- readRDS(file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))

for (epi1 in seq_along(files_episodes)){
  
  for(epi2 in seq_along(files_episodes)){
    
    # Skip same or duplicate pairs
    if (epi1 >= epi2) next
    
    # Save names of current episodes being processed
    name_epi1 <- sub("_treatment_episode\\.rds$", "", basename(files_episodes[epi1]))
    name_epi2 <- sub("_treatment_episode\\.rds$", "", basename(files_episodes[epi2]))
    
    message("Processing: ", name_epi1, " and ", name_epi2)
    
    # Load treatment episodes
    dt1 <- as.data.table(readRDS(file.path(paths$D3_dir, "tx_episodes", "individual", files_episodes[epi1])))
    dt2 <- as.data.table(readRDS(file.path(paths$D3_dir, "tx_episodes", "individual", files_episodes[epi2])))
    
    # Drop unnecessary columns 
    dt1[,c("episode.ID", "end.episode.gap.days", "episode.duration"):= NULL]
    dt2[,c("episode.ID", "end.episode.gap.days", "episode.duration"):= NULL]
    
    # Create window start and window end columns - periods of overlap
    dt1[, window_start := episode.start][, window_end := episode.end]
    dt2[, window_start := episode.start][, window_end := episode.end]
    
    # Keep only overlapping person_ids
    common_ids <- intersect(dt1[, unique(person_id)], dt2[, unique(person_id)])
    
    if (length(common_ids) == 0) next
    
    dt1_sub <- dt1[person_id %in% common_ids]
    dt2_sub <- dt2[person_id %in% common_ids]
    
    dt1_sub[, `:=`(window_start = as.IDate(window_start), window_end = as.IDate(window_end))]
    dt2_sub[, `:=`(window_start = as.IDate(window_start), window_end = as.IDate(window_end))]
    
    # Set keys
    setkey(dt1_sub, person_id, window_start, window_end)
    setkey(dt2_sub, person_id, window_start, window_end)
    
    # Find overlaps
    overlaps <- foverlaps(dt1_sub, dt2_sub, type = "any", nomatch = 0L)
    
    # Calculate overlap duration
    overlaps[, overlap_start := as.IDate(pmax(window_start, i.window_start))]
    overlaps[, overlap_end   := as.IDate(pmin(window_end, i.window_end))]
    overlaps[, overlap_days  := as.numeric(overlap_end - overlap_start) + 1]
    
    # Filter ≥182 days and same calendar year
    overlaps <- overlaps[overlap_days >= 182]
    # overlaps <- overlap_dt[overlap_days >= 182 & year(overlap_start) == year(overlap_end)]
    
    # Overlap should be between start and end fu
    overlaps <- overlaps[overlap_start>=start_follow_up & overlap_start <=end_follow_up & overlap_start>=start_follow_up & overlap_start <=end_follow_up]
    
    # Assign calendar year of each episode
    overlaps[, year_start_overlap := year(overlap_start)][, year_end_overlap := year(overlap_end)]
    
    if(nrow(overlaps)>0){
      
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
if(pop_prefix=="PC"){files_overlaps <- files_overlaps[!grepl("PC_HOSP", files_overlaps)]}

# Extract prefix before "_to"
prefixes <- sub("_to.*$", "", files_overlaps)
unique_prefixes <- unique(prefixes)

for (pfx in seq_along(unique_prefixes)) {
  
  current_prefix <- unique_prefixes[pfx]
  
  # Files matching current prefix (just filenames)
  group <- files_overlaps[prefixes == current_prefix]
  
  # Read in files of the same prefix
  overlaps <- rbindlist(lapply(file.path(paths$D3_dir, "tmp", group), function(f) as.data.table(readRDS(f))), use.names = TRUE, fill = TRUE)
  
  message("Processing Polytherapy for: ", current_prefix)
  
  # For each row, generate all years covered by the overlap interval
  dt_expanded <- overlaps[, .(year = mapply(seq, year(overlap_start), year(overlap_end))), by = person_id]
  dt_expanded <- dt_expanded[, .(year = unlist(year)), by = person_id]
  dt_expanded <- unique(dt_expanded)
  
  
  # Remove duplicates: Keep only one person id per year
  dt_expanded <- unique(dt_expanded, by = c("person_id", "year"))

  # Count number of unique treated persons per year (numerator for prevalence)
  overlap_counts <- dt_expanded[, .N, by = year]
  
  # Merge with denominator to calculate rates; include all years even if no treatments (all.y = TRUE)
  overlap_all <- merge(overlap_counts, denominator, by = "year", all.y = TRUE)
  
  # Set N = 0 for years with no treatments
  overlap_all[is.na(N), N := 0]
  
  # Calculate rates per 1000 person
  overlap_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
  
  # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
  if (nrow(overlap_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
  if (nrow(overlap_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
  
  # Save data where odd values 
  if(nrow(overlap_all[N > Freq])>0) fwrite(overlap_all[N > Freq], file.path(paths$D5_dir, "1.1_prevalence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_num_gt_denominator.csv")))
  if(nrow(overlap_all[Freq == 0 & N != 0])>0) fwrite(overlap_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.1_prevalence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_denominator_zero_numerator_nonzero.csv")))
  
  # Create column marking if rate is computable 
  overlap_all[, rate_computable := !(Freq == 0 & N >= 0)]
  
  # Rename columns 
  setnames(overlap_all, c("N", "Freq"), c("n_treated", "n_total"))
  
  # Save dataset 
  saveRDS(overlaps, file.path(paths$D4_dir, "1.2_polytherapy", paste0(unique_prefixes[pfx], "_polytherapy_data.rds")))
  
  # Save results 
  saveRDS(overlap_all, file.path(paths$D5_dir, "1.2_polytherapy", paste0(unique_prefixes[pfx], "_polytherapy.rds")))
  
} 


# Clean out tmp folder
if(length(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)) > 0) unlink(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE))





















