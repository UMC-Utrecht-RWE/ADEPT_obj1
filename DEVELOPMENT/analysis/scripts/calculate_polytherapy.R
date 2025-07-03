print("================================================================================")
print("========================= CALCULATE POYTHERAPY =================================")
print("================================================================================")

### Load All Required Data Sets ###

# Load the denominator file
denominator <- readRDS(file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))

# List all exposure treatment episodes
exposure_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = paste0("^", pop_prefix, ".*\\.rds$"), full.names = TRUE)

# If population = PC, exclude any PC_HOSP files 
if(populations[pop] == "PC_study_population.rds"){exposure_episodes<-exposure_episodes[!grepl("PC_HOSP", exposure_episodes)]}

for (epi1 in seq_along(exposure_episodes)){
  
  for(epi2 in seq_along(exposure_episodes)){
    
    # Skip same or duplicate pairs
    if (epi1 >= epi2) next
    
    # Save names of current episodes being processed
    name_epi1 <- sub("_treatment_episode\\.rds$", "", basename(exposure_episodes[epi1]))
    name_epi2 <- sub("_treatment_episode\\.rds$", "", basename(exposure_episodes[epi2]))
    
    message("Processing: ", name_epi1, " and ", name_epi2)
    
    # Load treatment episodes
    dt1 <- readRDS(exposure_episodes[epi1])
    dt2 <- readRDS(exposure_episodes[epi2])
    
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
    # overlaps <- overlap_dt[overlap_days >= 182]
    #overlaps <- overlap_dt[overlap_days >= 5 & year(overlap_start) == year(overlap_end)]
    
    if(nrow(overlaps)>0){
      # Assign calendar year of each incident episode
      overlaps[, year_start_overlap := year(overlap_start)][, year_end_overlap := year(overlap_end)]
      
      # For each row, generate all years covered by the overlap interval
      expanded_years <- overlaps[, .(Year = seq(year(overlap_start), year(overlap_end))), by = person_id]
      
      # Count unique persons per year
      overlap_counts <- unique(expanded_years)[, .N, by = Year]
      
      # Merge with denominator to calculate prevalence; include all years even if no treatments (all.y = TRUE)
      overlap_all <- merge(overlap_counts, denominator, by = "Year", all.y = TRUE)
      
      # Set N = 0 for years with no treatments
      overlap_all[is.na(N), N := 0]
      
      # Calculate incidence per 1000 person
      overlap_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
      
      # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
      if (nrow(overlap_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
      if (nrow(overlap_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
      
      # Create column marking if rate is computable i.e. if numerator is greater than denominator
      overlap_all[, rate_computable := !(Freq == 0 & N > 0)]
      
      # Rename columns
      setnames(overlap_all, c("N", "Freq"), c("n_treated", "n_total"))
      
      saveRDS(overlaps, file.path(paths$D4_dir, "1.2_polytherapy", paste0(name_epi1, "-", name_epi2, "_polytherapy_data.rds")))
      saveRDS(overlap_all, file.path(paths$D5_dir, "1.2_polytherapy", paste0(name_epi1, "-", name_epi2, "_polytherapy.rds")))
      
    } else {
      message(red(paste("No polytherapy between:", name_epi1, "and", name_epi2)))
    }
    
  }
}

