print("===================================================================================================")
print("========================= CALCULATING DISCONTINUATION - INDIVIDUAL GROUPS =========================")
print("===================================================================================================")

# List all .rds files (full paths)
all_files <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = "\\.rds$", full.names = TRUE)
# Filter to keep only files starting with pop_prefix + "_"
tx_episode_files <- all_files[grepl(paste0("/", pop_prefix, "_"), all_files) & (pop_prefix != "PC" | !grepl("/PC_HOSP_", all_files))]

# List prevalence files
prevalence_files <- list.files(file.path(paths$D5_dir, "1.1_prevalence"), pattern = "\\.rds$", full.names = TRUE)

# Create a named vector where the key is the base name WITHOUT the '_prevalence' suffix
prevalence_files <- list.files(
  file.path(paths$D5_dir, "1.1_prevalence"),
  pattern = paste0("^", pop_prefix, "_.*\\.rds$"),
  full.names = TRUE
)

# Create a named vector mapping from base name (without _prevalence suffix) to file path
prevalence_map <- setNames(
  prevalence_files,
  sub("_prevalence$", "", tools::file_path_sans_ext(basename(prevalence_files)))
)


# Loop through each treatment episode file
for (epi in seq_along(tx_episode_files)) {
  
  # Read the treatment episode file
  dt <- readRDS(tx_episode_files[epi])

  # Remove the suffix '_treatment_episode' from file_name_raw
  file_name <- sub("_treatment_episode$", "", tools::file_path_sans_ext(basename(tx_episode_files[epi])))
  
  message("Processing: ", file_name)

  # Order episodes by person & start date
  setorder(dt, person_id, episode.start)
  
  # We lag the end of the previous episode and calculate the gap:
  # Get next episode start date
  dt[, next_start := shift(episode.start, type = "lead"), by = .(person_id, atc_group)]
  
  # Convert all dates to IDate
  dt[, c("episode.start", "episode.end", "exit_date", "next_start") := lapply(.SD, as.IDate), .SDcols = c("exit_date", "episode.start", "episode.end", "next_start")]
  
  # Flag discontinuation events
  dt[, discontinuer_flag := fifelse(is.na(next_start), (exit_date - episode.end > 120), (next_start - episode.end > 120))]
  
  # Keep only the discontinued episodes
  discontinuers <- dt[discontinuer_flag == TRUE]
  # Keep only episodes between entry and exit date
  discontinuers <- discontinuers[episode.end >= start_follow_up & episode.end <= end_follow_up,]
  
  if(nrow(discontinuers)>0){
    
    # Assign calendar year of each incident episode
    discontinuers[, year := year(episode.end)]
    
    # Remove duplicates: if person has multiple treatments (e.g., ATCs) in the same year, keep only one
    discontinuers <- unique(discontinuers, by = c("person_id", "year"))
    
    # Count number of unique treated persons per year (numerator for prevalence)
    discontinued_counts <- discontinuers[, .("N" = .N), by = year]
    
    # Now match treatment episode file_name with prevalence_map keys
    prevalence_file <- prevalence_map[file_name]
    
    if (!is.na(prevalence_file)) {prev_counts <- readRDS(prevalence_file)} else {warning(paste("No matching prevalence file found for:", file_name))}
    
    # Prepare prevalence counts
    prev_counts[,c("n_total", "rate", "rate_computable"):=NULL]
    # Rename columns
    setnames(prev_counts, "n_treated", "n_total")
    
    # Merge discontinued with prevalence
    discontinued_all <- merge(discontinued_counts, prev_counts, by = "year", all.y = TRUE)
    
    # Set N = 0 for years with no treatments
    discontinued_all[is.na(N), N := 0]
    
    # Calculate discontinued as a rate (*100)
    discontinued_all[, rate := round(100 * N / n_total, 3)][N == 0 & n_total == 0, rate := 0]
  
    if (nrow(discontinued_all[N > n_total]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Check rows where numerator > denominator
    problem1 <- discontinued_all[N > n_total]
    if (nrow(problem1) > 0) {
      print(red("Rows where numerator > denominator:"))
      print(problem1)
      fwrite(problem1, file.path(paths$D5_dir, "1.2_switching", paste0(pfx, "_num_gt_denominator.csv")))
    }
    
    # Check rows where denominator = 0 but numerator != 0
    problem2 <- discontinued_all[n_total == 0 & N != 0]
    if (nrow(problem2) > 0) {
      print(red("Rows where denominator = 0 but numerator != 0:"))
      print(problem2)
      fwrite(problem2, file.path(paths$D5_dir, "1.2_switching", paste0(pfx, "_denominator_zero_numerator_nonzero.csv")))
    }
    # Create column marking if rate is computable 
    discontinued_all[, rate_computable := !(n_total == 0 & N > 0)]
    
    # rename columns
    setnames(discontinued_all, "N", "n_treated")
    
    # Save dataset
    saveRDS(discontinuers, file.path(paths$D4_dir, "1.2_discontinued", paste0(file_name, "_discontinued_data.rds")))
    # Save results 
    saveRDS(discontinued_all, file.path(paths$D5_dir, "1.2_discontinued", paste0(file_name, "_discontinued.rds")))
  } else {
    message(red(paste("No discontinuers were found for:", file_name)))
  }
}








