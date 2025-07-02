print("=============================================================================================")
print("========================= CALCULATING INCIDENCE - INDIVIDUAL GROUPS =========================")
print("=============================================================================================")

# List all .rds files (full paths)
all_files <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = "\\.rds$", full.names = TRUE)
# Filter to keep only files starting with pop_prefix + "_"
tx_episode_files <- all_files[grepl(paste0("/", pop_prefix, "_"), all_files) & (pop_prefix != "PC" | !grepl("/PC_HOSP_", all_files))]


# Load the denominator file
denominator <- readRDS(file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))

# Rename columns 
setnames(denominator, "Year", "year")

# Loop through each treatment episode file
for (epi in seq_along(tx_episode_files)) {
  
  # Read the treatment episode file
  dt <- readRDS(tx_episode_files[epi])
  
  # Extract the base file name (without path and extension) for labeling output
  # Remove the suffix '_treatment_episode' from file_name_raw
  file_name <- sub("_treatment_episode$", "", tools::file_path_sans_ext(basename(tx_episode_files[epi])))
  
  message("Processing: ", file_name)
  
  # Order episodes by person & start date
  setorder(dt, person_id, episode.start)
  
  # Flag “incident” episodes: an episode is incident if the gap since the previous episode’s end ≥ 365 days
  # dt[, prev_end := shift(episode.end, 1, type = "lag"), by = .(person_id, ATC)]
  dt[, prev_end := shift(episode.end, 1, type = "lag"), by = .(person_id)]
  dt[, gap_since_prev := as.numeric(as.IDate(episode.start) - as.IDate(prev_end))]
  dt[, incident_flag := is.na(prev_end) | gap_since_prev > 365]
  
  # Keep only the incident episodes
  incidents <- dt[incident_flag == TRUE]
  
  # Drop any incident use if episode.start is before start_follow_up
  incidents <- incidents[episode.start >= start_follow_up & episode.end <= end_follow_up,]
  
  if(nrow(incidents)>0){
    # Assign calendar year of each incident episode
    incidents[, year := year(episode.start)]
    
    # Remove duplicates: if person has multiple treatments (e.g., ATCs) in the same year, keep only one
    incidents <- unique(incidents, by = c("person_id", "year"))
    
    # Count number of unique treated persons per year (numerator for prevalence)
    incidence_counts <- incidents[, .("N" = .N), by = year]
    
    # Merge with denominator to calculate prevalence; include all years even if no treatments (all.y = TRUE)
    incidence_all <- merge(incidence_counts, denominator, by = "year", all.y = TRUE)
    
    # Set N = 0 for years with no treatments
    incidence_all[is.na(N), N := 0]
    
    # Calculate incidence per 1000 person
    incidence_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(incidence_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(incidence_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Create column marking if rate is computable i.e. if numerator is greater than denominator
    incidence_all[, rate_computable := !(Freq == 0 & N > 0)]
    
    # Rename columns
    setnames(incidence_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save dataset 
    saveRDS(incidents, file.path(paths$D4_dir, "1.1_incidence", paste0(file_name, "_incidence_data.rds")))
    
    # Save results 
    saveRDS(incidence_all, file.path(paths$D5_dir, "1.1_incidence", paste0(file_name, "_incidence.rds")))
  } else {
    message(red(paste("No incident users were found for:", file_name)))
  }
}