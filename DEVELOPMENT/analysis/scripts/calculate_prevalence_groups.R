print("==============================================================================================")
print("========================= CALCULATING PREVALENCE - SUBGROUPS =========================")
print("==============================================================================================")

# List all treatment episode .rds files from the tx_episodes directory
tx_episode_files <- list.files(file.path(paths$D3_dir, "tx_episodes", "groups"), pattern = "\\.rds$", full.names = TRUE)

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
  
  # Extract year ranges from episode start and end dates
  dt[, start_year := year(episode.start)]
  dt[, end_year := year(episode.end)]
  
  # Generate one row per person-year-ATC
  # dt_expanded <- dt[, .(year = unlist(Map(seq, start_year, end_year))), by = .(person_id, ATC)]
  dt_expanded <- dt[, {
    years <- seq(start_year, end_year)
    repeated <- .SD[rep(1L, length(years))]
    repeated[, year := years]
    repeated
  }, by = .I]
  
  # Remove prevalence that falls outside start and end follow up
  dt_expanded <- dt_expanded[year>=year(start_follow_up) & year<=year(end_follow_up),]
  
  # Remove duplicates: if person has multiple treatments (e.g., ATCs) in the same year, keep only one
  dt_expanded <- unique(dt_expanded, by = c("person_id", "year"))

  # Count number of unique treated persons per year (numerator for prevalence)
  prevalence_counts <- dt_expanded[, .N, by = year]
  
  # Merge with denominator to calculate prevalence; include all years even if no treatments (all.y = TRUE)
  prevalence_all <- merge(prevalence_counts, denominator, by = "year", all.y = TRUE)
  
  # Set N = 0 for years with no treatments
  prevalence_all[is.na(N), N := 0]
  
  # Calculate prevalence per 1000 person
  prevalence_all[, rate := fifelse(Freq == 0, NA_real_, round(1000 * N / Freq, 3))]
  
  # Create column marking if rate is computable 
  prevalence_all[, rate_computable := Freq != 0]
  
  # Rename columns 
  setnames(prevalence_all, c("N", "Freq"), c("n_treated", "n_total"))
  
  # Save dataset 
  saveRDS(dt_expanded, file.path(paths$D4_dir, "1.1_prevalence", paste0(file_name, "_prevalence_data.rds")))
  
  # Save results 
  saveRDS(prevalence_all, file.path(paths$D5_dir, "1.1_prevalence", paste0(file_name, "_prevalence.rds")))
  
}
