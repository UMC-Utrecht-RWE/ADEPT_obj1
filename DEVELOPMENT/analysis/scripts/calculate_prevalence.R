# TODO Prevalence not being counted correctly when treatment episode has multiple unique ATC

# List all treatment episode .rds files from the tx_episodes directory
tx_episode_files <- list.files(file.path(paths$D3_dir, "tx_episodes"), pattern = "\\.rds$", full.names = TRUE)

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
  
  # Extract year ranges from episode start and end dates
  dt[, start_year := year(episode.start)]
  dt[, end_year := year(episode.end)]
  
  # Generate one row per person-year (e.g., 2020, 2021 if a person is treated both years)
  person_years <- dt[, .(year = unlist(Map(seq, start_year, end_year))), by = person_id]
  
  # Remove duplicates: if person has multiple treatments (e.g., ATCs) in the same year, keep only one
  person_years <- unique(person_years, by = c("person_id", "year"))
  
  # Count number of unique treated persons per year (numerator for prevalence)
  prevalence_counts <- person_years[, .N, by = year]
  
  # Merge with denominator to calculate prevalence; include all years even if no treatments (all.y = TRUE)
  prevalence_all <- merge(prevalence_counts, denominator, by = "year", all.y = TRUE)
  
  # Set N = 0 for years with no treatments
  prevalence_all[is.na(N), N := 0]
  
  # Calculate prevalence per 1000 person
  prevalence_all[, rate := round(N / Freq * 1000, 3)]
  
  # Rename columns 
  setnames(prevalence_all, c("N", "Freq"), c("n_treated", "n_total"))
  
  # Save results 
  saveRDS(prevalence_all, file.path(paths$D4_dir, "1.1_prevalence", paste0(file_name, "_prevalence.rds")))
  
}
