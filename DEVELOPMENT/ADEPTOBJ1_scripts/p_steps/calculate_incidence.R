print("=========================================================================")
print("========================= CALCULATING INCIDENCE =========================")
print("=========================================================================")

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]}

# Load denominator file
denominator <- readRDS(file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))

# Loop through each treatment episode file
for (episode in seq_along(files_episodes)) {
  
  # Read the treatment episode file
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", "individual", files_episodes[episode]))
  
  # Print Message
  message("Processing: ", gsub("_treatment_episode\\.rds$", "", files_episodes[episode]))
  
  # Order episodes by person & start date
  setorder(dt, person_id, episode.start)
  
  # Flag “incident” episodes: an episode is incident if the gap since the previous episode’s end ≥ 365 days
  dt[, prev_end       := shift(episode.end, 1, type = "lag"), by = .(person_id)]
  dt[, gap_since_prev := as.numeric(as.IDate(episode.start) - as.IDate(prev_end))]
  dt[, incident_flag  := is.na(prev_end) | gap_since_prev > 365]
  
  # Keep only incident episodes
  incidence <- dt[incident_flag == TRUE]
  
  # Drop any incident use if episode.start is before start_follow_up
  incidence <- incidence[episode.start >= start_follow_up & episode.end <= end_follow_up,]
  
  # Perform Counts 
  if(nrow(incidence)>0){
    
    # Assign calendar year of each incident episode based on episode start
    incidence[, year := year(episode.start)]
    
    # Remove duplicates: Keep only one person id per year
    incidence <- unique(incidence, by = c("person_id", "year"))
    
    # Count number of incidence 
    incidence_counts <- incidence[, .("N" = .N), by = year]
    
    # Merge with denominator to calculate incidence; include all years even if no treatments (all.y = TRUE)
    incidence_all <- merge(incidence_counts, denominator, by = "year", all.y = TRUE)
    
    # Set N = 0 for years with no treatments
    incidence_all[is.na(N), N := 0]
    
    # Calculate incidence per 1000 person
    incidence_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(incidence_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(incidence_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Save data with odd values 
    if(nrow(incidence_all[N > Freq])>0) fwrite(incidence_all[N > Freq], file.path(paths$D5_dir, "1.1_incidence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_num_gt_denominator.csv")))
    if(nrow(incidence_all[Freq == 0 & N != 0])>0) fwrite(incidence_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.1_incidence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_denominator_zero_numerator_nonzero.csv")))
    
    # Create column marking if rate is computable i.e. if numerator is greater than denominator or if both numerator and denominator = 0
    incidence_all[, rate_computable := !(Freq == 0 & N >= 0)]
    
    # Rename columns
    setnames(incidence_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save dataset 
    saveRDS(incidence, file.path(paths$D4_dir, "1.1_incidence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_incidence_data.rds")))
    
    # Save results 
    saveRDS(incidence_all, file.path(paths$D5_dir, "1.1_incidence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_incidence.rds")))
    
  } else {
    
    message(red(paste("No incidence counts for:", files_episodes[episode])))
    
  }
}