print("===================================================================================")
print("========================= CALCULATING PREVALENCE - GROUPS =========================")
print("===================================================================================")

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "groups"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]}

# Load denominator file
denominator <- readRDS(file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))

# Loop through each treatment episode file
for (episode in seq_along(files_episodes)) {
  
  # Read the treatment episode file
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", "groups", files_episodes[episode]))
  
  # Print Message
  message("Processing: ", gsub("_treatment_episode\\.rds$", "", files_episodes[episode]))
  
  # Order episodes by person & start date
  setorder(dt, person_id, episode.start)
  
  # Extract year ranges from episode start and end dates
  dt[, start_year := year(episode.start)]
  dt[, end_year   := year(episode.end)]
  
  # Generate one row per person-year-ATC
  dt_expanded <- dt[, 
                    { 
                      years    <- seq(start_year, end_year)
                      repeated <- .SD[rep(1L, length(years))]
                      repeated[, year := years]
                      repeated
                    }, by = .I]
  
  # Remove prevalence that falls outside start and end follow up
  dt_expanded <- dt_expanded[year >= year(start_follow_up) & year <= year(end_follow_up),]
  
  if(nrow(dt_expanded)>0){
    
    # Remove duplicates: Keep only one person id per year
    dt_expanded <- unique(dt_expanded, by = c("person_id", "year"))
    
    # Count number of unique treated persons per year (numerator for prevalence)
    prevalence_counts <- dt_expanded[, .N, by = year]
    
    # Merge with denominator to calculate prevalence; include all years even if no treatments (all.y = TRUE)
    prevalence_all <- merge(prevalence_counts, denominator, by = "year", all.y = TRUE)
    
    # Set N = 0 for years with no treatments
    prevalence_all[is.na(N), N := 0]
    
    # Calculate prevalence per 1000 person
    prevalence_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(prevalence_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(prevalence_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Save data where odd values 
    if(nrow(prevalence_all[N > Freq])>0) fwrite(prevalence_all[N > Freq], file.path(paths$D5_dir, "1.1_prevalence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_num_gt_denominator.csv")))
    if(nrow(prevalence_all[Freq == 0 & N != 0])>0) fwrite(prevalence_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.1_prevalence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_denominator_zero_numerator_nonzero.csv")))
    
    # Create column marking if rate is computable 
    prevalence_all[, rate_computable := !(Freq == 0 & N >= 0)]
    
    # Rename columns 
    setnames(prevalence_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save dataset 
    saveRDS(dt_expanded, file.path(paths$D4_dir, "1.1_prevalence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_prevalence_data.rds")))
    
    # Save results 
    saveRDS(prevalence_all, file.path(paths$D5_dir, "1.1_prevalence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_prevalence.rds")))
    
  } else {
    
    message(red(paste("No prevalent users were found for:", files_episodes[episode])))
    
  }
}