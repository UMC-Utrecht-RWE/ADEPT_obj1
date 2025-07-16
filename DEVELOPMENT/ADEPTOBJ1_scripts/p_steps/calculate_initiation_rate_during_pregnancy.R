###############################################################################################################################################################################
# <<< Sub-objective 1.3: Initiation rate during pregnancy >>> 
# Measure: Annual initiation rate of ASM during pregnancy
# Numerator: Number of pregnancies in a calendar year with ≥1 treatment episode of an ASM during any trimester, but no treatment episode in the 12 months prior to pregnancy start
# Denominator: Total number of pregnancies in that calendar year in the data source
# Stratification by: Overall, individual drug substance, drug sub-groups, age groups, indication, calendar year, data source

# Pending: Stratification by Overall, age groups, indication
###############################################################################################################################################################################

print("==========================================================================================")
print("==================== CALCULATING ASM INITIATION RATE DURING PREGNANCY ====================")
print("==========================================================================================")

# List all incident treatment episode files matching population prefix
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = "\\.rds$")

# Keep only files that match population prefix AND contain "_F_" (female patients)
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes) & grepl("_F_", files_episodes)]

# Drop PC_HOSP files if pop_prefix is PC
if(pop_prefix == "PC") files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]

# Load pregnancies file
load(file.path(preg_dir, "D3_pregnancy_final.RData"))
pregnancies <- as.data.table(D3_pregnancy_final)

# Remove duplicates
pregnancies <- unique(pregnancies)

# Convert pregnancy dates to IDate
pregnancies[, pregnancy_start_date := as.IDate(pregnancy_start_date)]
pregnancies[, pregnancy_end_date   := as.IDate(pregnancy_end_date)]

# Add pregnancy start year
pregnancies[, preg_year := year(pregnancy_start_date)]

# Set key for joining
setkey(pregnancies, person_id)

# Create vector of study years from your study dates (must exist in environment)
study_years <- seq(year(start_study_date), year(end_study_date))

# Create template table with all years zeroed
template_years <- data.table(preg_year = study_years)

# Calculate total pregnancies per year (denominator)
total_preg_by_year <- pregnancies[, .(Freq = uniqueN(pregnancy_id)), by = preg_year]

# Loop through each treatment episode file
for (episode in seq_along(files_episodes)) {
  
  # Get name of current ASM
  treatment_name <- gsub("_treatment_episode\\.rds$", "", files_episodes[episode])
  
  # Print Message
  message("Processing treatment: ", treatment_name)
  
  # Load treatment episodes
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", "individual", files_episodes[episode]))
  
  # Convert episode dates to IDate
  dt[, episode.start := as.IDate(episode.start)][, episode.end := as.IDate(episode.end)]
  
  # Remove true duplicates
  dt <- unique(dt)
  
  # Set key for joining
  setkey(dt, person_id)
  
  # Merge treatment episode with pregnancies file on person id
  dt_all <- dt[pregnancies, on = .(person_id), nomatch = 0]
  
  # First, sort by person and episode.start to ensure order
  setorder(dt_all, person_id, episode.start)
  
  # Create a new column with the previous episode.end per person
  dt_all[, prior_episode_end := shift(episode.end), by = person_id]
  
  # Keep episodes overlapping pregnancy
  dt_all <- dt_all[episode.start <= pregnancy_end_date & episode.end >= pregnancy_start_date]
  
  # Exclude pregnancies where any episode started or ended within the 12 months before pregnancy star
  dt_all <- dt_all[pregnancy_start_date - prior_episode_end > 365 | is.na(prior_episode_end), ]
  
  # Get list of unique ids 
  preg_ids_all <- unique(dt_all$pregnancy_id)
  
  # Check if any pre-pregnancy ASM use was found 
  if(nrow(dt_all)>0){
    
    # Count the number of pregnancies with ASM use in the third trimester, grouped by pregnancy year
    initiation_rate_counts <- pregnancies[pregnancy_id %in% preg_ids_all, .N, by = preg_year]
    
    # Merge with template to get all years 
    initiation_rate_all <- merge(template_years[, .(preg_year)], initiation_rate_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    initiation_rate_all <- merge(initiation_rate_all, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    initiation_rate_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate rates
    initiation_rate_all[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Create column marking if rate is computable 
    initiation_rate_all[, rate_computable := Freq > 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(initiation_rate_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(initiation_rate_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Save data where odd values 
    if(nrow(initiation_rate_all[N > Freq])>0) fwrite(initiation_rate_all[N > Freq], file.path(paths$D5_dir, "1.3_initiation_rate_during_pregnancy", treatment_name, "_num_gt_denominator.csv"))
    if(nrow(initiation_rate_all[Freq == 0 & N != 0])>0) fwrite(initiation_rate_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_initiation_rate_during_pregnancy", treatment_name, "_denominator_zero_numerator_nonzero.csv"))
    
    # Rename columns 
    setnames(initiation_rate_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save files 
    saveRDS(dt_all, file = file.path(paths$D4_dir, "1.3_initiation_rate_during_pregnancy", paste0(treatment_name, "_initiation_rate_all_trimester_data.rds")))
    saveRDS(initiation_rate_all, file = file.path(paths$D5_dir, "1.3_initiation_rate_during_pregnancy", paste0(treatment_name, "_initiation_rate_all_trimester_counts.rds")))
    
  } else {
    message(red(paste0("There was no ASM initiation of ", treatment_name, " in all trimesters")))
  } 
}