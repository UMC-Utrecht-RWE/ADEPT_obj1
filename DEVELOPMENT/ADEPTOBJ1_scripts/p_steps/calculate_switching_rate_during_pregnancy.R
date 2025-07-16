###############################################################################################################################################################################
# <<< Sub-objective 1.4: Switching rate during pregnancy >>> 
# Measure: Switching rate from one ASM to a different ASM or to an alternative medication during pregnancy
# Numerator: The number of pre-pregnancy users of an ASM within a calendar year that switched to a different ASM or alternative medication during the pregnancy period
# Denominator: Total number of pre-pregnancy users of an ASM in a calendar year in the data source 
# Stratification by: Overall, individual drug substance, drug sub-groups, indication, calendar year, data source

# Pending: Individual drug substance, calendar year, data source

# Conditions: 
### Pre-pregnancy users
### 
###############################################################################################################################################################################
print("===============================================================================================")
print("========================= CALCULATING SWITCHING RATE DURING PREGNANCY =========================")
print("===============================================================================================")

# List all treatment episode files matching population prefix
files_episodes <- list.files(file.path(paths$D4_dir, "1.3_pre-pregnancy_use_rate"), pattern = "_12_0_window_data\\.rds$")

# Keep only files that match population prefix AND contain "_F_" (female patients)
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# Drop PC_HOSP files if pop_prefix is PC
if(pop_prefix == "PC") files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]

# Read in treatment episode data sets for switchers



# Create vector of study years from your study dates (must exist in environment)
study_years <- seq(year(start_study_date), year(end_study_date))

# Create template table with all years zeroed
template_years <- data.table(preg_year = study_years)

# Calculate total pregnancies per year (denominator)
total_preg_by_year <- pregnancies[, .(Freq = uniqueN(pregnancy_id)), by = preg_year]

# Loop through each treatment episode file
for (episode in seq_along(files_episodes)) {
  
  # Get name of current ASM
  treatment_name <- gsub("_pre_pregnancy_use_rate_.*_window_data\\.rds$", "", files_episodes[episode])
  
  # Print Message
  message("Processing: ", treatment_name)
  
  # Load treatment episodes
  dt <- readRDS(file.path(paths$D4_dir, "1.3_pre-pregnancy_use_rate", files_episodes[episode]))
  
  # Convert episode dates to IDate
  dt[, episode.start := as.IDate(episode.start)][, episode.end := as.IDate(episode.end)]
  
  # Remove true duplicates
  dt <- unique(dt)
  
  # Set key for joining
  setkey(dt, person_id)
}