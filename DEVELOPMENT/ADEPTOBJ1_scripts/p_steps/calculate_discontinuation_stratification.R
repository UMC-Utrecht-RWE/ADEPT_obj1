###############################################################################################################################################################################
# <<< Sub-objective 1.2: Discontinuation rate >>>
# Measure: Annual discontinuation rate of ASM
# Numerator: Number of individuals who have a gap of >=120 days between treatment episodes of an ASM in each calendar year
# Denominator: The number of prevalent ASM users in that calendar year in the data source
# Stratification by: Individual drug substance, drug sub-groups, age groups, calendar year, data source

###############################################################################################################################################################################
print("=============================================================================================")
print("========================= STRATIFYING DISCONTINUATION BY AGE GROUPS =========================")
print("=============================================================================================")

# Create folder for stratification counts
dir.create(file.path(paths$D5_dir, "1.2_discontinued", "stratified"), showWarnings = FALSE, recursive = TRUE)

# Get list of discontinued files
files_discontinued_episodes <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), pattern = "\\.rds$")

# Filter for pop_prefix
files_discontinued_episodes <- files_discontinued_episodes[grepl(paste0("^", pop_prefix, "_"), files_discontinued_episodes)]

# Set strata levels
# Age groups
age_levels <- c("12-18.99", "19-34.99", "35-54.99", "55-74.99", "75+", "UNKNOWN")

# Create vector of study years from study dates (exist in environment)
study_years <- seq(year(as.IDate(as.Date(start_study_date) + lookback_period)), year(as.IDate(end_study_date)))

# Create empty data frame using all possible years from the study for counts
all_combinations_agegroups   <- CJ(year = study_years, age_group = age_levels, unique = TRUE)

# Loop over files
for (episode in seq_along(files_discontinued_episodes)) {
  
  # Get name of file being processed currently
  file_name <- gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode])
  
  # Print message
  message("Processing: ", file_name)
  
  # Load current episode
  dt <- readRDS(file.path(paths$D4_dir, "1.2_discontinued", files_discontinued_episodes[episode]))
  
  # Prepare denominator
  denom_counts <- dt[, .(Freq = .N), by = year]
  
  #<<< AGE GROUPS >>>#
  # Create a copy of dt for age group calculations
  agegroups <- copy(dt)
  
  # Convert dates to IDates
  agegroups[, birth_date := as.IDate(birth_date)][, episode.end := as.IDate(episode.end)]
  
  # Create column - age at episode end
  agegroups[, age_at_episode_end := floor(time_length(interval(birth_date, episode.end), unit = "years"))]
  
  # Create age groups
  agegroups[, age_group := fifelse(age_at_episode_end >= 12 & age_at_episode_end < 19, "12-18.99",
                                   fifelse(age_at_episode_end >= 19 & age_at_episode_end < 35, "19-34.99",
                                           fifelse(age_at_episode_end >= 35 & age_at_episode_end < 55, "35-54.99",
                                                   fifelse(age_at_episode_end >= 55 & age_at_episode_end < 75, "55-74.99",
                                                           fifelse(age_at_episode_end >= 75, "75+", "UNKNOWN")))))]
  
  # Keep one row per person_id - episode.start
  agegroups <- unique(agegroups, by = c("person_id", "episode.start"))
  
  # Count groups per year
  agegroup_counts <- agegroups[, .N, by = .(year, age_group)]
  
  # Merge with empty data frame to get all years
  agegroup_counts <- merge(all_combinations_agegroups, agegroup_counts, by = c("year", "age_group"), all.x = TRUE)
  
  # If is.na(N), replace it with 0
  agegroup_counts[is.na(N), N := 0]
  
  # Merge with denominator
  agegroup_counts <- merge(agegroup_counts, denom_counts, by = c("year"), all.x = TRUE)
  
  # If is.na(Freq), replace it with 0
  agegroup_counts[is.na(Freq), Freq := 0]
  
  # Calculate rate, if N = 0 and Freq = 0 then change the rate to 0
  agegroup_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
  
  # Create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
  agegroup_counts[, rate_computable := Freq > 0]
  
  # Sanity check
  # Sum counts per year
  check_counts <- agegroup_counts[, .(sum_age_groups = sum(N), denominator = unique(Freq)), by = year]
  
  # Check for equality
  check_counts[, match := sum_age_groups == denominator]
  
  # Stop if any mismatch
  if (any(!check_counts$match)) {
    cat("\nError: Mismatch detected between numerator and denominator!\n")
    print(check_counts[match == FALSE])
    stop("Age Group counts do not add up to denominator for at least one year!")
  } else {
    message(blue("All age group counts match the denominator for every year"))
  }
  
  # Save file 
  saveRDS(agegroup_counts, file.path(paths$D5_dir, "1.2_discontinued", "stratified", paste0(file_name, "_discontinued_agegroup_counts.rds")))
}
