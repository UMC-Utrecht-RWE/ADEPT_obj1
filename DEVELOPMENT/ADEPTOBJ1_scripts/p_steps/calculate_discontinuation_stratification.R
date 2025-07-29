###############################################################################################################################################################################
# <<< Sub-objective 1.2: Discontinuation rate >>> 
# Measure: Annual discontinuation rate of ASM
# Numerator: Number of individuals who have a gap of ≥120 days between treatment episodes of an ASM in each calendar year
# Denominator: The number of prevalent ASM users in that calendar year in the data source 
# Stratification by: Individual drug substance, drug sub-groups, age groups, calendar year, data source

###############################################################################################################################################################################

print("=============================================================================================")
print("========================= STRATIFYING DISCONTINUATION BY AGE GROUPS =========================")
print("=============================================================================================")

# get list of discontinued files 
files_discontinued_episodes <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), pattern = "\\.rds$")

# filter for pop_prefix
files_discontinued_episodes <- files_discontinued_episodes[grepl(paste0("^", pop_prefix, "_"), files_discontinued_episodes)]

# if pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_discontinued_episodes <- files_discontinued_episodes[!grepl("PC_HOSP", files_discontinued_episodes)]

# create a folder for stratified counts
dir.create(file.path(paths$D5_dir, "1.2_discontinued", "stratified"), showWarnings = FALSE, recursive = TRUE)

# set stratification levels 
# age groups
age_levels <- c("12-18.99", "19-34.99", "35-54.99", "55-74.99", "75+", "UNKNOWN")

# create empty dt year for counts to include all possible combinations
all_years  <- seq(year(start_study_date), year(end_study_date))
all_combinations_agegroups   <- CJ(year = all_years, age_group = age_levels, unique = TRUE)

# loop over episodes
for(episode in seq_along(files_discontinued_episodes)){
  
  # print message
  message("Processing: ", gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode]))
  
  # load current episode
  dt <- readRDS(file.path(paths$D4_dir, "1.2_discontinued", files_discontinued_episodes[episode]))
  
  #<<< AGE GROUPS >>>#
  # convert dates to IDate 
  agegroups <- copy(dt)
  
  agegroups <- agegroups[, birth_date := as.IDate(birth_date)][, episode.end := as.IDate(episode.end)]
  
  # create column - age at episode end
  agegroups <- agegroups[, age_at_episode_end := floor(time_length(interval(birth_date, episode.end), unit = "years"))]

  # create age groups
  agegroups <- agegroups[, age_group := fifelse(age_at_episode_end >= 12 & age_at_episode_end < 19, "12-18.99",
                                                fifelse(age_at_episode_end >= 19 & age_at_episode_end < 35, "19-34.99",
                                                        fifelse(age_at_episode_end >= 35 & age_at_episode_end < 55, "35-54.99",
                                                                fifelse(age_at_episode_end >= 55 & age_at_episode_end < 75, "55-74.99",
                                                                        fifelse(age_at_episode_end >= 75, "75+", "UNKNOWN")))))]
  
  
  # extract year from group by date column - episode.end
  agegroups <- agegroups[, year := year(episode.end)]
 
  # Keep one row per person_id - episode.start
  agegroups <- unique(agegroups, by = c("person_id", "episode.start"))  
  
  # count groups per year
  agegroup_counts <- agegroups[, .N, by = .(year, age_group)]
  
  # merge counts with empty dt
  agegroup_counts <- merge(all_combinations_agegroups, agegroup_counts, by = c("year", "age_group"), all.x = TRUE)

  # if is.na(N), replace it with 0
  agegroup_counts <- agegroup_counts[is.na(N), N := 0]
  
  # calculate denominator per year 
  agegroup_counts <- agegroup_counts[, Freq := sum(N), by = year]
  
  # if is.na(Freq), replace it with 0
  agegroup_counts <- agegroup_counts[is.na(Freq), Freq := 0]
  
  # calculate rate, if N = 0 and Freq = 0 then change the rate to 0 
  agegroup_counts <- agegroup_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
  
  # create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
  agegroup_counts <- agegroup_counts[, rate_computable := Freq > 0]
  
  # save counts
  saveRDS(agegroup_counts, file.path(paths$D5_dir, "1.2_discontinued", "stratified", paste0(gsub("_discontinued_data\\.rds$", "_discontinued_agegroup_counts.rds", files_discontinued_episodes[episode]))))

}



