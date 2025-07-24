print("=========================================================================================")
print("========================= STRATIFYING POLYTHERAPY BY AGE GROUPS =========================")
print("=========================================================================================")

# get list of polytherapy files 
files_polytherapy_episodes <- list.files(file.path(paths$D4_dir, "1.2_polytherapy"), pattern = "\\.rds$")

# filter for pop_prefix
files_polytherapy_episodes <- files_polytherapy_episodes[grepl(paste0("^", pop_prefix, "_"), files_polytherapy_episodes)]

# if pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_polytherapy_episodes <- files_polytherapy_episodes[!grepl("PC_HOSP", files_polytherapy_episodes)]

# create a folder for stratified counts
dir.create(file.path(paths$D5_dir, "1.2_polytherapy", "stratified"), showWarnings = FALSE, recursive = TRUE)

# set stratification levels 
# age groups
age_levels <- c("12-18.99", "19-34.99", "35-54.99", "55-74.99", "75+", "UNKNOWN")

# create empty dt year for counts to include all possible combinations
all_years  <- seq(year(start_study_date), year(end_study_date))
all_combinations_agegroups   <- CJ(year = all_years, age_group = age_levels, unique = TRUE)

# loop over episodes
for(episode in seq_along(files_polytherapy_episodes)){
  
  # print message
  message("Processing: ", gsub("_polytherapy_data\\.rds$", "", files_polytherapy_episodes[episode]))
  
  # load current episode
  dt <- readRDS(file.path(paths$D4_dir, "1.2_polytherapy", files_polytherapy_episodes[episode]))
  
  #<<< AGE GROUPS >>>#
  
  # convert dates to IDate 
  agegroups <- dt[, birth_date := as.IDate(birth_date)][, overlap_start := as.IDate(overlap_start)]
  
  # create column - age at overlap start
  agegroups <- agegroups[, age_at_overlap_start := floor(time_length(interval(birth_date, overlap_start), unit = "years"))]

  # create age groups
  agegroups <- agegroups[, age_group := fifelse(age_at_overlap_start >= 12 & age_at_overlap_start < 19, "12-18.99",
                                                fifelse(age_at_overlap_start >= 19 & age_at_overlap_start < 35, "19-34.99",
                                                        fifelse(age_at_overlap_start >= 35 & age_at_overlap_start < 55, "35-54.99",
                                                                fifelse(age_at_overlap_start >= 55 & age_at_overlap_start < 75, "55-74.99",
                                                                        fifelse(age_at_overlap_start >= 75, "75+", "UNKNOWN")))))]
  

  # extract year from group by date column - overlap_start
  agegroups <- agegroups[, year := year(overlap_start)]
  
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
  saveRDS(agegroup_counts, file.path(paths$D5_dir, "1.2_polytherapy", "stratified",paste0(gsub("_polytherapy_data\\.rds$", "_polytherapy_agegroup_counts.rds", files_polytherapy_episodes[episode]))))
  
}



