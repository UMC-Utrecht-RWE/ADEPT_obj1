###############################################################################################################################################################################
# <<< Sub-objective 1.1: Prevalence rate >>> 
# Measure: Annual prevalence rate of ASM use
# Numerator: Number of individuals with ≥1 treatment episode of an ASM within a calendar year 
# Denominator: Total number of individuals in that calendar year in the data source
# Stratification by: Individual drug substance, drug sub-groups, age groups, indication, calendar year, data source

###############################################################################################################################################################################

print("=======================================================================================================")
print("========================= STRATIFYING PREVALENCE BY AGE GROUPS AND INDICATION =========================")
print("=======================================================================================================")

# get list of [incidence files prevalence files
files_prevalence_episodes <- list.files(file.path(paths$D4_dir, "1.1_prevalence"), pattern = "\\.rds$")

# filter for pop_prefix
files_prevalence_episodes <- files_prevalence_episodes[grepl(paste0("^", pop_prefix, "_"), files_prevalence_episodes)]

# if pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_prevalence_episodes <- files_prevalence_episodes[!grepl("PC_HOSP", files_prevalence_episodes)]

# list all files in the indication folder
files_indication <- list.files(file.path(paths$D3_dir, "indication"), pattern = "\\.rds$", full.names = TRUE)

# filter for pop_prefix
files_indication <- files_indication[grepl(paste0("^", pop_prefix, "_"), basename(files_indication))]

# if pop_prefix is PC, then drop any that are PC_HOSP
if (pop_prefix == "PC") files_indication <- files_indication[!grepl("PC_HOSP", basename(files_indication))]

# load and bind all indications into one dataset
dt_indication <- rbindlist(lapply(files_indication, readRDS), use.names = TRUE, fill = TRUE)

# remove any true duplicates
dt_indication <- unique(dt_indication)

# create a folder for stratified counts
dir.create(file.path(paths$D5_dir, "1.1_prevalence", "stratified"), showWarnings = FALSE, recursive = TRUE)

# set stratification levels 
# age groups
age_levels <- c("12-18.99", "19-34.99", "35-54.99", "55-74.99", "75+", "UNKNOWN")

# indications
indication_levels <- c("M_RESTLESSLEG_COV", "Ment_ANXIETY_COV", "Ment_BIPOLAR_AESI", "Ment_DEPRESSION_COV", "Ment_SCHIZOPHRENIA_COV",
                       "N_CONVULSION_AESI", "N_EPILEPSY_COV", "N_ESSENTIALTREMOR_AESI", "N_MIGRAINE_COV", "O_NEUROPATHICPAINALG_COV", "UNKNOWN")

# create empty dt year for counts to include all possible combinations
all_years  <- seq(year(start_study_date), year(end_study_date))
all_combinations_agegroups   <- CJ(year = all_years, age_group = age_levels, unique = TRUE)
all_combinations_indications <- CJ(year = all_years, indication = indication_levels, unique = TRUE)

# loop over episodes
for(episode in seq_along(files_prevalence_episodes)){
  
  # print message
  message("Processing: ", gsub("_prevalence_data\\.rds$", "", files_prevalence_episodes[episode]))
  
  # load current episode
  dt <- readRDS(file.path(paths$D4_dir, "1.1_prevalence", files_prevalence_episodes[episode]))
  
  #<<< AGE GROUPS >>>#
  if (grepl("DP_ANTIEPINEW|DP_ANTIEPIOLD|DP_BENZOANTIEPILEPTIC|DP_GABAPENTINOIDS", files_prevalence_episodes[episode])) {
    
    # convert dates to IDate 
    agegroups <- copy(dt)
    
    agegroups[, birth_date := as.IDate(birth_date)][, jan1 := as.IDate(paste0(year, "-01-01"))]
    
    # create column - age at Jan 1 of treatment year 
    agegroups[, age_at_start_of_year := floor(time_length(interval(birth_date, jan1), unit = "years"))]
    
    # create age groups
    agegroups[, age_group := fifelse(age_at_start_of_year >= 12 & age_at_start_of_year < 19, "12-18.99",
                                                  fifelse(age_at_start_of_year >= 19 & age_at_start_of_year < 35, "19-34.99",
                                                          fifelse(age_at_start_of_year >= 35 & age_at_start_of_year < 55, "35-54.99",
                                                                  fifelse(age_at_start_of_year >= 55 & age_at_start_of_year < 75, "55-74.99",
                                                                          fifelse(age_at_start_of_year >= 75, "75+", "UNKNOWN")))))]
    
    
    # extract year from group by date column - this is already in the dataset as year
    
    # Keep one row per person_id - episode.start - year 
    agegroups <- unique(agegroups, by = c("person_id", "episode.start", "year"))
    
    # count groups per year
    agegroup_counts <- agegroups[, .N, by = .(year, age_group)]
    
    # merge counts with empty dt
    agegroup_counts <- merge(all_combinations_agegroups, agegroup_counts, by = c("year", "age_group"), all.x = TRUE)
    
    # if is.na(N), replace it with 0
    agegroup_counts[is.na(N), N := 0]
    
    # calculate denominator per year 
    agegroup_counts[, Freq := sum(N), by = year]
    
    # if is.na(Freq), replace it with 0
    agegroup_counts[is.na(Freq), Freq := 0]
    
    # calculate rate, if N = 0 and Freq = 0 then change the rate to 0 
    agegroup_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
    agegroup_counts[, rate_computable := Freq > 0]
    
    # save counts
    saveRDS(agegroup_counts, file.path(paths$D5_dir, "1.1_prevalence", "stratified", paste0(gsub("_prevalence_data\\.rds$", "_prevalence_agegroup_counts.rds", files_prevalence_episodes[episode]))))
  }
  #<<< INDICATIONS >>>#
  
  dt_temp <- copy(dt)
  # prepare data for foverlaps
  # prevalence episodes
  dt_temp[, start_window := as.IDate(as.Date(episode.start) %m-% lookback_period)]
  dt_temp[, end_window   := as.IDate(episode.start)]
  
  # indication data
  dt_indication[, start_event := as.IDate(event_date)]
  dt_indication[, end_event   := as.IDate(event_date)]
  
  # set keys 
  setkey(dt_temp, person_id, start_window, end_window)
  setkey(dt_indication, person_id, start_event, end_event)
  
  # perform overlap join 
  indications <- foverlaps(dt_temp, 
                           dt_indication[, .(person_id, event_date, code, coding_system, event_definition, start_event, end_event)], 
                           by.x = c("person_id", "start_window", "end_window"),
                           by.y = c("person_id", "start_event", "end_event"), 
                           nomatch = NA
  )
  
  # drop unnecessary columns
  indications <- indications[, .SD, .SDcols = c("person_id", "event_date", "code",  "event_definition", "episode.start", "episode.end", "i.code", "atc_group",
                                                "sex_at_instance_creation", "birth_date", "start_follow_up", "end_follow_up", "entry_date", "exit_date",
                                                "start_year", "end_year")]
  
  # calculate difference in days between episode start and event date of indication 
  indications[, diff_days := as.numeric(difftime(episode.start, event_date, units = "days"))]
  
  # create column indication: 
  # if more than one rx is present, and epilepsy is among them, then priority is epilepsy
  # if any other rx are present, pick the one closest to episode.start
  # if one rx is present then that is the indication
  # if no indication is present within the 365 days before, then indication is unknown
  
  indications <- indications[
    , {
      if ("N_EPILEPSY_COV" %in% event_definition) {
        row <- .SD[event_definition == "N_EPILEPSY_COV"][1]
        row[, indication := "N_EPILEPSY_COV"]
        row
      } else if (all(is.na(event_date)) & all(is.na(code))) {
        row <- .SD[1]
        row[, indication := "UNKNOWN"]
        row
      } else {
        row <- .SD[which.min(diff_days)]
        row[, indication := row$event_definition]
        row
      }
    },
    by = .(person_id, episode.start)
  ]
  
  # order dataset by person id and episode start
  setorder(indications, person_id, episode.start)
  
  # expand dataset to get all prevalence years
  indications_expanded <- indications[, 
                                      { 
                                        years    <- seq(year(episode.start), year(episode.end))
                                        repeated <- .SD[rep(1L, length(years))]
                                        repeated[, year := years]
                                        repeated
                                      }, by = .(person_id, episode.start)]
  
  
  # Remove prevalence that falls outside start and end follow up
  indications_expanded <- indications_expanded[year >= year(start_follow_up) & year <= year(end_follow_up),]
  
  # Keep only unique person_id - episode.start - year combinations
  indications_expanded <- unique(indications_expanded, by = c("person_id", "episode.start", "year"))
  
  # count groups per year
  indication_counts <- indications_expanded[, .N, by = .(year, indication)]
  
  # merge counts with empty dt
  indication_counts <- merge(all_combinations_indications, indication_counts, by = c("year", "indication"), all.x = TRUE)
  
  # if is.na(N), replace it with 0
  indication_counts[is.na(N), N := 0]
  
  # calculate denominator per year 
  indication_counts[, Freq := sum(N), by = year]
  
  # if is.na(Freq), replace it with 0
  indication_counts[is.na(Freq), Freq := 0]
  
  # calculate rate, if N = 0 and Freq = 0 then change the rate to 0 
  indication_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
  
  # create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
  indication_counts[, rate_computable := Freq > 0]
  
  # save counts
  saveRDS(indication_counts, file.path(paths$D5_dir, "1.1_prevalence", "stratified", paste0(gsub("_prevalence_data\\.rds$", "_prevalence_indication_counts.rds", files_prevalence_episodes[episode]))))
  
}


