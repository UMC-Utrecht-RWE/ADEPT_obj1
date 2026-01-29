###############################################################################################################################################################################
# <<< Sub-objective 1.1: Incidence rate >>>
# Measure: Annual incidence rate of ASM use
# Numerator: Number of individuals with >= 1 treatment episode of an ASM within a calendar year and without an overlapping treatment episode during the 1-year look-back period
# Denominator: Total number of person-time in that calendar year in the data source
# Stratification by: Individual drug substance, drug sub-groups, age groups, indication, calendar year, data source

###############################################################################################################################################################################
print("======================================================================================================")
print("========================= STRATIFYING INCIDENCE BY AGE GROUPS AND INDICATION =========================")
print("======================================================================================================")

# create folder for stratification counts
dir.create(file.path(paths$D5_dir, "1.1_incidence", "stratified"), showWarnings = FALSE, recursive = TRUE)

# get list of incidence files
files_incidence_episodes <- list.files(file.path(paths$D4_dir, "1.1_incidence"), pattern = "\\.rds$")

# filter for pop_prefix
files_incidence_episodes <- files_incidence_episodes[grepl(paste0("^", pop_prefix, "_"), files_incidence_episodes)]

# get a list of indication files
files_indication <- list.files(file.path(paths$D3_dir, "indication"), pattern = "\\.rds$", full.names = TRUE)

# filter for pop_prefix
files_indication <- files_indication[grepl(paste0("^", pop_prefix, "_"), basename(files_indication))]

# load and bind all indications into one dataset, remove true duplicates
dt_indication <- unique(rbindlist(lapply(files_indication, readRDS), use.names = TRUE, fill = TRUE))

# change value of column event_definition in any rows with O_NEUROPATHICPAIN_COV or O_FIBROMYALGIA_AESI to algorithm name O_NEUROPATHICPAINALG_COV
dt_indication[event_definition == "O_NEUROPATHICPAIN_COV" | event_definition == "O_FIBROMYALGIA_AESI", event_definition := "O_NEUROPATHICPAINALG_COV"]

# set strata levels
# age groups
age_levels <- c("12-18.99", "19-34.99", "35-54.99", "55-74.99", "75+", "UNKNOWN")

# indications
indication_levels <- c("M_RESTLESSLEG_COV", "Ment_ANXIETY_COV", "Ment_BIPOLAR_AESI", "Ment_DEPRESSION_COV", "Ment_SCHIZOPHRENIA_COV",
                       "N_CONVULSION_AESI", "N_EPILEPSY_COV", "N_ESSENTIALTREMOR_AESI", "N_MIGRAINE_COV", "O_NEUROPATHICPAINALG_COV", "UNKNOWN")

# Create vector of study years from study dates (exist in environment)
study_years <- seq(year(as.IDate(as.Date(start_study_date) + lookback_period)), year(as.IDate(end_study_date)))

# create empty data frame using all possible years from the study for counts
all_combinations_agegroups   <- CJ(year = study_years, age_group = age_levels, unique = TRUE)
all_combinations_indications <- CJ(year = study_years, indication = indication_levels, unique = TRUE)

# loop over files
for (episode in seq_along(files_incidence_episodes)) {
  # get name of file being processed currently
  file_name <- gsub("_incidence_data\\.rds$", "", files_incidence_episodes[episode])
  # print message
  message("Processing: ", file_name)
  # load current episode
  dt <- readRDS(file.path(paths$D4_dir, "1.1_incidence", files_incidence_episodes[episode]))
  # prepare denominator
  denom_counts <- dt[, .(Freq = .N), by = year]
  #<<< AGE GROUPS >>>#
  # age group calculation only in groups
  if (grepl("DP_ANTIEPINEW|DP_ANTIEPIOLD|DP_BENZOANTIEPILEPTIC|DP_GABAPENTINOIDS", files_incidence_episodes[episode])) {
    # create a copy of dt for age group calculations
    agegroups <- copy(dt)
    # convert dates to IDates
    agegroups[, birth_date := as.IDate(birth_date)][, episode.start := as.IDate(episode.start)]
    # create column - age at episode start
    agegroups[, age_at_episode_start := floor(time_length(interval(birth_date, episode.start), unit = "years"))]
    # create age groups
    agegroups[, age_group := fifelse(age_at_episode_start >= 12 & age_at_episode_start < 19, "12-18.99",
                                     fifelse(age_at_episode_start >= 19 & age_at_episode_start < 35, "19-34.99",
                                             fifelse(age_at_episode_start >= 35 & age_at_episode_start < 55, "35-54.99",
                                                     fifelse(age_at_episode_start >= 55 & age_at_episode_start < 75, "55-74.99",
                                                             fifelse(age_at_episode_start >= 75, "75+", "UNKNOWN")))))]
    # keep one row per person_id - episode.start
    agegroups <- unique(agegroups, by = c("person_id", "episode.start"))
    # count groups per year
    agegroup_counts <- agegroups[, .N, by = .(year, age_group)]
    # merge with empty data frame to get all years
    agegroup_counts <- merge(all_combinations_agegroups, agegroup_counts, by = c("year", "age_group"), all.x = TRUE)
    # if is.na(N), replace it with 0
    agegroup_counts[is.na(N), N := 0]
    # merge with denominator
    agegroup_counts <- merge(agegroup_counts, denom_counts, by = c("year"), all.x = TRUE)
    # if is.na(Freq), replace it with 0
    agegroup_counts[is.na(Freq), Freq := 0]
    # calculate rate, if N = 0 and Freq = 0 then change the rate to 0
    agegroup_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    # create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
    agegroup_counts[, rate_computable := Freq > 0]
    # sanity check
    # sum counts per year
    check_counts <- agegroup_counts[, .(sum_age_groups = sum(N), denominator = unique(Freq)), by = year]
    # check for equality
    check_counts[, match := sum_age_groups == denominator]
    # stop if any mismatch
    if (any(!check_counts$match)) {
      cat("\nError: Mismatch detected between numerator and denominator!\n")
      print(check_counts[match == FALSE])
      stop("Age Group counts do not add up to denominator for at least one year!")
    } else {
      message(blue("All age group counts match the denominator for every year"))
    }
    # save counts
    saveRDS(agegroup_counts, file.path(paths$D5_dir, "1.1_incidence", "stratified", paste0(file_name, "_incidence_agegroup_counts.rds")))
  }
  #<<< INDICATIONS >>>#
  # create a copy of dt for indication calculations
  dt_temp <- copy(dt)
  # prepare data for foverlaps
  # incidence file
  dt_temp[, start_window := as.IDate(as.Date(episode.start) %m-% lookback_period)]
  dt_temp[, end_window   := as.IDate(episode.start)]
  # indication file
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
  # Keep one row per person_id - episode.start
  indications <- unique(indications, by = c("person_id", "episode.start"))
  # count groups per year
  indication_counts <- indications[, .N, by = .(year, indication)]
  # merge counts with empty dt
  indication_counts <- merge(all_combinations_indications, indication_counts, by = c("year", "indication"), all.x = TRUE)
  # if is.na(N), replace it with 0
  indication_counts[is.na(N), N := 0]
  # merge with denominator
  indication_counts <- merge(indication_counts, denom_counts, by = c("year"), all.x = TRUE)
  # if is.na(Freq), replace it with 0
  indication_counts[is.na(Freq), Freq := 0]
  # calculate rate, if N = 0 and Freq = 0 then change the rate to 0
  indication_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
  # create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
  indication_counts[, rate_computable := Freq > 0]
  # sanity check
  # sum counts per year
  check_counts <- indication_counts[, .(sum_indications = sum(N), denominator = unique(Freq)), by = year]
  # Check for equality
  check_counts[, match := sum_indications == denominator]
  # Stop if any mismatch
  if (any(!check_counts$match)) {
    cat("\nError: Mismatch detected between numerator and denominator!\n")
    print(check_counts[match == FALSE])
    stop("Indication counts do not add up to denominator for at least one year!")
  } else {
    message(blue("All indication counts match the denominator for every year"))
  }
  # save counts
  saveRDS(indication_counts, file.path(paths$D5_dir, "1.1_incidence", "stratified", paste0(file_name, "_incidence_indication_counts.rds")))
}
