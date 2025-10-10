###############################################################################################################################################################################
# <<< Sub-objective 1.3: Initiation rate during pregnancy >>>
# Measure: Annual initiation rate of ASM during pregnancy
# Numerator: Number of pregnancies in a calendar year with ≥1 treatment episode of an ASM during any trimester, but no treatment episode in the 12 months prior to pregnancy start
# Denominator: Total number of pregnancies in that calendar year in the data source
# Stratification by: Individual drug substance, drug sub-groups, age groups, indication, calendar year, data source
###############################################################################################################################################################################

print("======================================================================================================================")
print("========================= STRATIFYING PREGNANCY INITIATION RATE BY AGE GROUPS AND INDICATION =========================")
print("======================================================================================================================")

# create a folder for stratified counts
dir.create(file.path(paths$D5_dir, "1.3_pregnancy_initiation", "stratified"), showWarnings = FALSE, recursive = TRUE)

# Get initiation during pregnancy files
files_preg_init_episodes <- list.files(file.path(paths$D4_dir, "1.3_pregnancy_initiation"), pattern = "\\.rds$")

# Get indication files
files_indication <- list.files(file.path(paths$D3_dir, "indication"), pattern = "\\.rds$", full.names = TRUE)
if(deap_flags$is_EFEMERIS || deap_flags$is_FIN_REG) files_indication <- files_indication[grepl(paste0("^", pop_prefix, "_"), basename(files_indication))]
if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) files_indication <- files_indication[grepl(paste0("^", pop_prefix, "_F"), basename(files_indication))]

# load and bind all indications into one dataset
dt_indication <- rbindlist(lapply(files_indication, readRDS), use.names = TRUE, fill = TRUE)
dt_indication <- unique(dt_indication) # remove true duplicates

# change column event_definition in any rows with O_NEUROPATHICPAIN_COV or O_FIBROMYALGIA_AESI to algorithm name O_NEUROPATHICPAINALG_COV
dt_indication[event_definition== "O_NEUROPATHICPAIN_COV" | event_definition=="O_FIBROMYALGIA_AESI", event_definition:="O_NEUROPATHICPAINALG_COV"]

# set levels
# age groups - until 55 because these are only females
age_levels <- c("12-18.99", "19-34.99", "35-54.99", "UNKNOWN")

# indications
indication_levels <- c("M_RESTLESSLEG_COV", "Ment_ANXIETY_COV", "Ment_BIPOLAR_AESI", "Ment_DEPRESSION_COV", "Ment_SCHIZOPHRENIA_COV",
                       "N_CONVULSION_AESI", "N_EPILEPSY_COV", "N_ESSENTIALTREMOR_AESI", "N_MIGRAINE_COV", "O_NEUROPATHICPAINALG_COV", "UNKNOWN")

# create empty dt year for counts to include all possible combinations
# Create vector of study years from study dates (exist in environment)
all_combinations_agegroups   <- CJ(preg_year = study_years, age_group = age_levels, unique = TRUE)
all_combinations_indications <- CJ(preg_year = study_years, indication = indication_levels, unique = TRUE)

# loop over initiation files
for(episode in seq_along(files_preg_init_episodes)){
  
    # print message
    message("Processing: ", sub("_initiation_rates.*$", "", files_preg_init_episodes[episode]))

    # load current episode
    dt <- readRDS(file.path(paths$D4_dir, "1.3_pregnancy_initiation", files_preg_init_episodes[episode]))

    # prepare denominator
    denom_counts <- dt[, .(Freq = .N), by = preg_year]

    #<<< AGE GROUPS >>>#
    agegroups <- copy(dt)

    # Change date columns to IDate
    agegroups[, birth_date := as.IDate(birth_date)][, pregnancy_start_date := as.IDate(pregnancy_start_date)]

    # create column - age at pregnancy start
    agegroups[, age_at_pregnancy_start := floor(time_length(interval(birth_date, pregnancy_start_date), unit = "years"))]

    # create age groups
    agegroups[, age_group := fifelse(age_at_pregnancy_start >= 12 & age_at_pregnancy_start < 19, "12-18.99",
                                     fifelse(age_at_pregnancy_start >= 19 & age_at_pregnancy_start < 35, "19-34.99",
                                             fifelse(age_at_pregnancy_start >= 35 & age_at_pregnancy_start < 55, "35-54.99", "UNKNOWN")))]

    # Keep one row per pregnancy_id
    agegroups <- unique(agegroups, by = c("pregnancy_id"))

    # count groups per pregnancy start year
    agegroup_counts <- agegroups[, .N, by = .(preg_year, age_group)]

    # merge counts with all_combo template
    agegroup_counts <- merge(all_combinations_agegroups, agegroup_counts, by = c("preg_year", "age_group"), all.x = TRUE)

    # if is.na(N), replace it with 0
    agegroup_counts[is.na(N), N := 0]

    # Merge with denominator
    agegroup_counts <- merge(agegroup_counts, denom_counts, by = c("preg_year"))

    # if is.na(Freq), replace it with 0
    agegroup_counts[is.na(Freq), Freq := 0]

    # calculate rate, if N = 0 and Freq = 0 then change the rate to 0
    agegroup_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]

    # create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
    agegroup_counts[, rate_computable := Freq > 0]

    # sanity check
    # Sum counts per year
    check_counts <- agegroup_counts[, .(sum_age_groups = sum(N), denominator = unique(Freq)), by = preg_year]

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

    # save counts
    saveRDS(agegroup_counts, file.path(paths$D5_dir, "1.3_pregnancy_initiation", "stratified", paste0(sub("_initiation_rates.*$", "", files_preg_init_episodes[episode]), "_initiation_rates_in_pregnancy_agegroup_counts.rds")))

    #<<< INDICATIONS >>>#
    dt_temp <- copy(dt)
    
    # Set Windows
    # non-pregnancy only DEAPs
    #TODO - NEED TO CHECK WITH VISA - FINLAND
    if(!deap_flags$is_EFEMERIS) dt_temp[, start_window := as.IDate(as.Date(episode.start) %m-% lookback_period)][, end_window := episode.start]
    if(deap_flags$is_EFEMERIS)  dt_temp[, start_window := as.IDate(as.Date(pregnancy_start_date))][, end_window := as.IDate(as.Date(pregnancy_end_date))]
    dt_indication[, start_event := event_date][, end_event := event_date]
   
    if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
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
      indications <- indications[,.(person_id, pregnancy_id, event_date, code, event_definition, episode.start, i.code, atc_group, sex_at_instance_creation,
                                    birth_date, start_follow_up, end_follow_up, entry_date, exit_date, pregnancy_start_date, preg_year)]
      
    } else {
      
      # set keys
      setkey(dt_temp, pregnancy_id, start_window, end_window)
      setkey(dt_indication, pregnancy_id, start_event, end_event)
      
      # perform overlap join
      indications <- foverlaps(dt_temp,
                               dt_indication[, .(pregnancy_id, event_date, code, coding_system, event_definition, start_event, end_event)],
                               by.x = c("pregnancy_id", "start_window", "end_window"),
                               by.y = c("pregnancy_id", "start_event", "end_event"),
                               nomatch = NA
      )
      
      # drop unnecessary columns
      indications <- indications[,.(person_id, pregnancy_id, event_date, code, event_definition, episode.start, i.code, atc_group, sex_at_instance_creation,
                                    birth_date, start_follow_up, end_follow_up, entry_date, exit_date, pregnancy_start_date, preg_year)]
      
      
    }
    
    # calculate difference in days between episode start and event date of indication
    if(!deap_flags$is_EFEMERIS) indications[, diff_days := as.numeric(difftime(episode.start, event_date, units = "days"))]
    if(deap_flags$is_EFEMERIS)  indications[, diff_days := abs(as.numeric(difftime(episode.start, event_date, units = "days")))]
    
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
      by = .(pregnancy_id)
    ]

    # Keep one row per pregnancy_id
    indications <- unique(indications, by = c("pregnancy_id"))

    # COMMON TO BOTH
    # count groups per year
    indication_counts <- indications[, .N, by = .(preg_year, indication)]

    # merge counts with empty dt
    indication_counts <- merge(all_combinations_indications, indication_counts, by = c("preg_year", "indication"), all.x = TRUE)

    # if is.na(N), replace it with 0
    indication_counts[is.na(N), N := 0]

    # Merge with denominator
    indication_counts <- merge(indication_counts, denom_counts, by = c("preg_year"))

    # if is.na(Freq), replace it with 0
    indication_counts[is.na(Freq), Freq := 0]

    # calculate rate, if N = 0 and Freq = 0 then change the rate to 0
    indication_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]

    # create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
    indication_counts[, rate_computable := Freq > 0]

    # Sum counts per year
    check_counts <- indication_counts[, .(sum_indications = sum(N), denominator = unique(Freq)), by = preg_year]

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

    # change column name back to year (in the first submission the column name was year, to avoid issues in post processing I change it back to year)
    setnames(indication_counts, "preg_year", "year")
    
    # save counts
    saveRDS(indication_counts, file.path(paths$D5_dir, "1.3_pregnancy_initiation", "stratified", paste0(sub("_initiation_rates.*$", "", files_preg_init_episodes[episode]), "_initiation_rates_in_pregnancy_indication_counts.rds")))
    
}


