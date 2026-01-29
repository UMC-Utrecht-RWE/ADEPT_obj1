print("================================================================================================================")
print("========================= CALCULATING INDICATIONS AND COMORBIDITIES OF BASE POPULATION =========================")
print("================================================================================================================")

# create folders for counts
dir.create(file.path(paths$D5_dir, "baseline_tables", "indication_counts"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(paths$D5_dir, "baseline_tables", "comorbidity_counts"), showWarnings = FALSE, recursive = TRUE)

# List all exposure files excluding subgroups
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS") # subgroups for exclusion
files_exposures <- list.files(file.path(paths$D3_dir, "exposure"), pattern = "\\.rds$", full.names = FALSE) #list files in exposure folder
files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)] # keep files of current pop prefix
files_exposures <- files_exposures[!grepl(paste(exclude, collapse = "|"), files_exposures)] # Exclude subgroups

# Create dataset with all exposure medications
dt_exposures <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "exposure", files_exposures), readRDS), fill = TRUE)) # read and bind exposures

#################### FLOWCHART ####################

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  
  # Keep one row per person_id, code, rx_date
  dt_exposures <- unique(dt_exposures, by = c("person_id", "code", "rx_date"))
  
  # Remove any prescriptions outside start and end follow up 
  dt_exposures <- dt_exposures[rx_date >= start_follow_up & rx_date <= end_follow_up, ]
  
  #<<< For Flow chart >>>##
  total_population_base_cohort                           <- uniqueN(study_population$person_id) #total population (base cohort)
  nr_unique_persons_with_rx_between_startfu_and_endfu    <- uniqueN(dt_exposures$person_id)
  nr_unique_persons_without_rx_between_startfu_and_endfu <- total_population_base_cohort - nr_unique_persons_with_rx_between_startfu_and_endfu
  
  flow_data <- data.table(
    step = c(
      paste0("total_population_base_cohort-Study Population - ", sex_label),
      paste0("nr_unique_persons_with_rx_between_startfu_and_endfu - ", sex_label),
      paste0("nr_unique_persons_without_rx_between_startfu_and_endfu - ", sex_label)
    ),
    count = c(
      total_population_base_cohort,
      nr_unique_persons_with_rx_between_startfu_and_endfu,
      nr_unique_persons_without_rx_between_startfu_and_endfu
    )
  )
  
  # save table
  saveRDS(flow_data, file.path(paths$D5_dir, "flowcharts", paste0(pop_prefix, "_study_pop_to_ASM_users_flowchart.rds")))
  
  # Prepare data for indication and comorbidity counts
  setorder(dt_exposures, person_id, rx_date) # sort by person id and rx_date
  dt_exposures <- dt_exposures[, .SD[1], by = person_id] # keep first prescription for each person id
  
} else {
  
  # Keep one row per pregnancy_id, code, rx_date
  dt_exposures <- unique(dt_exposures, by = c("pregnancy_id", "code", "rx_date"))
  
  #<<< For Flow chart >>>#
  total_pregnancies_base_cohort    <- uniqueN(study_population$pregnancy_id) # total pregnancies (base cohort)
  nr_unique_pregnancies_with_rx    <- uniqueN(dt_exposures$pregnancy_id)
  nr_unique_pregnancies_without_rx <- total_pregnancies_base_cohort - nr_unique_pregnancies_with_rx
  
  flow_data <- data.table(
    step = c(
      paste0("total_pregnancies_base_cohort-Study Population"),
      paste0("nr_unique_pregnancies_with_rx"),
      paste0("nr_unique_pregnancies_without_rx")
    ),
    count = c(
      total_pregnancies_base_cohort,
      nr_unique_pregnancies_with_rx,
      nr_unique_pregnancies_without_rx
    )
  )
  
  # save table
  saveRDS(flow_data, file.path(paths$D5_dir, "flowcharts", paste0(pop_prefix, "_study_pop_to_ASM_users_flowchart.rds")))
  
  # Prepare data for indication and comorbidity counts
  setorder(dt_exposures, pregnancy_id, rx_date) # sort by pregnancy id and rx_date
  dt_exposures <- dt_exposures[, .SD[1], by = pregnancy_id] # keep first prescription for each pregnancy id 
}

#################### INDICATION COUNTS ####################

# List indication files
files_indication <- list.files(file.path(paths$D3_dir, "indication"), pattern = "\\.rds$", full.names = FALSE) #list files in indications folder
files_indication <- files_indication[grepl(paste0("^", pop_prefix, "_"), files_indication)] # keep files of current pop prefix
files_indication <- files_indication[!grepl(paste(exclude, collapse = "|"), files_indication)] # Exclude subgroups

# Create dataset with all indications
dt_indication <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "indication", files_indication), readRDS), fill = TRUE)) # read and bind datasets
dt_indication <- unique(dt_indication) # remove true duplicates

# Make a copy of exposures
dt_exposures_temp <- copy(dt_exposures)

# Get indication groups from bridge
indication_lookup <- bridge[indication==TRUE, .(Varname)]
# Merge with algorithm map to get algorithm components 
indication_lookup <- algorithm_map[, .(VariableName, Algorithm)][indication_lookup, on = .(Algorithm = Varname), allow.cartesian = TRUE]
# If not an algorithm, use same name as Variable name 
indication_lookup[is.na(VariableName), VariableName := Algorithm]
# Rename columns 
setnames(indication_lookup, c("VariableName", "Algorithm"), c("event_definition", "indication_group"))

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  
  # Create windows - Exposures
  dt_exposures_temp[, start_window := as.IDate(as.Date(rx_date) %m-% lookback_period)]
  dt_exposures_temp[, end_window := rx_date - 1][, end.window := as.IDate(end_window)]
  
  # Create windows - Indications
  dt_indication[, start_event := as.IDate(event_date)][, end_event := as.IDate(event_date)]
  
  # Rename columns 
  setnames(dt_exposures_temp, c("code", "rx_date"), c("exposure_ATC", "exposure_rx_date"))
  
  # Set keys
  setkey(dt_exposures_temp, person_id, start_window, end_window)
  setkey(dt_indication, person_id, start_event, end_event)
  
  # Overlap join between exposures and indications - within windows 
  indication_in_lookback <- foverlaps(dt_exposures_temp[, .(person_id, exposure_ATC, exposure_rx_date, start_window, end_window)],
                                      dt_indication[, .(person_id, code, event_date, event_definition, start_event, end_event)],
                                      by.x = c("person_id", "start_window", "end_window"),
                                      by.y = c("person_id", "start_event", "end_event"),
                                      nomatch = 0
  )
  
  # Add column indication group - use indication look-up
  indication_in_lookback <- indication_lookup[indication_in_lookback, on = "event_definition"]
  
  # Rename columns 
  setnames(indication_in_lookback, c("event_date", "event_definition"), c("indication_date", "indication"))
  
  # Deduplicate to one row per person per indication group
  indication_in_lookback <- unique(indication_in_lookback, by = c("person_id", "indication_group"))
  
  # Count unique persons having each indication 
  indication_counts <- indication_in_lookback[, .(indication_counts = uniqueN(person_id)), by = indication_group]
  
  # Add column with total users
  indication_counts[, total_users := nr_unique_persons_with_rx_between_startfu_and_endfu]
  
  # Calculate rates
  indication_counts[, rates := round(indication_counts/total_users, 3)][, rates_perc := rates * 100]
  
  # Save counts
  saveRDS(indication_counts, file.path(paths$D5_dir,  "baseline_tables", "indication_counts", paste0(pop_prefix, "_indication_counts.rds")))
  
} else {
  
  # Create windows - Exposures
  if(deap_flags$is_EFEMERIS){
    dt_exposures_temp[, start_window := as.IDate(op_start_date)] # 2.5 months before pregnancy start 
    dt_exposures_temp[, end_window   := as.IDate(op_end_date)]   # pregnancy end 
  }
  
  if(deap_flags$is_FIN_REG){
    dt_exposures_temp[, start_window := as.IDate(add_with_rollback(pregnancy_start_date, years(-1)))] # 1 year before pregnancy start
    dt_exposures_temp[, end_window   := as.IDate(pregnancy_end_date)] # pregnancy end 
  }
  
  # Create windows - Indications
  dt_indication[, start_event := as.IDate(event_date)][, end_event   := as.IDate(event_date)]
  
  # Rename columns
  setnames(dt_exposures_temp, c("code", "rx_date"), c("exposure_ATC", "exposure_rx_date"))
  
  # Set keys
  setkey(dt_exposures_temp, pregnancy_id, start_window, end_window)
  setkey(dt_indication, pregnancy_id, start_event, end_event)
  
  # Overlap join between exposures and indications - within windows 
  indication_in_lookback <- foverlaps(dt_exposures_temp[, .(person_id, pregnancy_id, exposure_ATC, exposure_rx_date, start_window, end_window)],
                                      dt_indication[, .(person_id, pregnancy_id, code, event_date, event_definition, start_event, end_event)],
                                      by.x = c("pregnancy_id", "start_window", "end_window"),
                                      by.y = c("pregnancy_id", "start_event", "end_event"),
                                      nomatch = 0
  )
  
  # Add column indication group - use indication look-up
  indication_in_lookback <- indication_lookup[indication_in_lookback, on = "event_definition"]
  
  # Rename columns 
  setnames(indication_in_lookback, c("event_date", "event_definition"), c("indication_date", "indication"))
  
  # Deduplicate to one row per person per indication group
  indication_in_lookback <- unique(indication_in_lookback, by = c("pregnancy_id", "indication_group"))
  
  # Count unique persons having each indication 
  indication_counts <- indication_in_lookback[, .(indication_counts = uniqueN(pregnancy_id)), by = indication_group]
  
  # Add column with total pregnancies
  indication_counts[, total_pregnancies := nr_unique_pregnancies_with_rx]
  
  # Calculate rates
  indication_counts[, rates := round(indication_counts/total_pregnancies, 3)][, rates_perc := rates * 100]
  
  # Save counts
  saveRDS(indication_counts, file.path(paths$D5_dir, "baseline_tables", "indication_counts", paste0(pop_prefix, "_indication_counts.rds")))
}

#################### COMORBIDITY COUNTS ####################

# List comorbidity files
files_comorbidities <- list.files(file.path(paths$D3_dir, "cov"), pattern = "\\.rds$", full.names = FALSE) #list files in comorbidity folder
files_comorbidities <- files_comorbidities[grepl(paste0("^", pop_prefix, "_"), files_comorbidities)] # keep files of current pop prefix
files_comorbidities <- files_comorbidities[!grepl(paste(exclude, collapse = "|"), files_comorbidities)] # Exclude subgroups

# Create dataset with all comorbidities - ATC codes and Dx codes in separate datasets
dt_comorbidity_meds <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "cov", files_comorbidities[ grepl("_med\\.rds$", files_comorbidities)]), readRDS), use.names = TRUE, fill = TRUE)) # read and bind datasets
dt_comorbidity_dx   <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "cov", files_comorbidities[!grepl("_med\\.rds$", files_comorbidities)]), readRDS), use.names = TRUE, fill = TRUE)) # read and bind datasets
dt_comorbidity_meds <- unique(dt_comorbidity_meds) # remove true duplicates
dt_comorbidity_dx   <- unique(dt_comorbidity_dx) # remove true duplicates

# Make a copy of exposures
dt_exposures_temp <- copy(dt_exposures)

# Get comorbidity groups from bridge
comorbidity_lookup <- bridge[cov==TRUE, .(Varname)]
# Merge with algorithm map to get algorithm components 
comorbidity_lookup <- algorithm_map[, .(VariableName, Algorithm)][comorbidity_lookup, on = .(Algorithm = Varname), allow.cartesian = TRUE]
# If not an algorithm, use same name as Variable name 
comorbidity_lookup[is.na(VariableName), VariableName := Algorithm]
# Rename columns 
setnames(comorbidity_lookup, c("VariableName", "Algorithm"), c("event_definition", "comorbidity_group"))

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  
  # Create windows - Exposures
  dt_exposures_temp[, start_window := as.IDate(as.Date(rx_date) %m-% lookback_period)]
  dt_exposures_temp[, end_window := rx_date - 1][, end.window := as.IDate(end_window)]
  
  # Create windows - Comorbidities
  dt_comorbidity_dx[, start_event := as.IDate(event_date)][, end_event := as.IDate(event_date)]
  dt_comorbidity_meds[, start_event := as.IDate(rx_date)][, end_event := as.IDate(rx_date)]
  
  # Rename columns 
  setnames(dt_exposures_temp, c("code", "rx_date"), c("exposure_ATC", "exposure_rx_date"))
  
  # Set keys
  setkey(dt_exposures_temp, person_id, start_window, end_window)
  setkey(dt_comorbidity_dx, person_id, start_event, end_event)
  setkey(dt_comorbidity_meds, person_id, start_event, end_event)
  
  # Overlap join between exposures and comorbidities - within windows 
  comorbidity_dx_in_lookback <- foverlaps(dt_exposures_temp[, .(person_id, exposure_ATC, exposure_rx_date, start_window, end_window)],
                                          dt_comorbidity_dx[, .(person_id, code, event_date, event_definition, start_event, end_event)],
                                          by.x = c("person_id", "start_window", "end_window"),
                                          by.y = c("person_id", "start_event", "end_event"),
                                          nomatch = 0
  )
  comorbidity_meds_in_lookback <- foverlaps(dt_exposures_temp[, .(person_id, exposure_ATC, exposure_rx_date, start_window, end_window)],
                                            dt_comorbidity_meds[, .(person_id, code, rx_date, Varname, start_event, end_event)],
                                            by.x = c("person_id", "start_window", "end_window"),
                                            by.y = c("person_id", "start_event", "end_event"),
                                            nomatch = 0
  )
  
  # Add column indication group - use indication look-up
  comorbidity_dx_in_lookback <- comorbidity_lookup[comorbidity_dx_in_lookback, on = "event_definition"]
  comorbidity_meds_in_lookback <- comorbidity_lookup[comorbidity_meds_in_lookback, on = c("event_definition" = "Varname")]
  
  # Rename columns prior to binding both dx and meds comorbidity files
  setnames(comorbidity_dx_in_lookback, c("event_date", "event_definition"), c("comorbidity_date", "comorbidity"))
  setnames(comorbidity_meds_in_lookback, c("rx_date", "event_definition"), c("comorbidity_date", "comorbidity"))
  
  # Bind the two data sets
  all_comorbidities_in_lookback <- rbindlist(list(comorbidity_dx_in_lookback, comorbidity_meds_in_lookback), use.names = TRUE, fill = TRUE)
  
  # Deduplicate to one row per person per comorbidity group
  all_comorbidities_in_lookback <- unique(all_comorbidities_in_lookback, by = c("person_id", "comorbidity_group"))
  
  # Count unique people having each comorbidity
  comorbidity_counts <- all_comorbidities_in_lookback[, .(comorbidity_counts = uniqueN(person_id)), by = comorbidity_group]
  
  # Add column with total new users
  comorbidity_counts[, total_users := nr_unique_persons_with_rx_between_startfu_and_endfu]
  
  # Calculate rates
  comorbidity_counts[, rates := round(comorbidity_counts/total_users, 3)][, rates_perc := rates * 100]

  # Save counts
  saveRDS(comorbidity_counts, file.path(paths$D5_dir, "baseline_tables", "comorbidity_counts", paste0(pop_prefix, "_comorbidity_counts.rds")))
  
} else {
  
  # Create windows - Exposures
  if(deap_flags$is_EFEMERIS){
    dt_exposures_temp[, start_window := as.IDate(op_start_date)] # 2.5 months before pregnancy start 
    dt_exposures_temp[, end_window   := as.IDate(op_end_date)]   # pregnancy end 
  }
  
  if(deap_flags$is_FIN_REG){
    dt_exposures_temp[, start_window := as.IDate(add_with_rollback(pregnancy_start_date, years(-1)))] # 1 year before pregnancy start
    dt_exposures_temp[, end_window   := as.IDate(pregnancy_end_date)] # pregnancy end 
  }
  
  # Create windows - Comorbidities
  dt_comorbidity_dx[, start_event := as.IDate(event_date)][, end_event := as.IDate(event_date)]
  dt_comorbidity_meds[, start_event := as.IDate(rx_date)][, end_event := as.IDate(rx_date)]
  
  # Rename columns 
  setnames(dt_exposures_temp, c("code", "rx_date"), c("exposure_ATC", "exposure_rx_date"))
  
  # set keys
  setkey(dt_exposures_temp, pregnancy_id, start_window, end_window)
  setkey(dt_comorbidity_dx, pregnancy_id, start_event, end_event)
  setkey(dt_comorbidity_meds, pregnancy_id, start_event, end_event)
  
  # Overlap join between exposures and comorbidities - within windows 
  comorbidity_dx_in_lookback <- foverlaps(dt_exposures_temp[, .(person_id, pregnancy_id, exposure_ATC, exposure_rx_date, start_window, end_window)],
                                          dt_comorbidity_dx[, .(person_id, pregnancy_id, code, event_date, event_definition, start_event, end_event)],
                                          by.x = c("pregnancy_id", "start_window", "end_window"),
                                          by.y = c("pregnancy_id", "start_event", "end_event"),
                                          nomatch = 0
  )
  comorbidity_meds_in_lookback <- foverlaps(dt_exposures_temp[, .(person_id, pregnancy_id, exposure_ATC, exposure_rx_date, start_window, end_window)],
                                            dt_comorbidity_meds[, .(person_id, pregnancy_id, code, rx_date, Varname, start_event, end_event)],
                                            by.x = c("pregnancy_id", "start_window", "end_window"),
                                            by.y = c("pregnancy_id", "start_event", "end_event"),
                                            nomatch = 0
  )
  
  # Add column indication group - use indication look-up
  comorbidity_dx_in_lookback <- comorbidity_lookup[comorbidity_dx_in_lookback, on = "event_definition"]
  comorbidity_meds_in_lookback <- comorbidity_lookup[comorbidity_meds_in_lookback, on = c("event_definition" = "Varname")]
  
  # Rename columns prior to binding both dx and meds comorbidity files
  setnames(comorbidity_dx_in_lookback, c("event_date", "event_definition"), c("comorbidity_date", "comorbidity"))
  setnames(comorbidity_meds_in_lookback, c("rx_date", "event_definition"), c("comorbidity_date", "comorbidity"))
  
  # Bind the two data sets
  all_comorbidities_in_lookback <- rbindlist(list(comorbidity_dx_in_lookback, comorbidity_meds_in_lookback), use.names = TRUE, fill = TRUE)
  
  # Deduplicate to one row per person per comorbidity group
  all_comorbidities_in_lookback <- unique(all_comorbidities_in_lookback, by = c("pregnancy_id", "comorbidity_group"))
  
  # Count unique people having each comorbidity
  comorbidity_counts <- all_comorbidities_in_lookback[, .(comorbidity_counts = uniqueN(pregnancy_id)), by = comorbidity_group]
  
  # Add column with total pregnancies
  comorbidity_counts[, total_pregnancies := nr_unique_pregnancies_with_rx]
  
  # Calculate rates
  comorbidity_counts[, rates := round(comorbidity_counts/total_pregnancies, 3)][, rates_perc := rates * 100]
  
  # Save counts
  saveRDS(comorbidity_counts, file.path(paths$D5_dir, "baseline_tables", "comorbidity_counts", paste0(pop_prefix, "_comorbidity_counts.rds")))
}

#################### ASM BASELINE TABLE ####################

# Calculate Stats for ASM population
dt_exposures_temp <- copy(dt_exposures)

# Make sure all dates are IDate
dt_exposures_temp[, (c("birth_date", "start_follow_up", "end_follow_up")) := lapply(.SD, as.IDate), .SDcols = c("birth_date", "start_follow_up", "end_follow_up")]

# Calculate follow-up time in days
dt_exposures_temp[, followup_days := as.numeric(difftime(end_follow_up, start_follow_up, units = "days"))]

# Calculate age at start_follow_up
dt_exposures_temp[, age_at_start_follow_up := floor(as.numeric(difftime(start_follow_up, birth_date, units = "days")) / 365.25)]

# Create age groups
dt_exposures_temp[, age_group := fifelse(age_at_start_follow_up >= 12 & age_at_start_follow_up < 19, "12-18.99",
                                         fifelse(age_at_start_follow_up >= 19 & age_at_start_follow_up < 35, "19-34.99",
                                                 fifelse(age_at_start_follow_up >= 35 & age_at_start_follow_up < 55, "35-54.99",
                                                         fifelse(age_at_start_follow_up >= 55 & age_at_start_follow_up < 75, "55-74.99",
                                                                 fifelse(age_at_start_follow_up >= 75, "75+", NA_character_)))))]


# 2. summary statistics

# Calculates median of followup in years
fu_median      <- median(dt_exposures_temp$followup_days) / 365.25
fu_IQR         <- IQR(dt_exposures_temp$followup_days) / 365.25
fu_min         <- min(dt_exposures_temp$followup_days) / 365.25
fu_max         <- max(dt_exposures_temp$followup_days) / 365.25
max_endfu_date <- max(dt_exposures_temp$end_follow_up)

# Mean Age
age_at_start_fu_mean <- mean(dt_exposures_temp$age_at_start_follow_up)
age_at_start_fu_SD   <- sd(dt_exposures_temp$age_at_start_follow_up)

# Counts Per Age_Group
age_group_12_18.99_count <- sum(dt_exposures_temp$age_group == "12-18.99", na.rm = TRUE)
age_group_19_34.99_count <- sum(dt_exposures_temp$age_group == "19-34.99", na.rm = TRUE)
age_group_35_54.99_count <- sum(dt_exposures_temp$age_group == "35-54.99", na.rm = TRUE)
age_group_55_74.99_count <- sum(dt_exposures_temp$age_group == "55-74.99", na.rm = TRUE)
age_group_above_75_count <- sum(dt_exposures_temp$age_group == "75+", na.rm = TRUE)

# Calculates percentages
age_group_12_18.99_perc <- (age_group_12_18.99_count / nrow(dt_exposures_temp)) * 100
age_group_19_34.99_perc <- (age_group_19_34.99_count / nrow(dt_exposures_temp)) * 100
age_group_35_54.99_perc <- (age_group_35_54.99_count / nrow(dt_exposures_temp)) * 100
age_group_55_74.99_perc <- (age_group_55_74.99_count / nrow(dt_exposures_temp)) * 100
age_group_above_75_perc <- (age_group_above_75_count / nrow(dt_exposures_temp)) * 100

# Create Baseline Table
names <- c("Follow-up, years - median",
           "Follow-up, years - IQR",
           "Follow-up, years - min",
           "Follow-up, years - max",
           "Max end-fu date",
           "Age at start fu - mean",
           "Age at start fu - sd",
           "age_group_12_18.99_count",
           "age_group_12_18.99_perc",
           "age_group_19_34.99_count",
           "age_group_19_34.99_perc",
           "age_group_35_54.99_count",
           "age_group_35_54.99_perc",
           "age_group_55_74.99_count",
           "age_group_55_74.99_perc",
           "age_group_above_75_count",
           "age_group_above_75_perc")

values <- c(as.character(round(fu_median, 2)),
            as.character(round(fu_IQR, 2)),
            as.character(round(fu_min, 2)),
            as.character(round(fu_max, 2)),
            as.character(max_endfu_date),
            as.character(round(age_at_start_fu_mean, 2)),
            as.character(round(age_at_start_fu_SD, 2)),
            as.character(age_group_12_18.99_count),
            as.character(round(age_group_12_18.99_perc, 2)),
            as.character(age_group_19_34.99_count),
            as.character(round(age_group_19_34.99_perc, 2)),
            as.character(age_group_35_54.99_count),
            as.character(round(age_group_35_54.99_perc, 2)),
            as.character(age_group_55_74.99_count),
            as.character(round(age_group_55_74.99_perc, 2)),
            as.character(age_group_above_75_count),
            as.character(round(age_group_above_75_perc, 2))
)

# Join names and values
baseline_table <- data.table(names, values)

# Save baseline table
saveRDS(baseline_table, file.path(paths$D5_dir, "baseline_tables", paste0(pop_prefix, "_ASM_users_baseline_table.rds")))
