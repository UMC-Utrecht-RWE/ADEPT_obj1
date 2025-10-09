print("================================================================================================================")
print("========================= CALCULATING INDICATIONS AND COMORBIDITIES OF BASE POPULATION =========================")
print("================================================================================================================")

# create folders for counts
dir.create(file.path(paths$D5_dir, "baseline_tables","indication_counts"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(paths$D5_dir, "baseline_tables","comorbidity_counts"), showWarnings = FALSE, recursive = TRUE)

# List all exposure files excluding subgroups 
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS") # subgroups for exclusion
files_exposures <- list.files(file.path(paths$D3_dir, "exposure"), pattern = "\\.rds$", full.names = FALSE) #list files in exposure folder
files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)] # keeo files of current pop prefix
files_exposures <- files_exposures[!grepl(paste(exclude, collapse = "|"), files_exposures)] # Exclude subgroups
if (pop_prefix == "PC") files_exposures <- files_exposures[!grepl("PC_HOSP", files_exposures)] #BIFAP

# Create dataset with all exposure medications
dt_exposures <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "exposure", files_exposures), readRDS),fill = TRUE)) # read and bind datasets
dt_exposures <- unique(dt_exposures) # remove true duplicates 

# keep only prescriptions between start and end fu
dt_exposures <- dt_exposures[rx_date>=start_follow_up & rx_date<=end_follow_up,]

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG){
  
  #<<< For Flow chart >>>## 
  total_population_base_cohort  <- uniqueN(study_population$person_id) #total population (base cohort)
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
  saveRDS(flow_data, file.path(paths$D5_dir,"flowcharts" ,paste0(pop_prefix, "_study_pop_to_ASM_users_flowchart.rds")))
  
  # sort per person id
  setorder(dt_exposures, person_id, rx_date)
  
  # Keep only the first prescription per person
  dt_exposures <- dt_exposures[, .SD[1], by = person_id]
  
} else {

  #<<< For Flow chart >>>#
  total_pregnancies_base_cohort <- uniqueN(study_population$pregnancy_id) # base cohort
  nr_unique_pregnancies_with_rx_between_startfu_and_endfu    <- uniqueN(dt_exposures$pregnancy_id)
  nr_unique_pregnancies_without_rx_between_startfu_and_endfu <- total_pregnancies_base_cohort - nr_unique_pregnancies_with_rx_between_startfu_and_endfu
  
  
  flow_data <- data.table(
    step = c(
      paste0("total_pregnancies_base_cohort-Study Population"),
      paste0("nr_unique_pregnancies_with_rx_between_startfu_and_endfu"),
      paste0("nr_unique_pregnancies_without_rx_between_startfu_and_endfu")
      
    ),
    count = c(
      total_pregnancies_base_cohort,
      nr_unique_pregnancies_with_rx_between_startfu_and_endfu,
      nr_unique_pregnancies_without_rx_between_startfu_and_endfu
    )
  )
  
  # save table 
  saveRDS(flow_data, file.path(paths$D5_dir,"flowcharts" ,paste0(pop_prefix, "_study_pop_to_ASM_users_flowchart.rds")))
  # Prepare dataset for indication and covariate counts
  # sort by person id and rx_date
  setorder(dt_exposures, pregnancy_id, rx_date)
  #keep only the first prescription per pregnancy
  dt_exposures <- dt_exposures[, .SD[1], by = pregnancy_id]
}


########################################################################################################################
########################################################################################################################
########################################################################################################################
# List all indication files 
files_indication <- list.files(file.path(paths$D3_dir, "indication"), pattern = "\\.rds$", full.names = FALSE) #list files in indications folder
files_indication <- files_indication[grepl(paste0("^", pop_prefix, "_"), files_indication)] # keeo files of current pop prefix
files_indication <- files_indication[!grepl(paste(exclude, collapse = "|"), files_indication)] # Exclude subgroups
if (pop_prefix == "PC") files_indication <- files_indication[!grepl("PC_HOSP", files_indication)] #BIFAP

# Create dataset with all indications
dt_indication <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "indication", files_indication), readRDS),fill = TRUE)) # read and bind datasets
dt_indication <- unique(dt_indication) # remove true duplicates 

# Make a copy of exposures
dt_exposures_temp <- copy(dt_exposures)

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG){
  
  # Create windows  
  # Exposures
  dt_exposures_temp[, start_window := as.IDate(as.Date(rx_date) %m-% lookback_period)]
  dt_exposures_temp[, end_window := rx_date - 1]
  # Indications 
  dt_indication[, start_event := event_date][, end_event := event_date]
  
  # rename columns
  setnames(dt_exposures_temp,c("code", "rx_date"), c("exposure_ATC", "exposure_rx_date"))
  
  # set keys 
  setkey(dt_exposures_temp, person_id, start_window, end_window)
  setkey(dt_indication, person_id, start_event, end_event)
  
  # perform overlap join -dx
  indication_in_lookback <- foverlaps(dt_exposures_temp[,.(person_id, exposure_ATC, exposure_rx_date, start_window, end_window)], 
                                      dt_indication[,.(person_id, code, event_date, event_definition, start_event, end_event)], 
                                      by.x = c("person_id", "start_window", "end_window"),
                                      by.y = c("person_id", "start_event", "end_event"), 
                                      nomatch = 0
  )
  
  # Add column for each indication group
  indication_in_lookback[event_definition=="M_RESTLESSLEG_COV",indication_group:="M_RESTLESSLEG_COV"]
  indication_in_lookback[event_definition=="Ment_ANXIETY_COV",indication_group:="Ment_ANXIETY_COV"]
  indication_in_lookback[event_definition=="Ment_BIPOLAR_AESI",indication_group:="Ment_BIPOLAR_AESI"]
  indication_in_lookback[event_definition=="Ment_DEPRESSION_COV",indication_group:="Ment_DEPRESSION_COV"]
  indication_in_lookback[event_definition=="Ment_SCHIZOPHRENIA_COV",indication_group:="Ment_SCHIZOPHRENIA_COV"]
  indication_in_lookback[event_definition=="N_CONVULSION_AESI",indication_group:="N_CONVULSION_AESI"]
  indication_in_lookback[event_definition=="N_EPILEPSY_COV",indication_group:="N_EPILEPSY_COV"]
  indication_in_lookback[event_definition=="N_ESSENTIALTREMOR_AESI",indication_group:="N_ESSENTIALTREMOR_AESI"]
  indication_in_lookback[event_definition=="N_MIGRAINE_COV",indication_group:="N_MIGRAINE_COV"]
  indication_in_lookback[event_definition=="O_FIBROMYALGIA_AESI" | event_definition=="O_NEUROPATHICPAIN_COV" ,indication_group:="O_NEUROPATHICPAINALG_COV"]
  
  # Make unique by person_id, and indication
  indication_in_lookback <- unique(indication_in_lookback, by = c("person_id", "indication_group"))
  
  # All users in denominator
  all_users <- unique(dt_exposures_temp$person_id)
  
  # Users already matched to an indication group
  matched_users <- unique(indication_in_lookback$person_id)
  
  # Users with no indication in lookback
  no_indication_users <- setdiff(all_users, matched_users)
  
  # Count unique people with each indication
  indication_counts <- indication_in_lookback[, .(indication_counts = uniqueN(person_id)), by = indication_group]
  
  # Add column with total users
  indication_counts[, total_users:= nr_unique_persons_with_rx_between_startfu_and_endfu]
  
  # save indication counts in D5
  saveRDS(indication_counts, file.path(paths$D5_dir,  "baseline_tables", "indication_counts", paste0(pop_prefix, "_indication_counts.rds")))
  
} else {
  
  # Create windows  
  if(deap_flags$is_EFEMERIS) {
    dt_exposures_temp[, start_window := as.IDate(as.Date(pregnancy_start_date))]
    dt_exposures_temp[, end_window   := as.IDate(as.Date(pregnancy_end_date))]
    dt_indication[, start_event := event_date]
    dt_indication[, end_event   := event_date]
  }
  
  #TODO Finland
  if(deap_flags$is_FIN_REG){
    dt_exposures_temp[, start_window := as.IDate(as.Date(rx_date) %m-% lookback_period)]
    dt_exposures_temp[, end_window   := rx_date - 1]
    dt_indication[, start_event := event_date]
    dt_indication[, end_event   := event_date]
  }
 
  # rename columns
  setnames(dt_exposures_temp,c("code", "rx_date"), c("exposure_ATC", "exposure_rx_date"))
  
  # set keys 
  setkey(dt_exposures_temp, pregnancy_id, start_window, end_window)
  setkey(dt_indication, pregnancy_id, start_event, end_event)
 
  # perform overlap join -dx
  indication_in_lookback <- foverlaps(dt_exposures_temp[,.(person_id, pregnancy_id, exposure_ATC, exposure_rx_date, start_window, end_window)], 
                                      dt_indication[,.(person_id, pregnancy_id, code, event_date, event_definition, start_event, end_event)], 
                                      by.x = c("pregnancy_id", "start_window", "end_window"),
                                      by.y = c("pregnancy_id", "start_event", "end_event"), 
                                      nomatch = 0
  )
  
  # Add column for each indication group
  indication_in_lookback[event_definition=="M_RESTLESSLEG_COV",indication_group:="M_RESTLESSLEG_COV"]
  indication_in_lookback[event_definition=="Ment_ANXIETY_COV",indication_group:="Ment_ANXIETY_COV"]
  indication_in_lookback[event_definition=="Ment_BIPOLAR_AESI",indication_group:="Ment_BIPOLAR_AESI"]
  indication_in_lookback[event_definition=="Ment_DEPRESSION_COV",indication_group:="Ment_DEPRESSION_COV"]
  indication_in_lookback[event_definition=="Ment_SCHIZOPHRENIA_COV",indication_group:="Ment_SCHIZOPHRENIA_COV"]
  indication_in_lookback[event_definition=="N_CONVULSION_AESI",indication_group:="N_CONVULSION_AESI"]
  indication_in_lookback[event_definition=="N_EPILEPSY_COV",indication_group:="N_EPILEPSY_COV"]
  indication_in_lookback[event_definition=="N_ESSENTIALTREMOR_AESI",indication_group:="N_ESSENTIALTREMOR_AESI"]
  indication_in_lookback[event_definition=="N_MIGRAINE_COV",indication_group:="N_MIGRAINE_COV"]
  indication_in_lookback[event_definition=="O_FIBROMYALGIA_AESI" | event_definition=="O_NEUROPATHICPAIN_COV" ,indication_group:="O_NEUROPATHICPAINALG_COV"]
  
  # Make unique by person_id, and indication
  indication_in_lookback <- unique(indication_in_lookback, by = c("pregnancy_id", "indication_group"))
  
  # Count unique people with each indication
  indication_counts <- indication_in_lookback[, .(indication_counts = uniqueN(pregnancy_id)), by = indication_group]
  
  # Add column with total users
  indication_counts[, total_pregnancies:= nr_unique_pregnancies_with_rx_between_startfu_and_endfu]
  
  # save indication counts in D5
  saveRDS(indication_counts, file.path(paths$D5_dir,  "baseline_tables", "indication_counts", paste0(pop_prefix, "_indication_counts.rds")))
}


########################################################################################################################
########################################################################################################################
########################################################################################################################
# List all comorbidity files 
files_comorbidities <- list.files(file.path(paths$D3_dir, "cov"), pattern = "\\.rds$", full.names = FALSE) #list files in comorbidity folder
files_comorbidities <- files_comorbidities[grepl(paste0("^", pop_prefix, "_"), files_comorbidities)] # keeo files of current pop prefix
files_comorbidities <- files_comorbidities[!grepl(paste(exclude, collapse = "|"), files_comorbidities)] # Exclude subgroups
if (pop_prefix == "PC") files_comorbidities <- files_comorbidities[!grepl("PC_HOSP", files_comorbidities)] #BIFAP

# Create dataset with all comorbidities - ATC codes and Dx codes in separate datasets
dt_comorbidity_meds <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "cov", files_comorbidities[grepl("_med\\.rds$", files_comorbidities)]), readRDS), use.names = TRUE, fill = TRUE))
dt_comorbidity_dx   <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "cov", files_comorbidities[!grepl("_med\\.rds$", files_comorbidities)]), readRDS), use.names = TRUE, fill = TRUE))
dt_comorbidity_meds <- unique(dt_comorbidity_meds) # remove true duplicates
dt_comorbidity_dx   <- unique(dt_comorbidity_dx) # remove true duplicates

# Make a copy of exposures
dt_exposures_temp <- copy(dt_exposures)

# Define comorbidity groups
### C_CARDIOCEREBROVASCULARDESE_COV
group1 <- c("C_ANGINA_AESI", "C_CARDIOMYOPATHY_COV", "C_HF_COV", "C_MI_COV", "C_MYOCARDALL_COV", "C_PERICARDALL_COV", "DP_COVCARDIOCEREBROVAS", "N_STROKE_COV", "V_ANEURYSMVASCMALF_COV")
### M_FRACTURESOSTEOPOROSISALG_COV
group2 <- c("M_FRACTURES_AESI", "M_OSTEOPOROSIS_COV")
### N_BRAININJURYALL_AESI
group3 <- c("N_BRAININJURY_AESI", "N_MENINGOENC_AESI", "N_NEONATENCEPHALOPATHY_AESI", "N_STROKEHEMO_AESI", "N_STROKEISCH_AESI")
### N_DEMENTIAMILDCI_COV
group4 <- c("N_DEMENTIA_COV", "N_MILDCOGNITIVEIMP_COV")
### R_RESPCHRONICALGORITHM_COV
group5 <- c("DP_COVRESPCHRONIC", "R_CHRONICPULMONARYDISEASE_COV")
### Ment_Insomnia_COV
group6 <- c("Ment_Insomnia_COV")
### N_BRAINHYPOXIA_COV
group7 <- c("N_BRAINHYPOXIA_COV")
### V_HYPERTENSION_COV
group8 <- c("V_HYPERTENSION_COV")


if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG){
  
  # Create windows  
  # Exposures
  dt_exposures_temp[, start_window := as.IDate(as.Date(rx_date) %m-% lookback_period)]
  dt_exposures_temp[, end_window := rx_date - 1]
  
  # Comorbidities
  dt_comorbidity_dx[, start_event := event_date][, end_event := event_date]
  dt_comorbidity_meds[, start_event := rx_date][, end_event := rx_date]
  
  # rename columns
  setnames(dt_exposures_temp,c("code", "rx_date"), c("exposure_ATC", "exposure_rx_date"))
  
  # set keys 
  setkey(dt_exposures_temp, person_id, start_window, end_window)
  setkey(dt_comorbidity_dx, person_id, start_event, end_event)
  setkey(dt_comorbidity_meds, person_id, start_event, end_event)
  
  # perform overlap join -dx
  comorbidity_dx_in_lookback <- foverlaps(dt_exposures_temp[,.(person_id, exposure_ATC, exposure_rx_date, start_window, end_window, start_follow_up, end_follow_up, entry_date, exit_date)], 
                                          dt_comorbidity_dx[,.(person_id, code, event_date, event_definition, start_event, end_event)], 
                                          by.x = c("person_id", "start_window", "end_window"),
                                          by.y = c("person_id", "start_event", "end_event"), 
                                          nomatch = 0
  )
  
  # perform overlap join - meds
  comorbidity_meds_in_lookback <- foverlaps(dt_exposures_temp[,.(person_id, exposure_ATC, exposure_rx_date, start_window, end_window, start_follow_up, end_follow_up, entry_date, exit_date)], 
                                            dt_comorbidity_meds[, .(person_id, code, rx_date, Varname, start_event, end_event)], 
                                            by.x = c("person_id", "start_window", "end_window"),
                                            by.y = c("person_id", "start_event", "end_event"), 
                                            nomatch = 0
  )
  
  # Rename columns to be the same before binding dataset 
  setnames(comorbidity_dx_in_lookback, c("event_date", "event_definition"), c("comorbidity_date", "comorbidity"))
  setnames(comorbidity_meds_in_lookback, c("rx_date", "Varname"), c("comorbidity_date", "comorbidity"))
  
  # Bind the two data sets into one
  all_comorbidities_in_lookback <- rbindlist(list(comorbidity_dx_in_lookback, comorbidity_meds_in_lookback), use.names = TRUE, fill = TRUE)
  
  # Assign group names to comorbidities
  all_comorbidities_in_lookback[comorbidity %in% group1, comorbidity_group:= "C_CARDIOCEREBROVASCULARDESE_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group2, comorbidity_group:= "M_FRACTURESOSTEOPOROSISALG_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group3, comorbidity_group:= "N_BRAININJURYALL_AESI"]
  all_comorbidities_in_lookback[comorbidity %in% group4, comorbidity_group:= "N_DEMENTIAMILDCI_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group5, comorbidity_group:= "R_RESPCHRONICALGORITHM_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group6, comorbidity_group:= "Ment_Insomnia_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group7, comorbidity_group:= "N_BRAINHYPOXIA_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group8, comorbidity_group:= "V_HYPERTENSION_COV"]
  
  # Make unique by person_id, episode.start, and co-morbidity
  all_comorbidities_in_lookback <- unique(all_comorbidities_in_lookback, by = c("person_id", "comorbidity_group"))
  
  # Count unique people with each co-morbidity
  comorbidity_counts <- all_comorbidities_in_lookback[, .(comorbidity_counts = uniqueN(person_id)), by = comorbidity_group]
  
  # Add column with total new users 
  comorbidity_counts[, total_users:= nr_unique_persons_with_rx_between_startfu_and_endfu]
  
  # save co-morbidity counts in D5
  saveRDS(comorbidity_counts, file.path(paths$D5_dir, "baseline_tables", "comorbidity_counts", paste0(pop_prefix, "_comorbidity_counts.rds")))
} else {
  
  # Create windows  
  if(deap_flags$is_EFEMERIS)  { 
    dt_exposures_temp[, start_window := as.IDate(op_start_date)]
    dt_exposures_temp[, end_window   := as.IDate(pregnancy_end_date)]
    dt_comorbidity_dx[, start_event := event_date]
    dt_comorbidity_dx[, end_event   := event_date]
    dt_comorbidity_meds[, start_event := rx_date]
    dt_comorbidity_meds[, end_event   := rx_date]
  }
  
  #TODO FINLAND
  if(deap_flags$is_FIN_REG) {
    dt_exposures_temp[, start_window := as.IDate(as.Date(rx_date) %m-% lookback_period)]
    dt_exposures_temp[, end_window := rx_date - 1]
    dt_comorbidity_dx[, start_event := event_date]
    dt_comorbidity_dx[, end_event   := event_date]
    dt_comorbidity_meds[, start_event := rx_date]
    dt_comorbidity_meds[, end_event   := rx_date]
    
  }  
  
  # rename columns
  setnames(dt_exposures_temp,c("code", "rx_date"), c("exposure_ATC", "exposure_rx_date"))
  
  # set keys 
  setkey(dt_exposures_temp, pregnancy_id, start_window, end_window)
  setkey(dt_comorbidity_dx, pregnancy_id, start_event, end_event)
  setkey(dt_comorbidity_meds, pregnancy_id, start_event, end_event)
  
  # perform overlap join -dx
  comorbidity_dx_in_lookback <- foverlaps(dt_exposures_temp[,.(pregnancy_id, exposure_ATC, exposure_rx_date, start_window, end_window, start_follow_up, end_follow_up)], 
                                          dt_comorbidity_dx[,.(pregnancy_id, code, event_date, event_definition, start_event, end_event)], 
                                          by.x = c("pregnancy_id", "start_window", "end_window"),
                                          by.y = c("pregnancy_id", "start_event", "end_event"), 
                                          nomatch = 0
  )
  
  # perform overlap join - meds
  comorbidity_meds_in_lookback <- foverlaps(dt_exposures_temp[,.(pregnancy_id, exposure_ATC, exposure_rx_date, start_window, end_window, start_follow_up, end_follow_up)], 
                                            dt_comorbidity_meds[, .(pregnancy_id, code, rx_date, Varname, start_event, end_event)], 
                                            by.x = c("pregnancy_id", "start_window", "end_window"),
                                            by.y = c("pregnancy_id", "start_event", "end_event"), 
                                            nomatch = 0
  )
  
  # Rename columns to be the same before binding dataset 
  setnames(comorbidity_dx_in_lookback, c("event_date", "event_definition"), c("comorbidity_date", "comorbidity"))
  setnames(comorbidity_meds_in_lookback, c("rx_date", "Varname"), c("comorbidity_date", "comorbidity"))
  
  # Bind the two data sets into one
  all_comorbidities_in_lookback <- rbindlist(list(comorbidity_dx_in_lookback, comorbidity_meds_in_lookback), use.names = TRUE, fill = TRUE)
  
  # Assign group names to comorbidities
  all_comorbidities_in_lookback[comorbidity %in% group1, comorbidity_group:= "C_CARDIOCEREBROVASCULARDESE_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group2, comorbidity_group:= "M_FRACTURESOSTEOPOROSISALG_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group3, comorbidity_group:= "N_BRAININJURYALL_AESI"]
  all_comorbidities_in_lookback[comorbidity %in% group4, comorbidity_group:= "N_DEMENTIAMILDCI_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group5, comorbidity_group:= "R_RESPCHRONICALGORITHM_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group6, comorbidity_group:= "Ment_Insomnia_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group7, comorbidity_group:= "N_BRAINHYPOXIA_COV"]
  all_comorbidities_in_lookback[comorbidity %in% group8, comorbidity_group:= "V_HYPERTENSION_COV"]
  
  # Make unique by person_id, episode.start, and co-morbidity
  all_comorbidities_in_lookback <- unique(all_comorbidities_in_lookback, by = c("pregnancy_id", "comorbidity_group"))
  
  # Count unique people with each co-morbidity
  comorbidity_counts <- all_comorbidities_in_lookback[, .(comorbidity_counts = uniqueN(pregnancy_id)), by = comorbidity_group]
  
  # Add column with total new users 
  comorbidity_counts[, total_pregnancies:= nr_unique_pregnancies_with_rx_between_startfu_and_endfu]
  
  # save co-morbidity counts in D5
  saveRDS(comorbidity_counts, file.path(paths$D5_dir, "baseline_tables", "comorbidity_counts", paste0(pop_prefix, "_comorbidity_counts.rds")))
}

########################################################################################################################
########################################################################################################################
########################################################################################################################
# ASM BASELINE TABLES 

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
fu_median      <- median(dt_exposures_temp$followup_days)/365.25
fu_IQR         <- IQR(dt_exposures_temp$followup_days)/365.25
fu_min         <- min(dt_exposures_temp$followup_days)/365.25
fu_max         <- max(dt_exposures_temp$followup_days)/365.25
max_endfu_date <- max(dt_exposures_temp$end_follow_up)

# Mean Age
age_at_start_fu_mean <-mean(dt_exposures_temp$age_at_start_follow_up)
age_at_start_fu_SD   <-sd(dt_exposures_temp$age_at_start_follow_up)

# Counts Per Age_Group 
age_group_12_18.99_count <- sum(dt_exposures_temp$age_group == "12-18.99", na.rm = TRUE)
age_group_19_34.99_count <- sum(dt_exposures_temp$age_group == "19-34.99", na.rm = TRUE)
age_group_35_54.99_count <- sum(dt_exposures_temp$age_group == "35-54.99", na.rm = TRUE)
age_group_55_74.99_count <- sum(dt_exposures_temp$age_group == "55-74.99", na.rm = TRUE)
age_group_above_75_count <- sum(dt_exposures_temp$age_group == "75+", na.rm = TRUE)
age_group_outside_range_count  <- sum(is.na(dt_exposures_temp$age_group)) # CHECK 

# Calculates percentages
age_group_12_18.99_perc <- (age_group_12_18.99_count/nrow(dt_exposures_temp)) * 100
age_group_19_34.99_perc <- (age_group_19_34.99_count/nrow(dt_exposures_temp)) * 100
age_group_35_54.99_perc <- (age_group_35_54.99_count/nrow(dt_exposures_temp)) * 100
age_group_55_74.99_perc <- (age_group_55_74.99_count/nrow(dt_exposures_temp)) * 100
age_group_above_75_perc <- (age_group_above_75_count/nrow(dt_exposures_temp)) * 100

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
           "age_group_above_75_perc", 
           "age_group_outside_range_count")

values<-c(as.character(round(fu_median,2)),
          as.character(round(fu_IQR,2)),
          as.character(round(fu_min,2)),
          as.character(round(fu_max,2)),
          
          as.character(max_endfu_date),
          
          as.character(round(age_at_start_fu_mean,2)), 
          as.character(round(age_at_start_fu_SD, 2)),
          
          as.character(age_group_12_18.99_count),
          as.character(round(age_group_12_18.99_perc,2)),
          as.character(age_group_19_34.99_count),
          as.character(round(age_group_19_34.99_perc,2)),
          as.character(age_group_35_54.99_count),
          as.character(round(age_group_35_54.99_perc,2)),
          as.character(age_group_55_74.99_count),
          as.character(round(age_group_55_74.99_perc,2)),
          as.character(age_group_above_75_count),
          as.character(round(age_group_above_75_perc,2)), 
          as.character(round(age_group_outside_range_count))
)

# Join names and values 
baseline_table <-data.table(names, values)

# Save baseline table 
saveRDS(baseline_table, file.path(paths$D5_dir, "baseline_tables", paste0(pop_prefix, "_ASM_users_baseline_table.rds")))










