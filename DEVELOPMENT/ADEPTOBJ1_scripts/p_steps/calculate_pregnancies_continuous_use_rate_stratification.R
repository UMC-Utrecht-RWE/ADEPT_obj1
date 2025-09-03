###############################################################################################################################################################################
# <<< Sub-objective 1.3: Continuous use rate >>> 
# Measure: Annual continuous rate of ASM use during pregnancy
# Numerator: The number of pre-pregnancy users of an ASM within a calendar year that also runs into the first, second and third trimester of pregnancy 
# Denominator: Total number of pregnancies in that calendar year in the data source
# Stratification by: Individual drug substance, drug sub-groups, indication, calendar year, data source

###############################################################################################################################################################################

print("=================================================================================================")
print("========================= STRATIFYING CONTINUOUS USE RATE BY INDICATION =========================")
print("=================================================================================================")

# create a folder for stratified counts
dir.create(file.path(paths$D5_dir, "1.3_pregnancy_continuous", "stratified"), showWarnings = FALSE, recursive = TRUE)

# get list of [incidence files prevalence files
files_cont_use_episodes <- list.files(file.path(paths$D4_dir, "1.3_pregnancy_continuous"), pattern = "\\.rds$")
if(pop_prefix=="PC") files_cont_use_episodes <- files_cont_use_episodes[!grepl("PC_HOSP", files_cont_use_episodes)] # BIFAP

# Get indication files 
files_indication <- list.files(file.path(paths$D3_dir, "indication"), pattern = "\\.rds$", full.names = TRUE)
if (pop_prefix == "PC") files_indication <- files_indication[!grepl("PC_HOSP", basename(files_indication))]

# load and bind all indications into one dataset
dt_indication <- rbindlist(lapply(files_indication, readRDS), use.names = TRUE, fill = TRUE)
dt_indication <- unique(dt_indication)

# indications
indication_levels <- c("M_RESTLESSLEG_COV", "Ment_ANXIETY_COV", "Ment_BIPOLAR_AESI", "Ment_DEPRESSION_COV", "Ment_SCHIZOPHRENIA_COV",
                       "N_CONVULSION_AESI", "N_EPILEPSY_COV", "N_ESSENTIALTREMOR_AESI", "N_MIGRAINE_COV", "O_NEUROPATHICPAINALG_COV", "UNKNOWN")

# create empty dt year for counts to include all possible combinations
all_years  <- seq(year(start_study_date), year(end_study_date))
all_combinations_indications <- CJ(year = all_years, indication = indication_levels, unique = TRUE)

if(length(files_cont_use_episodes)>0){
  # loop over episodes
  for(episode in seq_along(files_cont_use_episodes)){
    
    # print message
    message("Processing: ", sub("_continuous_use_rate_data\\.rds$", "", files_cont_use_episodes[episode]))
    
    # load current episode
    dt <- readRDS(file.path(paths$D4_dir, "1.3_pregnancy_continuous", files_cont_use_episodes[episode]))
    
    #<<< INDICATIONS >>>#
    dt_temp <- copy(dt)
    
    if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
      
      # Set Windows 
      dt_temp[, start_window := as.IDate(as.Date(episode.start) %m-% lookback_period)][, end_window := episode.start]
      dt_indication[, start_event := event_date][, end_event := event_date]
      
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
      indications <- indications[, .SD, .SDcols = c("person_id", "event_date", "code",  "event_definition", "episode.start", "i.code", "atc_group", 
                                                    "sex_at_instance_creation", "birth_date", "start_follow_up", "end_follow_up", "entry_date", "exit_date")]
      
      
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
      
      # extract year from group by date column - episode.start
      indications[, year := year(episode.start)]
      
      # Keep one row per person_id - episode.start
      indications <- unique(indications, by = c("person_id", "episode.start"))
      
    } else {
      
      # Set Windows 
      if(deap_flags$is_EFEMERIS) dt_temp[, start_window := as.IDate(episode.start) - lookback_period][,start_window:=as.IDate(start_window)]
      if(deap_flags$is_FIN_REG) dt_temp[, start_window := as.IDate(as.Date(episode.start) %m-% lookback_period)]
      dt_temp[, end_window := episode.start]
      dt_indication[, start_event := event_date][, end_event := event_date]
      
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
      indications <- indications[, .SD, .SDcols = c("person_id", "pregnancy_id","event_date", "code",  "event_definition", "episode.start", "i.code", "atc_group", 
                                                    "sex_at_instance_creation", "birth_date", "start_follow_up", "end_follow_up")]
      
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
        by = .(pregnancy_id, episode.start)
      ]
      
      # extract year from group by date column - episode.start
      indications[, year := year(episode.start)]
      
      # Keep one row per pregnancy_id - episode.start
      indications <- unique(indications, by = c("pregnancy_id", "episode.start"))
    }
    
    # COMMON TO BOTH 
    # count groups per year
    indication_counts <- indications[, .N, by = .(year, indication)]
    
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
    saveRDS(indication_counts, file.path(paths$D5_dir, "1.3_pregnancy_continuous", "stratified", paste0(sub("_continuous_rates.*$", "", files_cont_use_episodes[episode]), "_continuous_use_rates_in_pregnancy_indication_counts.rds")))
  }
  
} else {
  
  message("No continuers in pregnancy found for stratification")

  }