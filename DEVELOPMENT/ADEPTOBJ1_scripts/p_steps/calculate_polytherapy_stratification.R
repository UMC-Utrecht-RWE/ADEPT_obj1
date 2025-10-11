###############################################################################################################################################################################
# <<< Sub-objective 1.2: Polytherapy rate >>>
# Measure: Annual polytherapy rate of ASM
# Numerator: The number of individuals who use distinct ASMs in a calendar year with >=182 days overlap between the treatment episodes
# Denominator: Total number of study population in that calendar year in the data source
# Stratification by: indication, calendar year, data source

###############################################################################################################################################################################
print("=========================================================================================")
print("========================= STRATIFYING POLYTHERAPY BY INDICATION =========================")
print("=========================================================================================")

# create folder for stratification counts
dir.create(file.path(paths$D5_dir, "1.2_polytherapy", "stratified"), showWarnings = FALSE, recursive = TRUE)

# get list of polytherapy files
files_polytherapy_episodes <- list.files(file.path(paths$D4_dir, "1.2_polytherapy"), pattern = "\\.rds$")

# filter for pop_prefix
files_polytherapy_episodes <- files_polytherapy_episodes[grepl(paste0("^", pop_prefix, "_"), files_polytherapy_episodes)]

# get a list of indication files 
files_indication <- list.files(file.path(paths$D3_dir, "indication"), pattern = "\\.rds$", full.names = TRUE)

# filter for pop_prefix
files_indication <- files_indication[grepl(paste0("^", pop_prefix, "_"), basename(files_indication))]

# load and bind all indications into one dataset, remove true duplicates
dt_indication <- unique(rbindlist(lapply(files_indication, readRDS), use.names = TRUE, fill = TRUE))

# change value of column event_definition in any rows with O_NEUROPATHICPAIN_COV or O_FIBROMYALGIA_AESI to algorithm name O_NEUROPATHICPAINALG_COV
dt_indication[event_definition== "O_NEUROPATHICPAIN_COV" | event_definition=="O_FIBROMYALGIA_AESI", event_definition:="O_NEUROPATHICPAINALG_COV"]

# set strata levels
# indications
indication_levels <- c("M_RESTLESSLEG_COV", "Ment_ANXIETY_COV", "Ment_BIPOLAR_AESI", "Ment_DEPRESSION_COV", "Ment_SCHIZOPHRENIA_COV",
                       "N_CONVULSION_AESI", "N_EPILEPSY_COV", "N_ESSENTIALTREMOR_AESI", "N_MIGRAINE_COV", "O_NEUROPATHICPAINALG_COV", "UNKNOWN")

# Create vector of study years from study dates (exist in environment)
study_years <- seq(year(as.IDate(as.Date(start_study_date) + lookback_period)), year(as.IDate(end_study_date)))

# create empty data frame using all possible years from the study for counts
all_combinations_indications <- CJ(year = study_years, indication = indication_levels, unique = TRUE)

# Load and bind all polytherapy files
dt <- rbindlist(lapply(file.path(paths$D4_dir, "1.2_polytherapy", files_polytherapy_episodes), readRDS), use.names = TRUE, fill = TRUE)

# clean polytherapy dataset 
# keep records where overlap is greater or equal to 182 days
dt <- dt[overlap_days >= 182,]

if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG){
  
  # Sort by person id and overlap start
  setorder(dt, person_id, overlap_start)
  
  # Keep only one row per person per year
  dt <- unique(dt, by = c("person_id", "year"))
  
} else {
  
  # Sort by person id and overlap start
  setorder(dt, pregnancy_id, overlap_start)
  
  # Keep only one row per person per year
  dt <- unique(dt, by = c("pregnancy_id", "year"))
}

# prepare denominator
denom_counts <- dt[, .(Freq = .N), by = year]

#<<< INDICATIONS >>>#
# create a copy of dt for indication calculations
dt_temp <- copy(dt)

# prepare data for foverlaps
# polytherapy
dt_temp[, start_window := as.IDate(as.Date(overlap_start) %m-% lookback_period)][, end_window := overlap_start]

# indication file 
dt_indication[, start_event := as.IDate(event_date)][, end_event   := as.IDate(event_date)]

# set keys
setkey(dt_temp, person_id, start_window, end_window)
setkey(dt_indication, person_id, start_event, end_event)

# perform overlap join
indications <- foverlaps(dt_temp[.(person_id, start_window, end_window, overlap_start)],
                         dt_indication[,.(person_id, start_event, end_event, event_date, event_definition)],
                         by.x = c("person_id", "start_window", "end_window"),
                         by.y = c("person_id", "start_event", "end_event"),
                         nomatch = NA
)

# calculate difference in days between episode start and event date of indication
indications[, diff_days := as.numeric(difftime(overlap_start, event_date, units = "days"))]

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
    } else if (all(is.na(event_date)) & all(is.na(event_definition))) {
      row <- .SD[1]
      row[, indication := "UNKNOWN"]
      row
    } else {
      row <- .SD[which.min(diff_days)]
      row[, indication := row$event_definition]
      row
    }
  },
  by = .(person_id, overlap_start)
]

# Keep one row per person_id - overlap.start
indications <- unique(indications, by = c("person_id", "overlap_start"))
 
# count groups per year
indication_counts <- indications[, .N, by = .(year, indication)]

# merge counts with empty dt
indication_counts <- merge(all_combinations_indications, indication_counts, by = c("year", "indication"), all.x = TRUE)

# if is.na(N), replace it with 0
indication_counts[is.na(N), N := 0]

# merge with denominator
indication_counts <- merge(indication_counts, denom_counts, by = c("year"))

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
saveRDS(indication_counts, file.path(paths$D5_dir, "1.2_polytherapy", "stratified", paste0(pop_prefix, "_polytherapy_indication_counts.rds")))

