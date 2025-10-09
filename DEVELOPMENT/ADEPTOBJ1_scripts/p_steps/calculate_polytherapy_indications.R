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

# <<< POLYTHERAPY FILES >>>
# get list of polytherapy files 
files_polytherapy_episodes <- list.files(file.path(paths$D4_dir, "1.2_polytherapy"), pattern = "\\.rds$")

# filter for pop_prefix
files_polytherapy_episodes <- files_polytherapy_episodes[grepl(paste0("^", pop_prefix, "_"), files_polytherapy_episodes)]

# read in polytherapy file 
dt <- rbindlist(lapply(file.path(paths$D4_dir, "1.2_polytherapy", files_polytherapy_episodes), readRDS), use.names = TRUE, fill = TRUE)

# keep only records where overlap is greater than 182 days
dt <- dt[overlap_days >= 182,]

# Overlap should be between start and end fu
dt <- dt[overlap_start >= start_follow_up & overlap_start <= end_follow_up & overlap_end >= start_follow_up & overlap_end <= end_follow_up]

# Ensure overlap dates are IDate
dt[, `:=`(overlap_start = as.IDate(overlap_start), overlap_end = as.IDate(overlap_end))]

# create column year for year of overlap
dt[,year:= year(overlap_start)]

# Sort by person id and overlap start
setorder(dt, person_id, overlap_start)

# Keep only one row per person per year
dt <- unique(dt, by = c("person_id", "year"))

# prepare denominator 
denom_counts <- dt[, .(Freq = .N), by = year]

# <<< INDICATION FILES >>>
# get list of indication files 
files_indication <- list.files(file.path(paths$D3_dir, "indication"), pattern = "\\.rds$", full.names = TRUE)

# filter for pop_prefix
files_indication <- files_indication[grepl(paste0("^", pop_prefix, "_"), basename(files_indication))]

# read and bind indication files
dt_indication <- rbindlist(lapply(files_indication, readRDS), use.names = TRUE, fill = TRUE)

# remove any true duplicates
dt_indication <- unique(dt_indication)

# create a folder for stratified counts
dir.create(file.path(paths$D5_dir, "1.2_polytherapy", "stratified"), showWarnings = FALSE, recursive = TRUE)

# set stratification levels 
# indications
indication_levels <- c("M_RESTLESSLEG_COV", "Ment_ANXIETY_COV", "Ment_BIPOLAR_AESI", "Ment_DEPRESSION_COV", "Ment_SCHIZOPHRENIA_COV",
                       "N_CONVULSION_AESI", "N_EPILEPSY_COV", "N_ESSENTIALTREMOR_AESI", "N_MIGRAINE_COV", "O_NEUROPATHICPAINALG_COV", "UNKNOWN")

# create empty dt year for counts to include all possible combinations
all_years  <- seq(year(start_study_date), year(end_study_date))
all_combinations_indications <- CJ(year = all_years, indication = indication_levels, unique = TRUE)


if(nrow(dt)>0){ 
  
  #<<< PREPARE DATA FOR FOVERLAPS >>>  
  # poly files
  dt <- dt[, .(person_id, atc_group, episode.start, episode.end, i.atc_group, i.episode.start, i.episode.end, overlap_start, overlap_end, overlap_days, start_follow_up, end_follow_up, year)] 
  setnames(dt, c("atc_group", "episode.start", "episode.end", "i.atc_group", "i.episode.start", "i.episode.end"), c("atc_group1", "episode.start1", "episode.end1", "atc_group2", "episode.start2", "episode.end2"))          
  # set windows 
  dt[, start_window := as.IDate(as.Date(overlap_start) %m-% lookback_period)][, end_window := overlap_start]
  
  # indication files 
  dt_indication <- dt_indication[, .(person_id, event_date, event_definition)] 
  # set windows
  dt_indication[, start_event := event_date][, end_event := event_date]
  
  # change column event_definition in any rows with O_NEUROPATHICPAIN_COV or O_FIBROMYALGIA_AESI to algorithm name O_NEUROPATHICPAINALG_COV
  dt_indication[event_definition== "O_NEUROPATHICPAIN_COV" | event_definition=="O_FIBROMYALGIA_AESI", event_definition:="O_NEUROPATHICPAINALG_COV"]
  
  # set keys 
  setkey(dt, person_id, start_window, end_window)
  setkey(dt_indication, person_id, start_event, end_event)
  
  # perform overlap join 
  indications <- foverlaps(dt, 
                           dt_indication, 
                           by.x = c("person_id", "start_window", "end_window"),
                           by.y = c("person_id", "start_event", "end_event"), 
                           nomatch = NA
  )
  
  # calculate difference in days between overlap start and event date of indication 
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
  
  # counts
  # Keep one row per person_id - overlap.start
  indications <- unique(indications, by = c("person_id", "overlap_start"))
  
  # count groups per year
  indication_counts <- indications[, .N, by = .(year, indication)]
  
  # merge counts with empty dt
  indication_counts <- merge(all_combinations_indications, indication_counts, by = c("year", "indication"), all.x = TRUE)
  
  # if is.na(N), replace it with 0
  indication_counts[is.na(N), N := 0]
  
  # Merge with denominator 
  indication_counts <- merge(indication_counts, denom_counts, by = c("year"))
  
  # if is.na(Freq), replace it with 0
  indication_counts[is.na(Freq), Freq := 0]
  
  # calculate rate, if N = 0 and Freq = 0 then change the rate to 0 
  indication_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
  
  # create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
  indication_counts[, rate_computable := Freq > 0]
  
  # save counts
  saveRDS(indication_counts, file.path(paths$D5_dir, "1.2_polytherapy", "stratified", paste0(pop_prefix, "_polytherapy_indication_counts.rds")))
  
  # sanity check
  # Sum counts per year
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
  
} else {
  
  message(red("There are no polytherapy files to stratify by indication"))
  
}


























