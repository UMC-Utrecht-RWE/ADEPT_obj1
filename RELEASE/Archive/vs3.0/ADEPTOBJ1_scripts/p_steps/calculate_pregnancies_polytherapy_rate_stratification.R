###############################################################################################################################################################################
# <<< Sub-objective 1.2: Polytherapy rate >>> 
# Measure: Annual polytherapy rate of ASM
# Numerator: The number of individuals who use ≥2 distinct ASMs in a calendar year with ≥182 days overlap between the treatment episodes 
# Denominator: Total number of study population in that calendar year in the data source
# Stratification by: indication, calendar year, data source

###############################################################################################################################################################################


print("================================================================================================================")
print("========================= STRATIFYING POLYTHERAPY IN PRE-PREGNANCY USERS BY INDICATION =========================")
print("================================================================================================================")

# <<< POLYTHERAPY FILES >>>
# get list of polytherapy files 
files_polytherapy_episodes <- list.files(file.path(paths$D4_dir, "1.4_pregnancy_polytherapy"), pattern = "\\.rds$")
# filter for pop_prefix
files_polytherapy_episodes <- files_polytherapy_episodes[grepl(paste0("^", pop_prefix, "_"), files_polytherapy_episodes)]
# if pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_polytherapy_episodes <- files_polytherapy_episodes[!grepl("PC_HOSP", files_polytherapy_episodes)]
# read in file
dt <- rbindlist(lapply(file.path(paths$D4_dir, "1.4_pregnancy_polytherapy", files_polytherapy_episodes), readRDS), use.names = TRUE, fill = TRUE)

if(nrow(dt)>0){
# <<< INDICATION FILES >>>
# list all files in the indication folder
files_indication <- list.files(file.path(paths$D3_dir, "indication"), pattern = "\\.rds$", full.names = TRUE)
# filter for Female subpop only 
files_indication <- files_indication[grepl("_F_", files_indication)]
# if pop_prefix is PC, then drop any that are PC_HOSP
if (pop_prefix == "PC") files_indication <- files_indication[!grepl("PC_HOSP", basename(files_indication))]
# load and bind all indications into one dataset
dt_indication <- rbindlist(lapply(files_indication, readRDS), use.names = TRUE, fill = TRUE)
# remove any true duplicates
dt_indication <- unique(dt_indication)

# create a folder for stratified counts
dir.create(file.path(paths$D5_dir, "1.4_pregnancy_polytherapy", "stratified"), showWarnings = FALSE, recursive = TRUE)

# set stratification levels 
# indications
indication_levels <- c("M_RESTLESSLEG_COV", "Ment_ANXIETY_COV", "Ment_BIPOLAR_AESI", "Ment_DEPRESSION_COV", "Ment_SCHIZOPHRENIA_COV",
                       "N_CONVULSION_AESI", "N_EPILEPSY_COV", "N_ESSENTIALTREMOR_AESI", "N_MIGRAINE_COV", "O_NEUROPATHICPAINALG_COV", "UNKNOWN")

# create empty dt year for counts to include all possible combinations
all_years  <- seq(year(start_study_date), year(end_study_date))
all_combinations_indications <- CJ(year = all_years, indication = indication_levels, unique = TRUE)

#<<< PREPARE DATA FOR FOVERLAPS >>>  
dt_temp <- copy(dt)
# set start and end windows 
# dt_temp[, start_window := as.IDate(as.Date(overlap_start) - lookback_period)]
dt_temp[, start_window := as.IDate(as.Date(overlap_start) %m-% lookback_period)]
dt_temp[, end_window := overlap_start]
# Drop unnecessary columns
dt_indication <- dt_indication[, .(person_id, event_date, event_definition)] 
# indication data
dt_indication[, start_event := event_date][, end_event := event_date]

# set keys 
setkey(dt_temp, person_id, start_window, end_window)
setkey(dt_indication, person_id, start_event, end_event)

# perform overlap join 
indications <- foverlaps(dt_temp, 
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

# extract year from group by date column - episode.start
indications[, year := year(overlap_start)]

# Keep one row per person_id - episode.start
indications <- unique(indications, by = c("person_id", "overlap_start"))

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
saveRDS(indication_counts, file.path(paths$D5_dir, "1.4_pregnancy_polytherapy", "stratified", paste0(pop_prefix, "_polytherapy_in_pregnancy_indication_counts.rds")))
} else { 
  message(red("No polytherapy episodes found"))
  }


