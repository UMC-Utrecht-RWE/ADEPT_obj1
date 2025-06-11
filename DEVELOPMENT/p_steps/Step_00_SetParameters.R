#Author: Roel Elbers Drs.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

#modifications made by Ema Alsina, MSc. e.m.alsina-2@umcutrecht.nl

# this script creates a list of selection criteria which are used to extract the study population from the source dataset


# <<< Extract metadata from CDM_SOURCE file >>>

# Identify CDM_SOURCE file in CDMInstances Folder
cdm_source_file <- list.files(path_CDM_dir, pattern = "^CDM_SOURCE")

# Read in CDM_SOURCE file into a data.table
CDM_SOURCE <- fread(paste0(path_CDM_dir, cdm_source_file))

# Extract the date of CDM creation from the CDM_SOURCE table
date_creation <- CDM_SOURCE[, date_creation]

# Extract the data access provider name (DEAP name) from the CDM_SOURCE table
data_access_provider_name <- CDM_SOURCE[, data_access_provider_name]

# Extract the data source name from the CDM_SOURCE table
data_source_name <- CDM_SOURCE[, data_source_name]

# Convert the recommended_end_date to Date format (IDate), assuming format is YYYYMMDD
recommended_end_date <- as.IDate(as.character(CDM_SOURCE$recommended_end_date), "%Y%m%d")

# <<< Date Transformations >>>

# Convert study start and end dates from character to IDate format
start_study_date <- as.IDate(as.character(start_study_date), "%Y%m%d")
end_study_date   <- as.IDate(as.character(end_study_date), "%Y%m%d")

# Convert CDM creation date to IDate format (corrected)
date_creation    <- as.IDate(as.character(date_creation), "%Y%m%d")

# Create interval vector for study period
intv <- c(start_study_date, end_study_date)


# <<< Set end_study_date to the earliest (minimum) of end_study_date, date_creation, and recommended_end_date, ignoring any NAs >>>
print("Check date creation is after end study date")
end_study_date <- min(end_study_date, date_creation, recommended_end_date, na.rm = TRUE)


# <<< Set Selection Criteria >>>
SelectionCriteria <- list(
  #Ensures num_spell, op_start_date, and op_end_date are not missing.
  No_observation_time = expression(!is.na(num_spell) & !is.na(op_start_date) & !is.na(op_end_date)),
  # Ensures spell duration is at least 364 days.
  Spell_less_than_year = expression((op_end_date-op_start_date)>=364),
  # Ensures op_start_date is not missing.
  No_op_start_date = expression(!is.na(op_start_date)),
  # Ensures year_of_birth is not missing.
  No_year_of_birth = expression(!is.na(year_of_birth)),
  # Ensures year of death is not missing if day or month of death are provided.
  No_year_of_death = expression(!(is.na(year_of_death) & (!is.na(day_of_death) | !is.na(month_of_death)))),
  # Checks that a spell starts before it ends.
  OP_START_DATE_before_OP_END_DATE = expression(op_start_date < op_end_date),
  # Spell overlaps with study period, including partially before/after.
  Study_Period_and_spell_overlap  = expression(op_start_date %between% intv | op_end_date %between% intv | (op_start_date  < start_study_date & op_end_date > end_study_date)),
  # Spell duration is greater than lookback period.
  Spells_less_then_lookback_period = expression(op_end_date - op_start_date > lookback_period),
  # Time from spell start to study end is enough to satisfy lookback.
  Remaning_time_to_end_study_date_less_then_lookback_period = expression(end_study_date - op_start_date > lookback_period),
  # Time from spell start to max available date is greater than lookback.
  Remaning_time_to_date_max_less_then_lookback_period = expression(date_max - op_start_date > lookback_period),
  # The earliest date we have for a person (like their birth date) comes before the study ends. But where is date_min from?
  Age_min_to_end_of_study_above_0 = expression(end_study_date - date_min > 0),
  # Ensures there’s positive time from study start to maximum spell date.
  Start_of_study_Age_max_to_above_0 = expression(date_max - start_study_date > 0),
  # Checks that patient’s age at spell start and end is within [Age_min, Age_max] - WOMEN
  Age_filter_spells_F = expression(age_op_start_date < Age_max_women &  age_op_end_date > Age_min),
  # Checks that patient’s age at spell start and end is within [Age_min, Age_max] - MEN
  Age_filter_spells_M = expression(age_op_end_date > Age_min),
  # Checks that patients age_study_creation for Women is >= age_min and <= age_max_women or for Men is >=age_min
  # Age_start_study = expression(
  # (sex_at_instance_creation == "F" & age_op_start_date >= Age_min & age_op_start_date <= Age_max_women) |
  # (sex_at_instance_creation == "M" & age_op_start_date >= Age_min)
  # ),
  # Checks for sex = M or F
  Sex = expression(sex_at_instance_creation=="F" | sex_at_instance_creation=="M")
)


# start_study_date2 <- paste0(year(start_study_date),sprintf("%02d",month(start_study_date)),sprintf("%02d",day(start_study_date)))
# end_study_date2 <- paste0(year(end_study_date),sprintf("%02d",month(end_study_date)),sprintf("%02d",day(end_study_date)))
#Set to IDate format

# Age_min<-12
# Age_max<-55



#Check subpopulations from metadata
#Get metadata file name
metadata_file <- list.files(path_CDM_dir, pattern = "^METADATA")

#Retrieve data from METADATA
METADATA <- fread(paste0(path_CDM_dir, metadata_file))

if(any(METADATA[["type_of_metadata"]]== "subpopulations")){

  if(!(METADATA[type_of_metadata == "subpopulations", values] == "" | is.na(METADATA[type_of_metadata == "subpopulations", values]))){
    SUBP <-T
    METADATA_subp<-METADATA[!type_of_metadata %in% c("presence_of_table", "presence_of_column", "list_of_values")]

    #Get meanings_sets
    op_meaning_list_set<-METADATA[type_of_metadata=="op_meanings_list_per_set", c("other", "values")]
    names(op_meaning_list_set)<-c("op_meaning_sets", "op_meanings_list_per_set")

    #Get subpopulation_meanings
    subpopulation_meanings<-METADATA[type_of_metadata=="op_meaning_sets", c("other", "values")]
    names(subpopulation_meanings)<-c("subpopulations", "meaning_sets")

    library("stringr")
    subpopulations<-unlist(str_split(METADATA_subp[type_of_metadata=="subpopulations",values], pattern = " "))
    gc()
  } else SUBP <- FALSE
} else SUBP <- FALSE


#Parameters for end analyses
# Analyse_dates <- c("start_follow_up","end_follow_up","birth_date")

findR::findRscript(pattern = "date_min", path = "C:/Users/mgamb/Documents/GitHub/ADEPT_obj1/DEVELOPMENT/ADEPT_scripts/p_steps/scripts_used_in_previous_study")
