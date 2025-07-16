# Creates a list of selection criteria used to extract the study population from the source population

# Load CDM source 
CDM_SOURCE <- fread(file.path(CDM_dir, list.files(CDM_dir, pattern = "^CDM_SOURCE")))

# Extract data from table 
data_source_name          <- CDM_SOURCE[, data_source_name]
data_access_provider_name <- CDM_SOURCE[, data_access_provider_name]
date_creation             <- as.IDate(as.character(CDM_SOURCE[, date_creation]), "%Y%m%d")
recommended_end_date      <- as.IDate(as.character(CDM_SOURCE[, recommended_end_date]), "%Y%m%d")

# Create interval vector for study period
intv <- c(start_study_date, end_study_date)

# Create Selection Criteria List

SelectionCriteria <- list(
  # Sex (Male or Female)
  sex = expression(sex_at_instance_creation=="F" | sex_at_instance_creation=="M"),
  # Year of birth is not missing and is not absurd
  no_year_of_birth = expression(!is.na(year_of_birth) & year_of_birth >= 1900 & year_of_birth <= as.numeric(format(Sys.Date(), "%Y"))),
  # Year of death is not missing if day and month is provided 
  no_year_of_death = expression(!(is.na(year_of_death) & (!is.na(day_of_death) | !is.na(month_of_death)))),
  # Year of death is not greater than year of birth and not in the future
  year_of_death_greater_than_year_of_birth = expression (is.na(year_of_death) | (year_of_death >= year_of_birth & year_of_death <= as.numeric(format(Sys.Date(), "%Y")))),
  # Ensures num_spell, op_start_date, and op_end_date are not missing.
  no_observation_time = expression(!is.na(num_spell) & !is.na(op_start_date) & !is.na(op_end_date)),
  # Persons >12 after end_study date 
  include_if_not_younger_than_12_after_study_end= expression(date_min < end_study_date),
  # Persons >55 before start_study date
  include_if_not_older_than_55_before_study_start = expression(sex_at_instance_creation == "M" | date_max > start_study_date),
  # Ensures that a spell starts before it ends.
  op_start_date_before_op_end_date = expression(op_start_date < op_end_date),
  # Ensures that spell overlaps with study period, including partially before/after.
  study_period_and_spell_overlap = expression(op_start_date %between% intv | op_end_date %between% intv | (op_start_date < start_study_date & op_end_date > end_study_date)),
  # Spell has to be at least 365 days 
  spells_more_than_lookback_period = expression((op_end_date - op_start_date) > lookback_period),
  # Ensures time from spell start to study end is enough to satisfy lookback.
  remaning_time_to_end_study_date_less_then_lookback_period = expression((end_study_date - op_start_date) > lookback_period)
 
)


# Load Metadata table 
METADATA <- fread(file.path(CDM_dir, list.files(CDM_dir, pattern = "^METADATA")))

# Extract the subpopulations value if present
subpop_value <- METADATA[type_of_metadata == "subpopulations", values]

# Check if subpopulations metadata exists and is not empty or NA
if (length(subpop_value) > 0 && !is.na(subpop_value) && subpop_value != "") {
  
  SUBP <- TRUE
  
  # Filter out technical metadata rows
  METADATA_subp <- METADATA[!type_of_metadata %in% c("presence_of_table", "presence_of_column", "list_of_values")]
  
  # Get operational meaning list per set and rename columns
  op_meaning_list_set <- METADATA[type_of_metadata == "op_meanings_list_per_set", .(op_meaning_sets = other, op_meanings_list_per_set = values)]
  
  # Get subpopulation meanings and rename columns
  subpopulation_meanings <- METADATA[type_of_metadata == "op_meaning_sets", .(subpopulations = other, meaning_sets = values)]
  
  # Split the subpopulations string by space into a vector
  subpopulations <- unlist(str_split(METADATA_subp[type_of_metadata == "subpopulations", values], pattern = " "))
  
} else {
  
  SUBP <- FALSE
  
}



