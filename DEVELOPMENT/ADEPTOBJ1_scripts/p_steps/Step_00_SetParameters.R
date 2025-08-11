#################################################################
# Create Selection Criteria List
################################################################

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
  
  # Sex is defined as either male or female.
  sex_not_defined = expression(sex_at_instance_creation=="F" | sex_at_instance_creation=="M"),
  
  # Year of birth is not missing or is complete
  birth_date_incomplete = expression(!is.na(year_of_birth) & year_of_birth >= 1900 & year_of_birth <= as.numeric(format(Sys.Date(), "%Y"))),
  
  # Year of death is not missing in the case that death has been recorded.
  death_date_incomplete = expression(!(is.na(year_of_death) & (!is.na(day_of_death) | !is.na(month_of_death)))),
  
  # Year of death is not greater than year of birth and is less than current year 
  year_of_death_greater_than_year_of_birth = expression(is.na(year_of_death) | (year_of_death >= year_of_birth & year_of_death <= as.numeric(format(Sys.Date(), "%Y")))),
  
  # Number of individuals within the source population with at least one year of available data in the data source.
  observation_period_less_than_1_year = expression((op_end_date - op_start_date) > 365), 
  
  # Persons who turn 12 before end_study_date
  persons_younger_than_12_before_end_study_date = expression(date_min < end_study_date),
  
  # All males and women who are below 56 at start_study_date 
  women_older_than_55_before_start_study_date = expression(sex_at_instance_creation == "M" | date_max > start_study_date),
  
  # observation period overlaps study period
  observation_period_does_not_overlap_study_period = expression(op_start_date %between% intv | op_end_date %between% intv | (op_start_date < start_study_date & op_end_date > end_study_date)),

  # This is similar to observation_period_greater_than_1_year when lookback is 365 days, however some DEAPS will have shorter look back 
  not_enough_lookback_time = expression((op_end_date - op_start_date) > lookback_period)
  
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
  
  # Get the exclude meanings only for EVENTS and PC
  exclude_meanings_PC <- unlist(strsplit(METADATA_subp[type_of_metadata == "exclude_meaning" & tablename == "EVENTS", values], "\\s+"))
  
  # Split the subpopulations string by space into a vector
  subpopulations <- unlist(str_split(METADATA_subp[type_of_metadata == "subpopulations", values], pattern = " "))
  
} else {
  
  SUBP <- FALSE
  
}
