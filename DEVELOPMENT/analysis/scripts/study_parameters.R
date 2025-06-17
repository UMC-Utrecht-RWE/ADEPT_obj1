###################################################
# Set Study Parameters
#################################################

# <<< Define study_start_date - different per DEAP >>> #

# Read in all Observation Periods (if more than one)
OBSERVATION_PERIODS <- as.data.table(rbindlist(lapply(list.files(CDM_dir, pattern = "^OBSERVATION_PERIODS", full.names = TRUE), fread), use.names = TRUE, fill = TRUE))

# Get earliest observation start date in data
earliest_in_data <- OBSERVATION_PERIODS[, min(as.IDate(as.character(op_start_date), format = "%Y%m%d"), na.rm = TRUE)]

# set default start date
default_start <- as.IDate("2000-01-01")

# Start_study date is the max of earliest in data and default start date
start_study_date <- max(earliest_in_data, default_start, na.rm = TRUE)

# <<< Define study_end_date - different per DEAP >>> #

# Read CDM source file to get recommended end date 
CDM_SOURCE <- fread(file.path(CDM_dir, list.files(CDM_dir, pattern = "^CDM_SOURCE")))

# Assign end_study_date
end_study_date <- as.IDate(as.character(CDM_SOURCE[, recommended_end_date]), "%Y%m%d")

# <<< Other Parameters >>> #

# Other parameters
age_min          <- 12 # both males and females
age_max          <- 55 # females only
lookback_period  <- 365





