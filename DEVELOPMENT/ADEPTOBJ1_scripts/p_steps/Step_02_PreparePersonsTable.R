#################################################################
# Prepare Persons Table
################################################################

# Print message
print("Import and append persons files")

# Load all persons Tables
PERSONS <- as.data.table(rbindlist(lapply(list.files(CDM_dir, pattern = "^PERSONS", full.names = TRUE), fread), use.names = TRUE, fill = TRUE))

# Define vector of date-related column names
dates_persons <- c("year_of_birth", "month_of_birth", "day_of_birth", "year_of_death", "month_of_death", "day_of_death")

# Print message
print("Check if date variables are integer, if not set to integer")

# Convert 'month_of_birth' to character
PERSONS[, month_of_birth := as.character(month_of_birth)]

# For each date column, if it is not integer, convert it to integer silently (invisible output)
invisible(lapply(dates_persons, function(x) if (class(PERSONS[[x]]) != "integer") PERSONS[, eval(x) := as.integer(get(x))]))

# Print Message
print("Inpute birth and death day and month")

# For PERSONS missing birth day and month but have birth year, impute day=1, month=6 and flag imputation
PERSONS[is.na(day_of_birth) & is.na(month_of_birth) & !is.na(year_of_birth), ":=" (day_of_birth = 1, month_of_birth = 6, inputed_birth_day = TRUE, inputed_birth_month = TRUE)]

# If birth day missing but year known, impute day=16 and flag
PERSONS[is.na(day_of_birth) & !is.na(year_of_birth), ":=" (day_of_birth = 16, inputed_birth_day = TRUE)]

# If birth month missing but year known, impute month=6 and flag
PERSONS[is.na(month_of_birth) & !is.na(year_of_birth), ":=" (month_of_birth = 6, inputed_birth_month = TRUE)]

# Same as birth but for death day and month: impute day=1, month=6 and flag
PERSONS[is.na(day_of_death) & is.na(month_of_death) & !is.na(year_of_death), ":=" (day_of_death = 1, month_of_death = 6, inputed_death_day = TRUE, inputed_death_month = TRUE)]

# Impute death day=16 if missing and year known, flag
PERSONS[is.na(day_of_death) & !is.na(year_of_death), ":=" (day_of_death = 16, inputed_death_day = TRUE)]

# Impute death month=6 if missing and year known, flag
PERSONS[is.na(month_of_death) & !is.na(year_of_death), ":=" (month_of_death = 6, inputed_death_month = TRUE)]

# Create a subset table with person_id and flags for imputed birth/death dates
inputed <- PERSONS[inputed_birth_day == TRUE |
                     inputed_birth_month == TRUE |
                     inputed_death_day == TRUE |
                     inputed_death_month == TRUE,
                   .(person_id, inputed_birth_day, inputed_birth_month, inputed_death_day, inputed_death_month)]

# Save the imputation flag data to an RDS file
saveRDS(inputed, file = file.path(paths$D3_dir, "source_population", "inputed.rds"))

# Remove the imputation flag columns from PERSONS dataset
lapply(c("inputed_birth_day", "inputed_birth_month", "inputed_death_day", "inputed_death_month"), function(x) PERSONS <- PERSONS[, eval(x) := NULL])

# Print message
print("Create birth and death dates")

# For rows with complete birth dates, create 'birth_date' as a date object (IDate)
PERSONS[!is.na(day_of_birth) & !is.na(month_of_birth) & !is.na(year_of_birth), birth_date := as.IDate(paste0(year_of_birth, sprintf("%02d", month_of_birth), sprintf("%02d", day_of_birth)), "%Y%m%d")]

# Similarly, create 'death_date' as date object for complete death dates
PERSONS[!is.na(day_of_death) & !is.na(month_of_death) & !is.na(year_of_death), death_date := as.IDate(paste0(year_of_death, sprintf("%02d", month_of_death), sprintf("%02d", day_of_death)), "%Y%m%d")]

# Calculate age at study start by difference between birth_date and start_study_date in years, rounded down
PERSONS[, age_start_study := floor(time_length(interval(birth_date, start_study_date), "year"))]

# Save the cleaned PERSONS dataset to an RDS file
saveRDS(PERSONS, file = file.path(paths$D3_dir, "source_population", "persons.rds"))

# Check for duplicate person_id entries and stop if any found
if (any(duplicated(PERSONS[["person_id"]]))) stop("Duplicates in person table")
