#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

# Print message 
print('Import and append persons files')

# Import all files matching pattern "PERSONS" and append them into one data.table
PERSONS <- IMPORT_PATTERN(pat = "PERSONS", dir = path_CDM_dir)

# Print message
# print('Remove redundant variables')

# Remove columns 'race', 'country_of_birth', and 'quality' from PERSONS
# lapply(c("race","country_of_birth","quality"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])

# c("race","country_of_birth","quality")

# Define vector of date-related column names
dates_persons <- c("year_of_birth", "month_of_birth","day_of_birth","year_of_death", "month_of_death","day_of_death")

# Print message
print('Check if date variables are integer, if not set to integer')

# Convert 'month_of_birth' to character (probably for consistent handling)
PERSONS$month_of_birth <- as.character(PERSONS$month_of_birth)

# For each date column, if it is not integer, convert it to integer silently (invisible output)
invisible(lapply(dates_persons, function (x) if (class(PERSONS[[x]]) != "integer") PERSONS[, eval(x) := as.integer(get(x)) ]))

# Print Message
print('Inpute birth and death day and month')

# For persons missing birth day and month but have birth year, impute day=1, month=6 and flag imputation
PERSONS[is.na(day_of_birth) & is.na(month_of_birth) & !is.na(year_of_birth), ':=' (day_of_birth = 1, month_of_birth = 6, inputed_birth_day = T,inputed_birth_month = T)]

# If birth day missing but year known, impute day=16 and flag
PERSONS[is.na(day_of_birth) & !is.na(year_of_birth), ':=' (day_of_birth = 16, inputed_birth_day = T)]

# If birth month missing but year known, impute month=6 and flag
PERSONS[is.na(month_of_birth) & !is.na(year_of_birth), ':=' (month_of_birth = 6, inputed_birth_month = T)]

# Same as birth but for death day and month: impute day=1, month=6 and flag
PERSONS[is.na(day_of_death) & is.na(month_of_death) & !is.na(year_of_death), ':=' (day_of_death = 1, month_of_death = 6, inputed_death_day = T,inputed_death_month = T)]

# Impute death day=16 if missing and year known, flag
PERSONS[is.na(day_of_death) & !is.na(year_of_death), ':=' (day_of_death = 16, inputed_death_day = T)]

# Impute death month=6 if missing and year known, flag
PERSONS[is.na(month_of_death) & !is.na(year_of_death), ':=' (month_of_death = 6, inputed_death_month = T)]

# Create a subset table with person_id and flags for imputed birth/death dates
INPUTED <- PERSONS[inputed_birth_day == T |inputed_birth_month == T|inputed_death_day == T| inputed_death_month == T ,.(person_id,inputed_birth_day,inputed_birth_month,inputed_death_day,inputed_death_month)]

# Save the imputation flag data to an RDS file
saveRDS(INPUTED,file = paste0(path_study_source_pop,"/INPUTED.rds"))

# Remove the imputation flag columns from PERSONS dataset
lapply(c("inputed_birth_day","inputed_birth_month","inputed_death_day","inputed_death_month"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])

# Print message
print('Create birth and death dates')

# For rows with complete birth dates, create 'birth_date' as a date object (IDate)
PERSONS[!is.na(day_of_birth) & !is.na(month_of_birth) & !is.na(year_of_birth),birth_date := as.IDate(paste0(year_of_birth, sprintf("%02d",month_of_birth),sprintf("%02d",day_of_birth)),"%Y%m%d")]

# Similarly, create 'death_date' as date object for complete death dates
PERSONS[!is.na(day_of_death) & !is.na(month_of_death) & !is.na(year_of_death),death_date := as.IDate(paste0(year_of_death, sprintf("%02d",month_of_death),sprintf("%02d",day_of_death)),"%Y%m%d")]

# Calculate age at study start by difference between birth_date and start_study_date in years, rounded down
PERSONS <- PERSONS[,age_start_study := floor(time_length(interval(birth_date, start_study_date),"year"))]

# Print message
print('Delete abundant columns and tables')  
#lapply(c("day_of_death","month_of_death","year_of_death"), function (x) PERSONS <- PERSONS[,eval(x) := NULL])

# Save the cleaned PERSONS dataset to an RDS file
saveRDS(PERSONS,file = paste0(path_study_source_pop,"/PERSONS.rds"))

# Check for duplicate person_id entries and stop if any found
if(any(duplicated(PERSONS[["person_id"]]))) stop("Duplicates in person table") 
