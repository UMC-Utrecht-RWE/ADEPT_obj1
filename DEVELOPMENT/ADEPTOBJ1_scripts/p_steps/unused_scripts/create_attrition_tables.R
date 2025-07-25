# List all source population files
source_pops <- list.files(
  file.path(paths$D3_dir, "source_population"),
  pattern = "_source_population\\.rds$",
  full.names = TRUE
)

# Prepare an empty list to store attrition results
attrition_list <- list()

# Loop over each source population
for (spop in seq_along(source_pops)) {
  
  # Load population
  current_spop <- readRDS(source_pops[spop])
  spop_prefix <- sub("_source_population\\.rds$", "", basename(source_pops[spop]))
  
  # Start with full population size
  total_instance_population <- uniqueN(current_spop$person_id)
  before <- total_instance_population
  
  # Apply criteria
  sex_not_defined <- current_spop[!(sex_at_instance_creation %in% c("M", "F")), uniqueN(person_id)]
  birth_date_incomplete <- current_spop[is.na(year_of_birth) | year_of_birth < 1900 | year_of_birth > as.numeric(format(Sys.Date(), "%Y")), uniqueN(person_id)]
  death_date_incomplete <- current_spop[is.na(year_of_death) & (!is.na(day_of_death) | !is.na(month_of_death)), uniqueN(person_id)]
  nr_observation_periods_less_than_365_days <- current_spop[(as.IDate(op_end_date) - as.IDate(op_start_date)) < lookback_period, uniqueN(person_id)]
  persons_less_than_12_at_last_data_availability <- current_spop[date_min >= end_study_date, uniqueN(person_id)]
  women_more_than_55_at_start_of_observation <- current_spop[sex_at_instance_creation == "F" & date_max <= start_study_date, uniqueN(person_id)]
  
  # List of criteria and how many were lost
  criteria_names <- c(
    "sex_not_defined",
    "birth_date_incomplete",
    "death_date_incomplete",
    "nr_observation_periods_less_than_365_days",
    "persons_less_than_12_at_last_data_availability",
    "women_more_than_55_at_start_of_observation"
  )
  
  criteria_values <- c(
    sex_not_defined,
    birth_date_incomplete,
    death_date_incomplete,
    nr_observation_periods_less_than_365_days,
    persons_less_than_12_at_last_data_availability,
    women_more_than_55_at_start_of_observation
  )
  
  # Build attrition table
  attrition_dt <- data.table(
    selection_criteria = criteria_names,
    attrition = criteria_values
  )
  
  attrition_dt[, subpopulation := spop_prefix]
  attrition_dt[, before := shift(total_instance_population - cumsum(attrition), fill = total_instance_population)]
  attrition_dt[, after := before - attrition]
  
  # Arrange columns
  attrition_dt <- attrition_dt[, .(selection_criteria, subpopulation, before, after, attrition)]
  
  # Append to list
  attrition_list[[spop]] <- attrition_dt
}

# Combine all results
attrition_table <- rbindlist(attrition_list)

# Print to console
print(attrition_table)


