#################################################################
# Create Study Population
################################################################
# If SUBP == TRUE, create a scheme table (SCHEME_04) that lists:
#   - The input file names for observation spells per subpopulation (e.g., "PC_OBS_SPELLS.rds")
#   - The output file names for the resulting source population files
#   - The output folder name
# This scheme table will be used in a loop to process each subpopulation separately.
if (SUBP) {
  SCHEME_04 <- copy(subpopulation_meanings)
  SCHEME_04[, ":=" (
    file_in = paste0(subpopulations, "_source_population.rds"), 
    file_out = paste0(subpopulations, "_study_population.rds")
  )]
}
# If SUBP == FALSE create a simplified scheme with only one row corresponding to the entire population ("ALL"):
#   - file_in:   the input spells file for all persons
#   - file_out:  the output file where the combined source population will be saved
#   - folder_out: name of the output folder for temporary/intermediate use
if (!SUBP) {
  SCHEME_04 <- data.table(subpopulations = "ALL")
  SCHEME_04[, `:=`(
    file_in = "ALL_source_population.rds", 
    file_out = "ALL_study_population.rds"
  )]
}
# Store flowcharts per subpopulation
flow_chart_source_to_study_list <- list()

# Loop over subpopulations 
for (i in seq_len(nrow(SCHEME_04))){
  # Initialize flowchart for this subpopulation
  flow_chart_source_to_study <- data.table(
    selection_criteria = character(),
    subpopulation      = SCHEME_04$subpopulations[i],
    before             = integer(),
    after              = integer(),
    attrition          = integer()
  )
  # Load the source dataset for the current subpopulation
  SOURCE <- readRDS(file.path(paths$D3_dir, "source_population", SCHEME_04[["file_in"]][i]))
  # Print message
  print("Exclude patients according to SelectionCriteria specified in to_run file")
  # Apply Selection Criteria
  for (j in seq_along(SelectionCriteria)){
    # Print message
    message(names(SelectionCriteria)[j])
    # Count rows before applying criterion
    before <- nrow(SOURCE)
    # Apply the criterion
    SOURCE <- SOURCE[eval(SelectionCriteria[[j]]), ]
    # Count rows after applying criterion
    after <- nrow(SOURCE)
    # Add to flow chart 
    flow_chart_source_to_study <- rbind(
      flow_chart_source_to_study,
      data.table(
        selection_criteria = names(SelectionCriteria)[j],
        subpopulation      = SCHEME_04$subpopulations[i],
        before             = before,
        after              = after,
        attrition          = before - after
      )
    )
  }
  # Print message
  print(paste0("Set start_follow up date and end follow_up_date ", SCHEME_04[["subpopulations"]][i]))
  # Set start and end follow up dates
  SOURCE[, start_follow_up := add_with_rollback(as.Date(entry_date), lookback_period)]
  SOURCE[, end_follow_up   := exit_date]
  # Count rows before checking for valid follow up time 
  before <- nrow(SOURCE)
  # keep rows only if start_follow_up is before end_follow_up
  SOURCE <- SOURCE[start_follow_up < end_follow_up, ]
  # Count rows after excluding invalid follow up time 
  after <- nrow(SOURCE)
  # Add to flow chart
  flow_chart_source_to_study <- rbind(
    flow_chart_source_to_study,
    data.table(
      selection_criteria = "start_follow_up later than end_follow_up",
      subpopulation      = SCHEME_04$subpopulations[i],
      before             = before,
      after              = after,
      attrition          = before - after
    )
  )
  # Create a copy of source population to create study population
  study_population <- copy(SOURCE)
  # Print message
  print(paste0("Calculate age at start and end follow up ", SCHEME_04[["subpopulations"]][i]))
  # create columns for age at start_follow_up and age_end_follow_up
  study_population[, ":=" (age_start_follow_up = floor(time_length(interval(birth_date, start_follow_up), "year")),
                           age_end_follow_up   = floor(time_length(interval(birth_date, end_follow_up), "year")))]
  SCHEME_04[i, "nrows"] <- nrow(study_population)
  SCHEME_04[i, "ncols"] <- ncol(study_population)
  # Save id as character
  study_population[,person_id:=as.character(person_id)]
  # save files
  saveRDS(study_population, file = file.path(paths$D3_dir, "study_population", SCHEME_04[["file_out"]][i]))
  # Save flow chart for this subpopulation
  flow_chart_source_to_study_list[[i]] <- flow_chart_source_to_study
}


flow_chart_source_to_study_combined <- rbindlist(flow_chart_source_to_study_list, use.names = TRUE, fill = TRUE)
# Combine and save all flowcharts 
saveRDS(flow_chart_source_to_study_combined, file = file.path(paths$D5_dir, "flowcharts", "source_to_study_flowchart.rds"))






  

  
 