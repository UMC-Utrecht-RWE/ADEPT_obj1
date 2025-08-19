#################################################################
# Create Study Population
################################################################

# If SUBP == TRUE, create a scheme table (SCHEME_04) that lists:
#   - The input file names for observation spells per subpopulation (e.g., "PC_OBS_SPELLS.rds")
#   - The output file names for the resulting source population files
#   - The output folder name
# This scheme table will be used in a loop to process each subpopulation separately.

if(SUBP){
  SCHEME_04 <- copy(subpopulation_meanings)
  SCHEME_04[, ':=' (file_in = paste0(subpopulations,"_source_population.rds"), file_out = paste0(subpopulations,"_study_population.rds"))]
}

# If SUBP == FALSE create a simplified scheme with only one row corresponding to the entire population ("ALL"):
#   - file_in:   the input spells file for all persons
#   - file_out:  the output file where the combined source population will be saved
#   - folder_out: name of the output folder for temporary/intermediate use
if(!SUBP) {
  SCHEME_04 <- data.table(subpopulations = "ALL")
  SCHEME_04[, `:=` (file_in = "ALL_source_population.rds", file_out = "ALL_study_population.rds")]
}

# Initialize lists 
flow_chart_check_lookback <- list()
flow_chart_source_to_study_list <- list()

# Loop through each row in the SCHEME_04 table, each representing a subpopulation and its input/output file
for(i in 1:nrow(SCHEME_04)){
  
  flow_chart_source_to_study <- data.table(
    selection_criteria = names(SelectionCriteria),
    subpopulation      = SCHEME_04[["subpopulations"]][i],
    before             = NA_integer_,
    after              = NA_integer_,
    attrition          = NA_integer_
  )
  
  # Load the source dataset for the current subpopulation
  SOURCE <- readRDS(file.path(paths$D3_dir, "source_population", SCHEME_04[["file_in"]][i]))
  
  # Print message
  print('Exclude patients according to SelectionCriteria specified in to_run file')
  
  # Loop through each selection criterion
  for (j in 1:length(SelectionCriteria)){
    
    # Print message   
    message(names(SelectionCriteria)[j])
    
    # Count rows before applying criterion
    before <- nrow(SOURCE)
    # Apply the criterion
    SOURCE <- SOURCE[eval(SelectionCriteria[[j]]),]
    # Count rows after applying criterion
    after <- nrow(SOURCE)
    # Calculate how many rows were excluded 
    attrition <- before-after
    # Get the name of the criterion 
    crit_name <- names(SelectionCriteria)[j]
    
    # Record attrition details for the current criterion in the flowchart
    flow_chart_source_to_study$selection_criteria[j] <- crit_name
    flow_chart_source_to_study$before[j]             <- before
    flow_chart_source_to_study$after[j]              <- after
    flow_chart_source_to_study$attrition[j]          <- attrition
  }
  
  # 
  flow_chart_source_to_study_list[[i]] <- flow_chart_source_to_study
  
  # Print message 
  print(paste0("Set start_follow up date and end follow_up_date ",SCHEME_04[["subpopulations"]][i]))
  
  # Set start and end follow up dates 
  if(deap_flags$is_EFEMERIS || deap_flags$is_FIN_REG){
    # start fu is the same as op_start_date
    SOURCE[, start_follow_up := pmax(op_start_date, entry_date)]
    SOURCE[, end_follow_up   := pmin(op_end_date, exit_date, na.rm = TRUE)]
    
  } else {
    # create columns start and end follow up
    SOURCE[, start_follow_up := add_with_rollback(as.Date(entry_date), lookback_period)]
    SOURCE[, end_follow_up   := exit_date]
  }
  
  study_population <- copy(SOURCE)
  # # attrition
  # before <- nrow(study_population)
  # # keep rows only if start_follow_up is before end_follow_up
  # study_population <- study_population[start_follow_up < end_follow_up ,]
  # 
  # after <- nrow(study_population)
  
  # flow_chart_check_lookback[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$step <- "04_CreateStudyPopulation"
  # flow_chart_check_lookback[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$population <- SCHEME_04[["subpopulations"]][i]
  # flow_chart_check_lookback[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$before <- before
  # flow_chart_check_lookback[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$after <- after
  
  print(paste0("Calculate age at start and end follow up ",SCHEME_04[["subpopulations"]][i]))
  
  # create columns for age at start_follow_up and age_end_follow_up
  study_population[, ':=' 
                   ( age_start_follow_up = floor(time_length(interval(birth_date, start_follow_up), "year")),
                     age_end_follow_up   = floor(time_length(interval(birth_date, end_follow_up  ), "year")) 
                   )
  ]
  
  study_population[, Population := SCHEME_04[["subpopulations"]][i]]
  SCHEME_04[i,"nrows"] <- nrow(study_population)
  SCHEME_04[i,"ncols"] <- ncol(study_population)
  
  # save files 
  saveRDS(study_population, file = file.path(paths$D3_dir, "study_population", SCHEME_04[["file_out"]][i]))
} 

flow_chart_source_to_study_combined <- rbindlist(flow_chart_source_to_study_list, use.names = TRUE, fill = TRUE)
saveRDS(flow_chart_source_to_study_combined, file = file.path(paths$D5_dir, "flowcharts", "source_to_study_flowchart.rds"))

# saveRDS(flow_chart_check_lookback, file = file.path(paths$D5_dir, "flowcharts", "flow_chart_check_lookback.rds"))
# saveRDS(SCHEME_04, file = file.path(paths$D5_dir, "flowcharts","scheme_04.rds"))
