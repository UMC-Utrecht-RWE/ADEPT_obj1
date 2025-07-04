# If there are subpopulations, create SCHEME_04 by copying subpopulation_meanings
if(SUBP){
  SCHEME_04 <- copy(subpopulation_meanings)
  # Add file input/output names and output folder for each subpopulation
  SCHEME_04 <- SCHEME_04[, ':=' (file_in = paste0(subpopulations,"_source_population.rds"), 
                                 file_out = paste0(subpopulations,"_study_population.rds"), 
                                 folder_out = "populations") ]
  
}

# If not using subpopulations, create a single-row SCHEME_04 for the whole population
if(!SUBP) SCHEME_04 <- data.frame(subpopulations = c("ALL"),
                                  file_in = "ALL_source_population.rds", 
                                  file_out = "ALL_study_population.rds",
                                  folder_out = "populations")

# Initialize number of rows and columns as NA
SCHEME_04$nrows <- as.integer(NA)
SCHEME_04$ncols <- as.integer(NA)

# Expected number of columns needed in final dataset
SCHEME_04$ncolsneeded <- 23

# Initialize a data frame to track selection criteria and attrition across study subpopulations
FlowChartSourcetoStudy <- data.frame(selection_criteria = rep(NA,   length(SelectionCriteria)),
                                     subpopulation      = rep(SUBP, length(SelectionCriteria)), 
                                     before             = rep(NA,   length(SelectionCriteria)), 
                                     after              = rep(NA,   length(SelectionCriteria)),
                                     attrition          = rep(NA,   length(SelectionCriteria))
)
# Initialize a list 
FlowChart3 <- list()

# Loop through each row in the SCHEME_04 table, each representing a subpopulation and its input/output file
for(i in 1:nrow(SCHEME_04)){
  
  # Load the source dataset for the current subpopulation
  SOURCE <- readRDS(file.path(paths$D3_dir, "source_population", SCHEME_04[["file_in"]][i]))
  
  # Print message
  print('Exclude patients according to SelectionCriteria specified in to_run file')
  
  # Loop through each selection criterion
  for (j in 1:length(SelectionCriteria)){
    
    # Print message   
    print(names(SelectionCriteria)[j])
    
    # Count rows before applying criterion
    before <- nrow(SOURCE)
    # Apply the criterion
    SOURCE <- SOURCE[eval(SelectionCriteria[[j]]),]
    # Count rows after applying criterion
    after <- nrow(SOURCE)
    # Calculate how many rows were excluded 
    attrition<-before-after
    # Get the name of the criterion 
    crit_name<-names(SelectionCriteria)[j]
    
    # Record attrition details for the current criterion in the flowchart
    FlowChartSourcetoStudy$selection_criteria[j] <- crit_name
    FlowChartSourcetoStudy$before[j]             <- before
    FlowChartSourcetoStudy$after[j]              <- after
    FlowChartSourcetoStudy$attrition[j]          <- attrition
  }
  
  # Print message 
  print(paste0("Set start_follow up date and end follow_up_date ",SCHEME_04[["subpopulations"]][i]))
  
  # Create column start_follow_up - Max date of: 
  ## start_study_date
  ## op_start_date (previously set - max of date_min, op_start_date, start_study_date) + lookback period
  ## date_min 

  # study_population <- SOURCE[, start_follow_up := pmax(start_study_date, entry_date + lookback_period, date_min, na.rm = TRUE)]

  SOURCE[, entry_plus_1yr := as.IDate(seq(entry_date, length.out = 2, by = "1 year")[2], origin = "1970-01-01"), by = 1:nrow(SOURCE)]
  study_population <- SOURCE[, start_follow_up := pmax(start_study_date, entry_plus_1yr, date_min, na.rm = TRUE)]
  study_population <- study_population[, end_follow_up := pmin(end_study_date, exit_date, date_creation, recommended_end_date, date_max, na.rm = TRUE)]

  before <- nrow(study_population)
  study_population <- study_population[start_follow_up < end_follow_up ,]
  study_population <- study_population[(start_follow_up - op_start_date) >= lookback_period ,]
  after <- nrow(study_population)
  
  FlowChart3[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$step <- "04_CreateStudyPopulation"
  FlowChart3[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$population <- SCHEME_04[["subpopulations"]][i]
  FlowChart3[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$before <- before
  FlowChart3[[paste0("End_look_back_period_after_end_follow_up_",SCHEME_04[["subpopulations"]][i])]]$after <- after
  
  print(paste0("Calculate age at start and end follow up ",SCHEME_04[["subpopulations"]][i]))
  
  study_population <- study_population[, ':=' 
                                       ( age_start_follow_up = floor(time_length(interval(birth_date, start_follow_up), "year")),
                                         age_end_follow_up   = floor(time_length(interval(birth_date, end_follow_up  ), "year")) 
                                       )
  ]
  
  study_population <- study_population[, Population := SCHEME_04[["subpopulations"]][i]]
  SCHEME_04[i,"nrows"] <- nrow(study_population)
  SCHEME_04[i,"ncols"] <- ncol(study_population)
  
  saveRDS(study_population, file = file.path(paths$D3_dir, "study_population", SCHEME_04[["file_out"]][i]))
  saveRDS(FlowChartSourcetoStudy, file = file.path(paths$D5_dir, "flowcharts", "flowchart_source_to_study.rds"))
  
} 

saveRDS(FlowChart3, file = file.path(paths$D5_dir, "flowcharts", "flowchart3.rds"))
saveRDS(SCHEME_04, file = file.path(paths$D5_dir, "flowcharts", "scheme_04.rds"))











