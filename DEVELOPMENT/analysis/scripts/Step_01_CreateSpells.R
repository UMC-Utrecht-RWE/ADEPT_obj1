# Generates the observation spells used in the study.
# Observations with a gap of 7 days or less are concatenated,
# and in the case of multiple spells, the most recent is taken, and the others are discarded, resulting in one spell per personID

# Initialize empty list to store spells info for flowchart
flowchart_create_spells <- list()

# Print message
print('Import and append observation periods files') 

# Load Observation Spells 
OBSERVATION_PERIODS <- as.data.table(rbindlist(lapply(list.files(CDM_dir, pattern = "^OBSERVATION_PERIODS", full.names = TRUE), fread), use.names = TRUE, fill = TRUE))

# Check for missing Observation Dates

missing_OP_dates <- OBSERVATION_PERIODS[, .(
  total_rows          = .N,
  missing_start_date  = sum(is.na(op_start_date)),
  missing_end_date    = sum(is.na(op_end_date)),
  missing_both        = sum(is.na(op_start_date) & is.na(op_end_date)),
  missing_either      = sum(is.na(op_start_date) | is.na(op_end_date)),
  complete_dates      = sum(!is.na(op_start_date) & !is.na(op_end_date))
)]

# Save file
saveRDS(missing_OP_dates, file = file.path(paths$D5_dir, "flowcharts", "flowchart_missing_op_dates.rds"))

# Label for initial step in flowchart
step0 <- "original data" 

# Count rows in original data
step0_nrow <- nrow(OBSERVATION_PERIODS) 

# Count unique person IDs in original data
step0_unique <- length(unique(OBSERVATION_PERIODS$person_id)) 

# Description for next step
step1 <- 'Set start and end date to date format and if end date is empty fill with end study date' 

# Print progress
print('Set start and end date to date format and if end date is empty fill with end study date') 

# Convert start and end dates to Date format
lapply(c("op_start_date", "op_end_date"), function(x) OBSERVATION_PERIODS <- OBSERVATION_PERIODS[, eval(x) := as.IDate(as.character(get(x)), "%Y%m%d")])

# Fill missing end dates with end of study date
OBSERVATION_PERIODS <- OBSERVATION_PERIODS[is.na(op_end_date), op_end_date := end_study_date]

# For flow chart
step1_nrow <- nrow(OBSERVATION_PERIODS) # Count rows after date formatting

# Count unique person IDs after date formatting
step1_unique <- length(unique(OBSERVATION_PERIODS$person_id)) 

# Check if subpopulations exist
if (SUBP) {
  
  # Print message
  print("There are subpopulations, so a column with meaning_set is added as specified in metadata") 
  
  # Loop over meaning sets
  for (i in 1:nrow(op_meaning_list_set)) {
    # Assign meaning_set to observations matching op_meanings_list_per_set
    OBSERVATION_PERIODS[OBSERVATION_PERIODS[, op_meaning %in% unlist(str_split(op_meaning_list_set[i, op_meanings_list_per_set], pattern = " "))], meaning_set := op_meaning_list_set[i, op_meaning_sets]]
  }
  
  # Initialize list to store overlap info
  flowchart_overlap <- list() 
  
  # Print progress
  print("Create subpopulations subsets, Create spells and select latest") 
  
  # Loop over each subpopulation
  for (i in 1:nrow(subpopulation_meanings)) {
    # Print current subpopulation name
    print(subpopulation_meanings[["subpopulations"]][i]) 
    
    # Record row count before processing
    before <- nrow(OBSERVATION_PERIODS) 
    
    # Filter OBSERVATION_PERIODS for current subpopulation meaning_sets
    TEMP <- OBSERVATION_PERIODS[meaning_set %in% unlist(str_split(subpopulation_meanings[subpopulations == subpopulations[i], meaning_sets], pattern = " "))]
    
    # Keep only relevant columns
    TEMP <- TEMP[, c("person_id", "op_start_date", "op_end_date", "meaning_set")]
    
    # Count unique person IDs in TEMP
    original_unique_ID <- length(unique(TEMP$person_id)) 
    
    # If TEMP has rows
    if (nrow(TEMP) > 0) {
      
      # If subpop name contains underscore (PC_HOSP)
      if (length(strsplit(subpopulation_meanings[["subpopulations"]][i], "_")[[1]]) > 1) {
        # Print message
        print("Select only overlapping periods") 
        
        # Store row count at start
        flowchart_overlap[[paste0("Rows at start ", subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(TEMP)
        
        # Initialize counter for while loop
        k = 1
        
        # Get unique meaning_set values
        meaning_sets <- unique(TEMP[["meaning_set"]])
        
        # Loop while k less than number of meaning sets
        while (k < length(meaning_sets)) { 
          
          # First iteration
          if (k == 1) {
            # Assign first meaning set
            meaning_sets1 <- meaning_sets[k]
            
          } else {
            # Concatenate first two meaning sets
            meaning_sets1 <- paste0(meaning_sets[1], "_", meaning_sets[2])
            
          }
          
          # Assign next meaning set
          meaning_sets2 <- meaning_sets[k + 1]
          
          # Filter TEMP for these two sets
          TEMP <- TEMP[meaning_set %in% c(meaning_sets1, meaning_sets2), ]
          
          # Store row count for this pair
          flowchart_overlap[[paste0("Rows in pair ", meaning_sets1, "|||", meaning_sets2, " for", subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(TEMP)
          
          TEMP1<-copy(TEMP)
          # Call function to create spells with overlap allowed and gap of 7 days
          CreateSpells(  
            
            dataset = TEMP,
            id = "person_id",
            start_date = "op_start_date",
            end_date = "op_end_date",
            category = "meaning_set",
            replace_missing_end_date = "date_creation",
            overlap = T,
            dataset_overlap = "overlap",
            gap_allowed = 7
            
          )
          
          # Rename column names 
          setnames(overlap, "entry_spell_category", "op_start_date")
          setnames(overlap, "exit_spell_category", "op_end_date")
          
          # Convert to date format
          overlap[, op_start_date := as.IDate(op_start_date)]
          overlap[, op_end_date := as.IDate(op_end_date)]
          
          # Store count of overlapping rows
          flowchart_overlap[[paste0("Rows with overlap ", meaning_sets1, "|||", meaning_sets2, " for", subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(overlap)
          
          # If not last pair
          if (k < length(meaning_sets) - 1) {
            # Remaining meaning sets
            meaning_sets3 <- meaning_sets[k + 2:length(meaning_sets)]
            
            # Filter original data for remaining meaning sets
            TEMP <- copy(OBSERVATION_PERIODS)[meaning_set %in% meaning_sets3, c("person_id", "op_start_date", "op_end_date", "meaning_set")]
            
            # Store count
            flowchart_overlap[[paste0("Rows in that where not in pair ", meaning_sets[1], "|||", meaning_sets[2], " for", subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(TEMP)
            
            # Remove num_spell column from overlap
            overlap[, num_spell := NULL]
            
            # Combine overlap and TEMP
            TEMP <- rbind(overlap, TEMP, fill = T)
            
          } else {
            # Last iteration, TEMP is just overlap
            TEMP <- overlap
            
            # Filter rows where start < end
            TEMP <- TEMP[op_start_date < op_end_date, ]
            
          }
          
          # Store row count at end of this round
          flowchart_overlap[[paste0("Rows at end of round ", meaning_sets1, "|||", meaning_sets2, " for", subpopulation_meanings[["subpopulations"]][i])]]$count <- nrow(TEMP)
          
          # Increment counter
          k = k + 1
          
          # If no rows in TEMP
          if (nrow(TEMP) == 0) {
            # Exit loop
            k = length(meaning_sets)
          
            # Print message about no overlapping spells
            print(paste0(subpopulation_meanings[["subpopulations"]][i], " Has no overlapping spells so a file with 0 rows is returned"))
          }
        }
        
        # Keep only the max spell number per person
        if (nrow(TEMP) > 0) TEMP <- TEMP[, temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell, ][, temp := NULL]
        
        # If subpop name does not contain underscore (PC)
      } else {
      
        # Create spells without overlap
        TEMP <- CreateSpells(
          dataset = TEMP,
          id = "person_id",
          start_date = "op_start_date",
          end_date = "op_end_date",
          overlap = F,
          dataset_overlap = "overlap",
          replace_missing_end_date = end_study_date,
          only_overlaps = F
        )
        
        # Keep only the max spell number per person
        TEMP <- TEMP[, temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell, ][, temp := NULL]
        
        # Rename date columns 
        setnames(TEMP, "entry_spell_category", "op_start_date")
        setnames(TEMP, "exit_spell_category", "op_end_date")
        
        # Convert to date format
        TEMP[, op_start_date := as.IDate(op_start_date)]
        TEMP[, op_end_date := as.IDate(op_end_date)]

      }
      
      # If TEMP has no rows
    } else {
      
      # Create empty data.table with appropriate columns
      TEMP <- data.table(person_id = as.character(), op_start_date = as.IDate(x = integer(0), origin = "1970-01-01"), op_end_date = as.IDate(x = integer(0), origin = "1970-01-01"), meaning_set = as.character(), num_spell = as.numeric())
      
      # Warning message
      print(paste0(subpopulation_meanings[["subpopulations"]][i], " has no observations. Please check if metadata is filled correctly and if CDM contains observations for this subpopulation"))
      
    }
    
    # Save TEMP 
     saveRDS(TEMP, file = file.path(paths$D3_dir, "spells", paste0(subpopulation_meanings[["subpopulations"]][i], "_OBS_SPELLS.rds")))
    
    # Count rows after processing
    after <- nrow(TEMP)
    
    # Store flowchart info for this subpopulation
    flowchart_create_spells[[paste0("Spells_", subpopulation_meanings[["subpopulations"]][i])]]$step <- "01_CreateSpells"
    flowchart_create_spells[[paste0("Spells_", subpopulation_meanings[["subpopulations"]][i])]]$original_unique_ID <- original_unique_ID
    flowchart_create_spells[[paste0("Spells_", subpopulation_meanings[["subpopulations"]][i])]]$population <- subpopulation_meanings[["subpopulations"]][i]
    flowchart_create_spells[[paste0("Spells_", subpopulation_meanings[["subpopulations"]][i])]]$before <- before
    flowchart_create_spells[[paste0("Spells_", subpopulation_meanings[["subpopulations"]][i])]]$after <- after

    # Save flowchart list to file
    saveRDS(flowchart_create_spells, file = file.path(paths$D5_dir, "flowcharts", "SUBPOP_flowchart_overlap.rds"))
  }
  
  # If no subpopulations exist
} else {
 
  # Print message
  print("Create spells and select latest for ALL")
  
  # Count unique person IDs
  before_CreateSpells <- nrow(OBSERVATION_PERIODS)
  
  # Create spells without overlap for entire dataset
  OBSERVATION_PERIODS1 <- CreateSpells(
    dataset = OBSERVATION_PERIODS,
    id = "person_id",
    start_date = "op_start_date",
    end_date = "op_end_date",
    overlap = FALSE,
    only_overlaps = F,
    gap_allowed = 7
  )
  
  # Print message
  print("CreateSpells run OK")
  
  # Count rows in Observation Periods 1
  after_CreateSpells <- nrow(OBSERVATION_PERIODS1)
  
  # Print message
  print("select most recent Observation Period")
  
  # Count rows in Observation Periods1 after keeping max spell only
  select_most_recent <- nrow(OBSERVATION_PERIODS1)
  
  # Print Message
  print("CLEANUP OBSERVATION_PERIODS1")
  
  # keep only the row with the maximum num_spell
  OBSERVATION_PERIODS1 <- OBSERVATION_PERIODS1[, temp := lapply(.SD, max), by = c("person_id"), .SDcols = "num_spell"][temp == num_spell, ][, temp := NULL]
  
  # Rename date columns 
  setnames(OBSERVATION_PERIODS1, "entry_spell_category", "op_start_date")
  setnames(OBSERVATION_PERIODS1, "exit_spell_category", "op_end_date")
  
  # Convert start date to Date format
  OBSERVATION_PERIODS1[, op_start_date := as.IDate(op_start_date)]
  OBSERVATION_PERIODS1[, op_end_date := as.IDate(op_end_date)]
  
  # Save Observation Periods
  saveRDS(OBSERVATION_PERIODS1, file = file.path(paths$D3_dir, "spells", "ALL_OBS_SPELLS.rds"))
  
  
  # Print Message
  print("store FlowChart data on attrition")
  
  # Define descriptive steps for the flowchart to explain each count
  CreateSpellsStep <- c("original number of OBSERVATION PERIODS", "original number of unique personID",
                        "number of OBSERVATION PERIODS after concatenating observations with gaps <= 7 days",
                        "number of OBSERVATION PERIODS after selecting the most recent observation (one spell per unique ID)")
  
# Create flow chart
  OBS_number <- c(before_CreateSpells, step0_unique, after_CreateSpells, select_most_recent)
  
  # Combine steps and numbers into a data frame for reporting
  flowchart_create_spells <- as.data.frame(cbind(CreateSpellsStep, OBS_number))
  
  # Save the flowchart
  saveRDS(flowchart_create_spells, file = file.path(paths$D5_dir, "flowcharts", "flowchart_create_spells.rds"))
  
  # If an object called flowchart_overlap exists, Save it
  if (exists("flowchart_overlap")) {
    saveRDS(flowchart_overlap, file = file.path(paths$D5_dir, "flowcharts", "SUBPOP_flowchart_overlap.rds"))
  }
}

