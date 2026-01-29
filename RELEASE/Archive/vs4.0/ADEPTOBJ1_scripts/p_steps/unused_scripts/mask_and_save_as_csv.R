# In the case the global environment has been wiped clean: 
# open to_run.R script and run the lines listed below before continuing on with this script:
# if (!require(rstudioapi)) install.packages("rstudioapi")
# thisdir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Once those lines have been run, you can proceed with this script:
# Define root folder 
if (!exists("root_dir")) root_dir <- thisdir

# Assign names to paths 
paths <- list(D5_dir = file.path(root_dir, "D5_results"))

# Load packages
source(file.path(thisdir, "p_steps", "packages.R"), local = TRUE)

# Set paths to input and output folders 
input_root  <- paths$D5_dir
output_root <- file.path(thisdir, "D5_results_masked_csv")

# List all rds files in D5 folder recursively 
rds_files <- list.files(path = input_root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)

for (rds_path in rds_files) {
  
  # Read in file 
  dt <- as.data.table(readRDS(rds_path))
  
  
  ###############################################################################
  # START OF MASKING!
  ###############################################################################
  
  # Initialize masked column for all table types
  if (!"masked" %in% colnames(dt)) dt[, masked := FALSE]
  
  ###################################################################################################
  #  Masking for n_treated / n_total:
  ## incidence, prevalence, altmed, discontinued, switching, polytherapy, 
  ## pre-pregnancy, continuous use rate in pregnancy, initiation rates, polytherapy in pregnancy
  ###################################################################################################
  
  if (all(c("n_treated","n_total") %in% colnames(dt))) {
    
    # FULL MASK: numerator 1-4 AND denominator 1-4
    full_mask <- dt$n_treated >= 1 & dt$n_treated <= 4 & dt$n_total >= 1 & dt$n_total <= 4
    
    # PARTIAL MASK: numerator 1-4, denominator >=5
    partial_mask <- dt$n_treated >= 1 & dt$n_treated <= 4 & dt$n_total >= 5
    
    # DENOMINATOR ONLY MASK: numerator = 0, denominator 1-4
    denom_only_mask <- dt$n_treated == 0 & dt$n_total >= 1 & dt$n_total <= 4
    
    # Apply masks
    dt[full_mask, `:=`(n_treated=NA_integer_, n_total=NA_integer_, rate=NA_real_, masked=TRUE)]
    dt[partial_mask, `:=`(n_treated=NA_integer_, rate=NA_real_, masked=TRUE)]
    dt[denom_only_mask, `:=`(n_total=NA_integer_, rate=NA_real_, masked=TRUE)]
    
    # Rows not masked
    dt[!(full_mask | partial_mask | denom_only_mask), masked := FALSE]
    
  } 
  
  ###################################################################################################
  #  Masking for N / Freq, with secondary suppression per year
  ## stratified counts: incidence, prevalence, discontinued, polytherapy, 
  ## continuous use rate in pregnancy, initiation rates, polytherapy in pregnancy
  ###################################################################################################
  else if (all(c("N","Freq","year") %in% colnames(dt))) {
    
    # FULL MASK: numerator 1-4 AND denominator 1-4
    full_mask <- dt$N >= 1 & dt$N <= 4 & dt$Freq >= 1 & dt$Freq <= 4
    
    # PARTIAL MASK: numerator 1-4, denominator >=5
    partial_mask <- dt$N >= 1 & dt$N <= 4 & dt$Freq >= 5
    
    # DENOMINATOR ONLY MASK: numerator = 0, denominator 1-4
    denom_only_mask <- dt$N == 0 & dt$Freq >= 1 & dt$Freq <= 4
    
    # Apply masks
    dt[full_mask, `:=`(N=NA_integer_, Freq=NA_integer_, rate=NA_real_, masked=TRUE)]
    dt[partial_mask, `:=`(N=NA_integer_, rate=NA_real_, masked=TRUE)]
    dt[denom_only_mask, `:=`(Freq=NA_integer_, rate=NA_real_, masked=TRUE)]
    
    # Secondary suppression per year - if only one row is masked per year, we mask the second lowest value as well, to prevent counting back 
    years <- unique(dt$year)
    
    for (yy in years) {
      
      masked_rows <- which(dt$year == yy & dt$masked == TRUE)
      
      # apply secondary suppression only if exactly one row is masked in that year
      if (length(masked_rows) == 1) {
        
        # find row with the smallest unmasked numerator > 0
        unmasked_rows <- which(dt$year == yy & (dt$masked != TRUE | is.na(dt$masked)) & !is.na(dt$N) & dt$N > 0)
        
        if (length(unmasked_rows) > 0) {
          next_lowest_row <- unmasked_rows[which.min(dt$N[unmasked_rows])]
          dt[next_lowest_row, `:=`(
            N      = NA_integer_,
            Freq   = NA_integer_,
            rate   = NA_real_,
            masked = TRUE
          )]
        }
      }
    }
  } 
  
  ###################################################################################################
  #  Masking single columns: n_persons, comorbidity_counts, indication_counts
  ## treatment_duration, baseline comorbidity counts, baseline indication counts 
  ###################################################################################################
  
  else if (any(c("n_persons", "comorbidity_counts", "indication_counts") %in% colnames(dt))) {
    
    # Mask n_persons where value is 1-4
    if ("n_persons" %in% colnames(dt)) dt[n_persons >= 1 & n_persons <= 4, `:=`(n_persons = NA_integer_,masked = TRUE)]
    
    # Mask comorbidity_counts if value is 1-4
    if ("comorbidity_counts" %in% colnames(dt)) dt[comorbidity_counts >= 1 & comorbidity_counts <= 4, `:=`(comorbidity_counts = NA_integer_, masked = TRUE)]
    
    # Mask indication_counts if value is 1-4
    if ("indication_counts" %in% colnames(dt)) dt[indication_counts >= 1 & indication_counts <= 4, `:=`(indication_counts = NA_integer_, masked = TRUE)]
  }
  
  ###################################################################################################
  #  Masking objective 1.5: Secondary suppression is required per preg_start_year
  ###################################################################################################
  #-------------------------
  else if (all(c("n_in_period", "n_overall", "proportion", "preg_start_year") %in% colnames(dt))) {
    
    # FULL MASK: numerator 1-4 AND denominator 1-4
    full_mask <- dt$n_in_period >= 1 & dt$n_in_period <= 4 & dt$n_overall >= 1 & dt$n_overall <= 4
    
    # PARTIAL MASK: numerator 1-4, denominator >=5
    partial_mask <- dt$n_in_period >= 1 & dt$n_in_period <= 4 & dt$n_overall >= 5
    
    # DENOMINATOR ONLY MASK: numerator = 0, denominator 1-4
    denom_only_mask <- dt$n_in_period == 0 & dt$n_overall >= 1 & dt$n_overall <= 4
    
    # Apply masks
    dt[full_mask, `:=`(n_in_period = NA_integer_, n_overall = NA_integer_, proportion = NA_real_, masked = TRUE)]
    dt[partial_mask, `:=`(n_in_period = NA_integer_, proportion = NA_real_, masked = TRUE)]
    dt[denom_only_mask, `:=`(n_overall = NA_integer_, proportion = NA_real_, masked = TRUE)]
    
    # Secondary suppression per year
    years <- unique(dt$preg_start_year)
    
    for (yy in years) {
      
      masked_rows <- which(dt$preg_start_year == yy & dt$masked == TRUE)
      
      # apply secondary suppression only if exactly one row is masked in that year
      if (length(masked_rows) == 1) {
        
        # find row with the smallest unmasked numerator > 0
        unmasked_rows <- which(dt$preg_start_year == yy & (dt$masked != TRUE | is.na(dt$masked)) & !is.na(dt$n_in_period) & dt$n_in_period > 0)
        
        if (length(unmasked_rows) > 0) {
          next_lowest_row <- unmasked_rows[which.min(dt$n_in_period[unmasked_rows])]
          dt[next_lowest_row, `:=`(
            n_in_period = NA_integer_,
            n_overall   = NA_integer_,
            proportion  = NA_real_,
            masked      = TRUE
          )]
        }
      }
    }
    
    ###################################################################################################
    #  Masking Baseline Tables
    ###################################################################################################
    
  } else if (all(c("names","values") %in% colnames(dt))) {
    
    # Find rows that are counts (_count suffix)
    count_rows <- grep("_count$", dt$names)
    
    # Convert only these rows to numeric
    values_num <- suppressWarnings(as.numeric(dt$values))
    
    # Mask counts 1-4 (numeric only)
    mask_counts <- count_rows[!is.na(values_num[count_rows]) & values_num[count_rows] >= 1 & values_num[count_rows] <= 4]
    values_num[mask_counts] <- NA_real_
    
    # Mask corresponding percentages
    perc_rows <- match(sub("_count$", "_perc", dt$names[mask_counts]), dt$names)
    values_num[perc_rows] <- NA_real_
    
    # Secondary suppression: if only one count initially masked, mask next lowest unmasked numeric count
    if (length(mask_counts) == 1) {
      unmasked_counts <- setdiff(count_rows, mask_counts)
      unmasked_counts <- unmasked_counts[!is.na(values_num[unmasked_counts])]
      
      if (length(unmasked_counts) > 0) {
        second_lowest <- unmasked_counts[which.min(values_num[unmasked_counts])]
        values_num[second_lowest] <- NA_real_
        
        # Mask its corresponding percentage
        second_perc <- match(sub("_count$", "_perc", dt$names[second_lowest]), dt$names)
        if (!is.na(second_perc)) values_num[second_perc] <- NA_real_
        
        # Add secondary suppression to masked tracking
        mask_counts <- c(mask_counts, second_lowest)
        perc_rows <- c(perc_rows, second_perc)
      }
    }
    
    # Replace original numeric values in dt$values
    dt$values[count_rows] <- values_num[count_rows]
    dt$values[perc_rows] <- values_num[perc_rows]
    
    # Update masked column
    # dt[, masked := FALSE]
    dt[dt$names %in% dt$names[c(mask_counts, perc_rows)], masked := TRUE]
  }
  
  ###############################################################################
  # END OF MASKING!
  ###############################################################################
  
  # Save as csv file 
  relative_path <- substr(rds_path, nchar(input_root) + 2, nchar(rds_path))
  output_path <- file.path(output_root, relative_path)
  
  output_dir <- dirname(output_path)
  filename <- basename(output_path)
  
  base_filename <- tools::file_path_sans_ext(filename)
  new_filename <- paste0(DEAP_data, "_", base_filename, ".csv")
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  csv_path <- file.path(output_dir, new_filename)
  fwrite(dt, csv_path)
  
  if (!file.exists(csv_path)) {
    warning(paste("File was read but NOT saved to CSV:", rds_path))
  }
}
