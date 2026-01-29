# Define root folder
if (!exists("root_dir")) root_dir <- thisdir

# Assign names to paths
paths <- list(D5_dir = file.path(root_dir, "D5_results"))

# Load packages
source(file.path(thisdir, "p_steps", "packages.R"), local = TRUE)

# Set paths to input and output folders
input_root  <- paths$D5_dir
output_root <- file.path(thisdir, "D5_results_aggregated")
# List all rds files in D5 folder recursively
rds_files <- list.files(path = input_root,
                        pattern = "\\.rds$",
                        recursive = TRUE,
                        full.names = TRUE)

# Remove files that are inside 'flowcharts' or 'baseline_tables' folders
rds_files <- rds_files[!grepl("/(flowcharts|baseline_tables|treatment_duration_months)/", rds_files, ignore.case = TRUE)]


for (rds_path in rds_files) {
  
  # Read file
  dt <- as.data.table(readRDS(rds_path))
  
  # if preg_year column in data: 
  if ("preg_year" %in% names(dt)){
    dt[preg_year >= 2000 & preg_year <= 2004, period := "2000-2004"]
    dt[preg_year >= 2005 & preg_year <= 2007, period := "2005-2007"]
    dt[preg_year >= 2008 & preg_year <= 2010, period := "2008-2010"]
    dt[preg_year >= 2011 & preg_year <= 2013, period := "2011-2013"]
    dt[preg_year >= 2014 & preg_year <= 2016, period := "2014-2016"]
    dt[preg_year >= 2017 & preg_year <= 2019, period := "2017-2019"]
    dt[preg_year >= 2020 & preg_year <= 2022, period := "2020-2022"]
    dt <- dt[preg_year >= 2000 & preg_year <= 2022,]
    
    if (grepl("agegroup", rds_path, ignore.case = TRUE)){
      dt_sum <- dt[, .(
        N    = sum(N, na.rm = TRUE),
        Freq = sum(Freq, na.rm = TRUE)
      ), by = .(period, age_group)]
      
      # calculate rates 
      dt_sum[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    }
    
    if (grepl("indication", rds_path, ignore.case = TRUE)){
      dt_sum <- dt[, .(
        N    = sum(N, na.rm = TRUE),
        Freq = sum(Freq, na.rm = TRUE)
      ), by = .(period, indication)]
      
      # calculate rates 
      dt_sum[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    }
    
    
    if (!grepl("agegroup|indication", rds_path, ignore.case = TRUE)){
      dt_sum <- dt[, .(
        n_treated = sum(n_treated, na.rm = TRUE),
        n_total   = sum(n_total, na.rm = TRUE)
      ), by = .(period)]
      
      # prepregnancy/initiation/continuous/polytherapy
      if (grepl("pre_pregnancy_counts\\.rds$|initiation_rates_in_pregnancy_counts\\.rds$|continuous_use_rate_in_pregnancy_counts\\.rds$|polytherapy_in_pregnancy_counts\\.rds$", rds_path)) {
        dt_sum[, rate := round(1000 * n_treated / n_total, 3)][n_treated == 0 & n_total == 0, rate := 0]
      }
      
      # discontinuation/switching
      if (grepl("discontinuation_in_pregnancies_counts\\.rds$|switching_in_pregnancies_counts\\.rds$", rds_path)) {
        dt_sum[, rate := round(100 * n_treated / n_total, 3)][n_treated == 0 & n_total == 0, rate := 0]
      }
      
      # create col preg_year (first year of period)
      dt_sum[, preg_year := as.integer(sub("-.*", "", period))]
      
      # reorder cols
      setcolorder(dt_sum, c("preg_year", "n_treated", "n_total", "rate", "period"))
    }

    # Get relative path using forward slashes
    rel_path <- sub(paste0("^", normalizePath(input_root, winslash = "/"), "/?"), "", normalizePath(rds_path, winslash = "/"))
    
    # Build output path safely
    output_path <- file.path(output_root, rel_path)
    
    # Make sure directory exists
    dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
    
    # Save file
    saveRDS(dt_sum, output_path)    
    
  }
}

# Obj 1.5 
# Keep 1.5 files only
rds_files_doses <- rds_files[grepl("dose_group_summary", rds_files, ignore.case = TRUE)]

# Extract prefixes (everything before _dose_group_summary_t#)
prefixes <- sub("_dose_group_summary_t[0-3]\\.rds$","",basename(rds_files_doses))
unique_prefixes <- unique(prefixes)

# Loop through prefixes
for(pfx in unique_prefixes){
  # Files for this prefix
  matched_files <- rds_files_doses[grepl(paste0("^", pfx, "_dose_group_summary_t[0-3]\\.rds$"), basename(rds_files_doses))]
  
  # Combine all files with the same name
  combined_dt <- rbindlist(lapply(matched_files, readRDS), use.names = TRUE, fill = TRUE)
  
  combined_dt[preg_start_year >= 2000 & preg_start_year <=2009, aggregated_years := "2000-2009"]
  combined_dt[preg_start_year >= 2010 & preg_start_year <=2018, aggregated_years := "2010-2018"]
  combined_dt[preg_start_year >= 2019 & preg_start_year <=2024, aggregated_years := "2019-2024"]
  combined_dt <- combined_dt[preg_start_year >= 2000 & preg_start_year <= 2022,]
  
  # Aggregate by aggregated years and period
  agg_dt <- combined_dt[, .(
    n_in_period = sum(n_in_period, na.rm = TRUE),
    n_overall = sum(n_overall, na.rm = TRUE)
  ), by = .(dose_group, period, aggregated_years)]
  
  # Calculate new rate 
  agg_dt[, proportion := (n_in_period/n_overall) * 100]
  agg_dt[n_in_period == 0 & n_overall == 0, proportion := 0]
  
  # -------------------------------
  # Save in mirrored folder structure
  # -------------------------------
  
  # Get folder of the first matched file
  original_dir <- dirname(matched_files[1])
  
  # Convert to relative path
  rel_path <- sub(paste0("^", normalizePath(input_root, winslash = "/"), "/?"), "", normalizePath(original_dir, winslash = "/"))
  
  # Build output folder under mirror
  output_folder <- file.path(output_root, rel_path)
  
  # Create folder if it does not exist
  dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  
  # Save file using prefix as filename
  saveRDS(agg_dt, file.path(output_folder, paste0("FINLAND_", pfx, ".rds")))
}

# SANITY CHECK
# Original dose group files
dose_files <- rds_files[grepl("dose_group_summary", rds_files, ignore.case = TRUE)]
dose_prefixes <- sub("_dose_group_summary_t[0-3]\\.rds$", "", basename(dose_files))  # define dose_prefixes here

# Aggregated files in mirror folder
agg_files <- list.files(output_root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
agg_prefixes <- sub("\\.rds$", "", basename(agg_files))
agg_prefixes <- sub("^FINLAND_", "", agg_prefixes)  # remove prefix to match original dose_prefixes

# Check which dose prefixes are NOT in the aggregated folder
not_saved_prefixes <- setdiff(unique(dose_prefixes), agg_prefixes)

if (length(not_saved_prefixes) == 0) {
  cat("All dose group files were aggregated and saved.\n")
} else {
  cat("Missing aggregated files for prefixes:\n")
  print(not_saved_prefixes)
}
