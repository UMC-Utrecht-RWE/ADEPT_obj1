# set paths to input and output folders 
input_root  <- paths$D5_dir
output_root <- file.path(thisdir, "D5_results_masked_csv")

# find all .rds files recursively 
rds_files <- list.files(path = input_root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)

# loop through files
for (rds_path in rds_files) {
  
  # read in file 
  dt <- as.data.table(readRDS(rds_path))
  
  # Masking 
  if (all(c("n_treated","n_total") %in% colnames(dt))) {
    
    # Identify rows to mask
    mask_rows <- dt$n_treated < 5 | dt$n_total < 5
    
    # Apply masking
    dt[mask_rows, `:=`(
      n_treated = NA_integer_,
      n_total   = NA_integer_,
      rate      = NA_real_,
      masked    = TRUE
    )]
    
    # Rows not masked
    dt[!mask_rows, masked := FALSE]
    
  } else if (all(c("N","Freq") %in% colnames(dt))) {
    
    # Identify rows to mask
    mask_rows <- dt$N < 5 | dt$Freq < 5
    
    # Apply masking
    dt[mask_rows, `:=`(
      N        = NA_integer_,
      Freq     = NA_integer_,
      rate     = NA_real_,
      masked   = TRUE
    )]
    
    # Rows not masked
    dt[!mask_rows, masked := FALSE]
    
  } 
  # else -> do nothing
  
  # get relative path from input_root
  relative_path <- substr(rds_path, nchar(input_root) + 2, nchar(rds_path))
  
  # create output path 
  output_path <- file.path(output_root, relative_path)
  
  # Extract folder and file name
  output_dir <- dirname(output_path)
  filename <- basename(output_path)
  
  # Prefix file name with dap_name and change extension to .csv
  base_filename <- tools::file_path_sans_ext(filename)
  new_filename <- paste0(DEAP_data, "_", base_filename, ".csv")
  
  # Make sure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  csv_path <- file.path(output_dir, new_filename)
  # Write csv
  fwrite(dt, file.path(output_dir, new_filename))
  
  # Check if the CSV was actually saved
  if (!file.exists(csv_path)) {
    warning(paste("File was read but NOT saved to CSV:", rds_path))
  } else {
    # message(paste("Saved CSV for:", rds_path))
  }
}








