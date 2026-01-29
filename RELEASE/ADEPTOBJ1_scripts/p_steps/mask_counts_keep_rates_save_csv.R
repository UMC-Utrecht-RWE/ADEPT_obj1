# In the case the global environment has been wiped clean:
# open to_run.R and run the lines listed below before continuing
# on with this script:

# if (!require(rstudioapi)) install.packages("rstudioapi")
# thisdir <- dirname(rstudioapi::getSourceEditorContext()$path)

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
rds_files <- list.files(path = input_root,
                        pattern = "\\.rds$",
                        recursive = TRUE,
                        full.names = TRUE)

for (rds_path in rds_files) {
  # Read file
  dt <- as.data.table(readRDS(rds_path))
  ##############################################################################
  # CASE 1: comorbidity_counts / indication_counts subfolders
  ##############################################################################
  if (grepl("baseline_tables/comorbidity_counts|baseline_tables/indication_counts", rds_path)) {
    # Identify which of the special columns actually exist in the data table
    special_cols <- intersect(names(dt),
                              c("comorbidity_counts", "indication_counts"))
    # Add a new logical column 'masked' to track whether a value has
    # been masked; initialize as FALSE
    dt[, masked := FALSE]
    # Loop over each special column and apply the masking rule
    for (col in special_cols) {
      # Set 'masked' to TRUE for rows where the value is less than 3
      dt[get(col) < 3, `:=`(masked = TRUE)]
      # Replace values less than 3 with NA in the column
      dt[get(col) < 3, (col) := NA]
    }
    ############################################################################
    # CASE 2: Files ending with baseline_table (row-based summary)
    ############################################################################
  } else if (grepl("baseline_tables/.+baseline_table\\.rds$", rds_path)) {
    # Get the name of the first column in the data table
    first_col <- names(dt)[1]
    # Keep only rows where the first column's value does NOT end with "_count"
    dt <- dt[!grepl("_count$", get(first_col))]
    ############################################################################
    # CASE 3: treatment duration - mask if less than 3 
    ############################################################################
  } else if (grepl("1\\.2_treatment_duration", rds_path)) {
    # Check if the column 'n_persons' exists in the data table
    if ("n_persons" %in% names(dt)) {
      # Add a new logical column 'masked' to track which rows have been masked;
      # initialize as FALSE
      dt[, masked := FALSE]
      # For rows where 'n_persons' is less than 3, mark them as masked
      dt[n_persons < 3, `:=`(masked = TRUE)]
      # Replace 'n_persons' values less than 3 with NA to mask small counts
      dt[n_persons < 3, n_persons := NA]
    }
    ############################################################################
    # CASE 4: Default - remove all count columns 
    ############################################################################
  } else {
    count_cols <- c("n_treated", "n_total", "N", "Freq",
                    "n_in_period", "n_overall",
                    "n_persons", "comorbidity_counts", "indication_counts")
    cols_to_remove <- intersect(names(dt), count_cols)
    if (length(cols_to_remove) > 0) dt[, (cols_to_remove) := NULL]
    count_suffix_cols <- grep("_count$", names(dt), value = TRUE)
    if (length(count_suffix_cols) > 0) dt[, (count_suffix_cols) := NULL]
  }
  ##############################################################################
  # SAVE AS CSV
  ##############################################################################
  relative_path <- substr(rds_path, nchar(input_root) + 2, nchar(rds_path))
  output_path   <- file.path(output_root, relative_path)
  output_dir    <- dirname(output_path)
  filename      <- basename(output_path)
  base_filename <- tools::file_path_sans_ext(filename)
  new_filename  <- paste0(DEAP_data, "_", base_filename, ".csv")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  csv_path <- file.path(output_dir, new_filename)
  fwrite(dt, csv_path)
  if (!file.exists(csv_path)) {
    warning(paste("File was read but NOT saved to CSV:", rds_path))
  }
}
