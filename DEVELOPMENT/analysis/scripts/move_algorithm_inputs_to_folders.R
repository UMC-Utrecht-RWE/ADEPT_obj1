# Move all algorithm input files into folders corresponding to the algorithm names they belong to

#TODO - THIS WILL BE FOR BOTH ATC & DX codes as some algorithms are made from both. 
# Dx subsets still needs to be done!
dp_algorithm_map <- algorithm_map[startsWith(Algorithm, "DP_")]

# <<< ALGORITHMS THAT ARE ALGORITHM INPUTS >>>

# Get names of all Variables that are also Algorithms 
algos_that_are_varnames <- dp_algorithm_map[Algorithm %in% VariableName, unique(Algorithm)]

# For each variable that is also listed as an Algorithm
for (v in algos_that_are_varnames) {
  
  # Get all component variable names that make up the current algorithm 'v'
  component_varnames <- dp_algorithm_map[Algorithm == v, VariableName]
  
  # Remove self-reference if present to avoid infinite recursion or duplication
  component_varnames <- setdiff(component_varnames, v)
  
  # Construct full paths to .rds files corresponding to each component variable
  component_files <- file.path(paths$D3_dir, "algorithm_input", paste0(pop_prefix, "_", component_varnames, ".rds"))

  # Check which component files exist
  file_exists <- file.exists(component_files)
  
  # Warn and skip if any files are missing
  if (any(!file_exists)) {
    warning("Missing files for algorithm ", v, ":\n", 
            paste(component_files[!file_exists], collapse = "\n"))
    next
  }
  
  # Read in all varnames for the variables, bind them, and save them under algorithm name
  combined_dt <- data.table::rbindlist(lapply(component_files, readRDS), use.names = TRUE, fill = TRUE)
  
  # Save under the varname-algorithm name
  saveRDS(combined_dt, file.path(file.path(paths$D3_dir, "algorithm_input"), paste0(pop_prefix, "_", v, ".rds")))
}

#<<< MOVE ALGORITHM INPUTS INTO RESPECTIVE FOLDERS >>> 

# Initialize a character vector to keep track of files that have been copied at least once
copied_files <- character()

# Loop over each unique algorithm name in the algorithm mapping table
for (algo in unique(dp_algorithm_map$Algorithm)) {
  
  # Get all variable names associated with the current algorithm
  varnames <- algorithm_map[Algorithm == algo, VariableName]
  
  # Create a directory named after the current algorithm inside the algorithm input directory
  algo_dir <- file.path(file.path(paths$D3_dir, "algorithm_input"), algo)
  # Create the directory if it does not exist
  dir_create(file.path(file.path(paths$D3_dir, "algorithm_input"), algo))
  
  # Loop through each variable name for this algorithm
  for (v in varnames) {
    
    # Construct the filename for the variable's RDS file based on population prefix and variable name
    src <- file.path(file.path(paths$D3_dir, "algorithm_input"), paste0(pop_prefix, "_", v, ".rds"))
    
    # Check if the RDS file exists before trying to copy it
    if (file_exists(src)) {
      
      # Copy the RDS file to the algorithm-specific folder, overwriting if the file already exists there
      file_copy(src, algo_dir, overwrite = TRUE)
      
      # Track the copied file path so it can be deleted later
      copied_files <- c(copied_files, src)
      
    } else {
      
      # Warn if the expected RDS file for the variable name does not exist
      message("For algorithm: ", algo, ", missing input: ", tools::file_path_sans_ext(basename(src)))
    }
  }
}

# Remove duplicate entries from the copied_files vector, in case any file was copied multiple times
copied_files <- unique(copied_files)

# Loop through all copied files and delete the originals to avoid duplication
for (f in copied_files) {if (file_exists(f)) {file_delete(f)}}

