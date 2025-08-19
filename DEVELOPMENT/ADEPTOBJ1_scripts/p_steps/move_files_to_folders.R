#################################################################
# Move files to folders
#################################################################

print("=========================================================================")
print("========================= MOVE FILES TO FOLDERS =========================")
print("=========================================================================")

# Define flags to check
flags <- c("exposure", "cov", "indication", "alternatives", "algorithm_input")

# List all .rds files in the source directory
rds_files <- list.files(paths$D3_dir, pattern = "\\.rds$", full.names = TRUE)

# Build file info table
file_info <- data.table(path = rds_files)
file_info[, file := basename(path)]
file_info[, Varname := sub(paste0("^", pop_prefix, "_"), "", sub("\\.rds$", "", file))]
file_info[, Varname := trimws(Varname)]

# Merge with the bridge (concept set table)
merged <- merge(file_info, bridge, by = "Varname", all.x = TRUE)

# Track which files were successfully copied to at least one folder
copied_files <- character()

# Loop over flags
for (flag in flags) {
  
  # Get Varname values where flag == TRUE
  subset_vars <- bridge[get(flag) == TRUE, unique(Varname)]
  
  # Get matching files
  subset_files <- merged[Varname %in% subset_vars]
  
  # Remove duplicate entries
  subset_files <- unique(subset_files, by = "file")
  
  # Destination directory
  dest_dir <- file.path(paths$D3_dir, flag)
  if (!dir_exists(dest_dir)) dir_create(dest_dir)
  
  # Loop over matching files
  for (i in seq_len(nrow(subset_files))) {
    row <- subset_files[i]
    source_path <- row$path
    dest_path <- file.path(dest_dir, row$file)
    
    # Skip if already in destination
    if (dirname(source_path) == dest_dir) next
    
    if (file_exists(source_path)) {
      tryCatch({
        file_copy(source_path, dest_path, overwrite = TRUE)
        # message(sprintf("Copied '%s' to folder '%s'", row$file, flag))
        copied_files <- unique(c(copied_files, row$file))
      }, error = function(e) {
        message(sprintf("Failed to copy '%s' to folder '%s': %s", row$file, flag, e$message))
      })
    }
  }
}

# === DELETE ORIGINALS for files that were copied ===

# Only one original path per file
original_files_to_delete <- file_info[file %in% copied_files]
original_files_to_delete <- unique(original_files_to_delete, by = "file")

for (i in seq_len(nrow(original_files_to_delete))) {
  row <- original_files_to_delete[i]
  if (file_exists(row$path)) {
    tryCatch({
      file_delete(row$path)
      # message(sprintf("Deleted original file '%s'", row$file))
    }, error = function(e) {
      message(sprintf("Failed to delete '%s': %s", row$file, e$message))
    })
  }
}

# === REPORT UNCOPIED FILES ===

not_copied <- setdiff(file_info$file, copied_files)
if (length(not_copied) > 0) {
  message("\nFiles NOT copied to any folder:")
  for (file in not_copied) message(" - ", file)
} else {
  message("\nAll files copied to folders and originals deleted.")
}


#<<< MOVE ALGORITHM INPUTS INTO RESPECTIVE FOLDERS >>> 

# Initialize a character vector to keep track of files that have been copied at least once
copied_files <- character()

# Loop over each unique algorithm name in the algorithm mapping table
for (algo in unique(algorithm_map$Algorithm)) {
  # print(algo)
  # Get all variable names associated with the current algorithm
  varnames <- algorithm_map[Algorithm == algo, VariableName]
  
  # Create a directory named after the current algorithm inside the algorithm input directory
  algo_dir <- file.path(paths$D3_dir, "algorithm_input", algo)
 
  # Create the directory if it does not exist
  dir_create(file.path(paths$D3_dir, "algorithm_input", algo))
  
  # Loop through each variable name for this algorithm
  for (v in varnames) {
    
    # Construct the filename for the variable's RDS file based on population prefix and variable name
    src <- file.path(paths$D3_dir, "algorithm_input", paste0(pop_prefix, "_", v, ".rds"))
    
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

# Bind algorithm inputs before moving to folders 

bind_and_save_group <- function(group_name) {
  alg_folders <- data.table(folder_path = list.dirs(file.path(paths$D3_dir, "algorithm_input"), recursive = TRUE, full.names = TRUE))
  alg_folders[, Varname := basename(folder_path)]
  
  algos <- bridge[get(group_name) == TRUE & algorithm == TRUE, .(Varname)]
  matched <- alg_folders[algos, on = "Varname", nomatch = 0]
  
  to_folder <- file.path(paths$D3_dir, group_name)
  dir_create(to_folder)
  
  for (i in seq_len(nrow(matched))) {
    from_folder <- matched$folder_path[i]
    algo_name <- matched$Varname[i]
    
    rds_files <- list.files(from_folder, pattern = "\\.rds$", full.names = TRUE)
    
    if (length(rds_files) == 0) {
      unlink(from_folder, recursive = TRUE, force = TRUE)
      
      next
    }
    
    # Separate medicine vs diagnosis files
    is_medicine <- grepl(paste0("^", pop_prefix, "_DP_"), basename(rds_files))
    med_files <- rds_files[is_medicine]
    diag_files <- rds_files[!is_medicine]
    
    # Bind and save medicine files
    if (length(med_files) > 0) {
      med_list <- lapply(med_files, readRDS)
      combined_med <- rbindlist(med_list, use.names = TRUE, fill = TRUE)
      saveRDS(combined_med, file = file.path(to_folder, paste0(pop_prefix, "_", algo_name, "_algo_med.rds")))
    }
    
    # Bind and save diagnosis files
    if (length(diag_files) > 0) {
      diag_list <- lapply(diag_files, readRDS)
      combined_diag <- rbindlist(diag_list, use.names = TRUE, fill = TRUE)
      saveRDS(combined_diag, file = file.path(to_folder, paste0(pop_prefix, "_", algo_name, "_algo_dx.rds")))
    }
    
    unlink(from_folder, recursive = TRUE, force = TRUE)
  }
  
  message("Finished processing group: ", group_name)
}


bind_and_save_group("alternatives")
bind_and_save_group("exposure")
bind_and_save_group("indication")
bind_and_save_group("cov")


# clean up before moving on
rm(list = grep("algorithms|alt_component|alt_components|alt_with_atc|alternatives_list|ATC_codelist|ATC_concept|bridge|code|codelist|combined|concept_|copied_files|current_table|dt|event_files|file_info|files_to_bind|matched|med_files|merged|not_copied|original|rds_files|row|subset|varname|voc", ls(), value = TRUE, ignore.case = TRUE))
