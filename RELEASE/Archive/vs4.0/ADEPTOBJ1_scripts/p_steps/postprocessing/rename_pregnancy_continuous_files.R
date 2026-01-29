# Renames pregnancy_continuous files post processing - issue has already been fixed in main script and should not occur in a future run

# set path to folder 1.3_pregnancy_continuous/stratified
in_dir <- "C:/Users/mgamb/Desktop/D5_results/1.3_pregnancy_continuous/stratified"

# find all files in folder, list with path 
files <- list.files(in_dir, pattern = "\\.rds$", full.names = TRUE)

# sanity check 
if (length(files) == 0) stop("No .rds files found in in_dir")

################################################################################
# prepare new names 
base_files <- basename(files)

# prepare pattern to be removed: continuous_use_rate_in_pregnancy_data.rds_
pattern <- "_continuous_use_rate_in_pregnancy_data\\.rds_continuous_use_rates_in_pregnancy_indication_counts$"

# replacement 
replacement <- "_continuous_use_rates_in_pregnancy_indication_counts"

# remove pattern from the basename(s)
new_base <- sub(pattern, replacement, base_files)

# build target new paths
new_paths <- file.path(in_dir, new_base)

################################################################################
# sanity checks 

# check for duplicates among target base names
if (any(duplicated(new_base))) {
  dupes <- new_base[duplicated(new_base) | duplicated(new_base, fromLast = TRUE)]
  stop("Duplicate target filenames would be created. Example duplicates:\n", paste(unique(dupes), collapse = ", "))
}

# check for existing files that are not the same as the source
conflicts <- file.exists(new_paths) & !(new_paths %in% files)

if (any(conflicts)) {
  conflict_names <- new_base[conflicts]
  stop("These target filenames already exist in the folder (would overwrite):\n", paste(conflict_names, collapse = ", "))
}

################################################################################
# preview mapping
preview <- data.frame(old = base_files, new = new_base, stringsAsFactors = FALSE)
print(preview, row.names = FALSE)

################################################################################
# identify files that need to be renamed
# (skip if old path == new path)
to_rename_idx <- which(files != new_paths)

################################################################################
# backup originals only for files that will be renamed ----------------------
if (length(to_rename_idx) > 0) {
  
  backup_dir <- file.path(in_dir, "backup_originals")
  dir.create(backup_dir, showWarnings = FALSE)
  copied <- file.copy(files[to_rename_idx], backup_dir, overwrite = FALSE)
  
  if (any(!copied)) message("Note: some files were not copied because they already exist in the backup folder.")
  
}

################################################################################
# rename files
if (length(to_rename_idx) == 0) {
  message("No filenames need changing. Nothing was renamed.")
} else {
  ok <- file.rename(files[to_rename_idx], new_paths[to_rename_idx])
  
  if (all(ok)) {
    message("All requested files renamed successfully.")
  } else {
    warning("Some renames failed. Failed files:")
    print(preview$old[to_rename_idx][!ok])
  }
}
