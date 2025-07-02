print("======================================================================================")
print("========================= PROCESSING ALTERNATIVE MEDICATIONS =========================")
print("======================================================================================")

# Load bridge to get list of altmeds 
bridge <- unique(as.data.table(read_excel(file.path(thisdir, "definitions", "bridge", "ADEPT_O1_BRIDGE_19Mayo25.xlsx"), sheet = "OBJ1")))

# Create a list of altmed algorithms
altnames <- bridge[alternatives==TRUE, .(Varname)]

# Look for folders in dir
alt_folders <- data.table(folder_path = list.dirs(file.path(paths$D3_dir, "algorithm_input"), recursive = TRUE, full.names = TRUE))
alt_folders[, Varname := basename(folder_path)]

# Match folders to algorithm names
matched <- alt_folders[altnames, on = "Varname", nomatch = 0]

# Move folders to alternatives folder
# Move files from algorithm_input subfolders to alternatives/Varname/
for (i in seq_len(nrow(matched))) {
  from_folder <- matched$folder_path[i]
  to_folder <- file.path(paths$D3_dir, "alternatives", matched$Varname[i])
  
  # Create the destination folder if it doesn't exist
  dir.create(to_folder, recursive = TRUE, showWarnings = FALSE)
  
  # List all .rds files in the source folder
  rds_files <- list.files(from_folder, pattern = "\\.rds$", full.names = TRUE)
  
  # Move each .rds file individually to avoid nesting folders
  for (f in rds_files) {
    file.rename(f, file.path(to_folder, basename(f)))
  }
  
  # Optionally delete the original folder if it's empty
  if (length(list.files(from_folder)) == 0) {
    unlink(from_folder, recursive = TRUE)
  }
}


# Loop over folders 
alt_folders <- list.dirs(file.path(paths$D3_dir, "alternatives"), recursive = FALSE, full.names = TRUE)


# Loop through each alternative folder
for (folder in alt_folders) {
  
  # Get name of alternative group
  file_name <- basename(folder)
  
  message("Processing: ",pop_prefix, "_", file_name)
  # Read and combine all .rds files
  dt <- as.data.table(rbindlist(lapply(list.files(folder, pattern = paste0("^", pop_prefix, ".*\\.rds$"), full.names = TRUE), readRDS), use.names = TRUE, fill = TRUE))
  
  if (nrow(dt)){
  # Create a column with year of rx
  dt[, year := year(rx_date)][,source:=basename(folder)]
  dt <- unique(dt, by = c("person_id", "year"))
  

    # Count the number of persons per year. 
    alt_counts <- dt[, .("N" = .N), by = year]
    
    # Merge with denominator to calculate prevalence; include all years even if no treatments (all.y = TRUE)
    alt_all <- merge(alt_counts, denominator, by = "year", all.y = TRUE)
    
    # Set N = 0 for years with no treatments
    alt_all[is.na(N), N := 0]
    
    # Calculate altmeds per 1000 person
    alt_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(alt_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(alt_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Create column marking if rate is computable 
    alt_all[, rate_computable := !(Freq == 0 & N > 0)]
    
    # Rename columns
    setnames(alt_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save dataset 
    saveRDS(dt, file.path(paths$D4_dir, "1.2_altmeds", paste0(pop_prefix,"_",file_name, "_altmeds_data.rds")))
    
    # Save results 
    saveRDS(alt_all, file.path(paths$D5_dir, "1.2_altmeds", paste0(pop_prefix, "_", file_name, "_altmeds.rds")))
    
  } else {
    
    message(red(paste("No Altmeds were found for :", file_name)))
  }
}
