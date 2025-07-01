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
for (i in seq_len(nrow(matched))) {
  from <- matched$folder_path[i]
  to <- file.path(paths$D3_dir, "alternatives", matched$Varname[i])
  # Move folder
  file_move(from, to)
}


# Loop over folders 
alt_folders <- list.dirs(file.path(paths$D3_dir, "alternatives"), recursive = FALSE, full.names = TRUE)


# Loop through each alternative folder
for (folder in alt_folders) {
  
  # Get name of alternative group
  file_name <- basename(folder)
  
  message("Processing: ", file_name)
  # Read and combine all .rds files
  dt <- as.data.table(rbindlist(lapply(list.files(folder, pattern = "\\.rds$", full.names = TRUE), readRDS), use.names = TRUE, fill = TRUE))
  
  # Create a column with year of rx
  dt[, year := year(rx_date)][,source:=basename(folder)]
  dt <- unique(dt, by = c("person_id", "year"))

  # Count the number of persons per year. 
  alt_counts <- dt[, .("N" = .N), by = year]

  # Merge with denominator to calculate prevalence; include all years even if no treatments (all.y = TRUE)
  alt_all <- merge(alt_counts, denominator, by = "year", all.y = TRUE)
  
  # Set N = 0 for years with no treatments
  alt_all[is.na(N), N := 0]
  
  # Calculate prevalence per 1000 person
  alt_all[, rate := fifelse(Freq == 0, NA_real_, round(1000 * N / Freq, 3))]
  
  # Create column marking if rate is computable 
  alt_all[, rate_computable := Freq != 0]
  
  # Rename columns
  setnames(alt_all, c("N", "Freq"), c("n_treated", "n_total"))
  
  # Save dataset 
  saveRDS(dt, file.path(paths$D4_dir, "1.2_altmeds", paste0(pop_prefix,"_",file_name, "_altmeds_data.rds")))
  
  # Save results 
  saveRDS(alt_all, file.path(paths$D5_dir, "1.2_altmeds", paste0(pop_prefix, "_", file_name, "_altmeds.rds")))
  
}
