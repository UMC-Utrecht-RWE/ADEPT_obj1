###############################################################################################################################################################################
# <<< Sub-objective 1.2: Alternative medications >>>
# Measure: Annual rate of alternative medication use
# Numerator: Number of individuals with one prescription/dispensing of all alternative medications for each indication of interest within a calendar year in the data source
# Denominator: Total number of person-time in that calendar year in the data source

###############################################################################################################################################################################

print("======================================================================================")
print("========================= PROCESSING ALTERNATIVE MEDICATIONS =========================")
print("======================================================================================")

# List all alternative medication files
files_altmeds <- list.files(file.path(paths$D3_dir, "alternatives"), pattern = "\\.rds$")

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  
  # Filter altmeds for current pop_prefix only
  files_altmeds <- files_altmeds[grepl(paste0("^", pop_prefix, "_"), files_altmeds)]
  
  # Load denominator file
  denominator <- readRDS(file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds"))) 
}

# Loop through each alternative file
for (altmed in seq_along(files_altmeds)) {
  
  # Get current altmed name 
  altmed_name <- gsub("_algo_med\\.rds$", "", files_altmeds[altmed]) #get name 
  
  # Print Message
  message("Processing group: ", altmed_name) # print name 
  
  # Load file
  dt <- readRDS(file.path(paths$D3_dir, "alternatives", files_altmeds[altmed]))
  dt <- unique (dt)
  
  # Save data set
  saveRDS(dt, file.path(paths$D4_dir, "1.2_altmeds", paste0(altmed_name, "_altmed_data.rds")))
  
  if (deap_flags$is_EFEMERIS || deap_flags$is_FIN_REG) next
  
  # Counts 
  # Keep records between start and end follow up 
  dt_clean <- dt[rx_date >= start_follow_up & rx_date <= end_follow_up]
  
  # Create a column with year of rx and name of altmed group it comes
  dt_clean[, `:=`(year = year(rx_date), source = altmed_name)]
  
  # Keep one person id per year 
  dt_clean <- unique(dt_clean, by = c("person_id", "year"))
  
  # Count number of incidence
  altmed_counts <- dt_clean[, .("N" = .N), by = year]
  
  # Merge with denominator
  altmed_all <- merge(altmed_counts, denominator, by = "year", all.y = TRUE)
  
  # Set N = 0 for years with no treatments
  altmed_all[is.na(N), N := 0]
  
  # Calculate incidence per 1000 person
  altmed_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
  
  # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
  if (nrow(altmed_all[N > Freq]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
  if (nrow(altmed_all[Freq == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
  
  # Save data with odd values
  if (nrow(altmed_all[N > Freq]) > 0) fwrite(altmed_all[N > Freq], file.path(paths$D5_dir, "1.2_altmeds", paste0(pop_prefix, "_", altmed_name, "_num_gt_denominator.csv")))
  if (nrow(altmed_all[Freq == 0 & N != 0]) > 0) fwrite(altmed_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.2_altmeds", paste0(pop_prefix, "_", altmed_name, "_denominator_zero_numerator_nonzero.csv")))
  
  # Create column marking if rate is computable i.e. if numerator is greater than denominator or if both numerator and denominator = 0
  altmed_all[, rate_computable := Freq > 0]
  
  # Rename columns
  setnames(altmed_all, c("N", "Freq"), c("n_treated", "n_total"))
  
  # Save results
  saveRDS(altmed_all, file.path(paths$D5_dir, "1.2_altmeds", paste0(altmed_name, "_altmed_counts.rds")))
}
