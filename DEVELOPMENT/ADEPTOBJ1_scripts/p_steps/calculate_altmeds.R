print("======================================================================================")
print("========================= PROCESSING ALTERNATIVE MEDICATIONS =========================")
print("======================================================================================")

# Load denominator file
denominator <- readRDS(file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))

# Get list of altmed folders 
altmed_group_folders <- basename(list.dirs(file.path(paths$D3_dir, "alternatives"), recursive = FALSE))

# Loop through each alternative folder
for (group in seq_along(altmed_group_folders)) {
  # Print message
  message("Processing group: ", pop_prefix ,"_", altmed_group_folders[group])
  
  # List all files in altmed folder
  files_altmeds <- list.files(file.path(paths$D3_dir, "alternatives", altmed_group_folders[group]), pattern = "\\.rds$")
  
  # Filter exposures for current pop_prefix only
  files_altmeds <- files_altmeds[grepl(paste0("^", pop_prefix, "_"), files_altmeds)]
  
  # If pop_prefix is PC, then drop any that are PC_HOSP
  if(pop_prefix=="PC"){files_altmeds <- files_altmeds[!grepl("PC_HOSP", files_altmeds)]}
  
  if(length(files_altmeds)>0){
    
    # For each folder read in files and bind them 
    dt_combined <- rbindlist(lapply(files_altmeds, function(f) readRDS(file.path(file.path(paths$D3_dir, "alternatives", altmed_group_folders[group]), f))), use.names = TRUE, fill = TRUE)
    
    dt_combined <- dt_combined[rx_date >= start_follow_up & rx_date <= end_follow_up,]
    
    if (nrow(dt_combined)>0){
      # Create a column with year of rxm and name of altmed group it comes 
      dt_combined[, year := year(rx_date)][,source:= altmed_group_folders[group]]
      
      # Remove duplicates: Keep only one person id per year
      dt_combined1 <- unique(dt_combined, by = c("person_id", "year"))
      # Count number of incidence 
      altmed_counts <- dt_combined1[, .("N" = .N), by = year]
      
      # Merge with denominator to calculate incidence; include all years even if no treatments (all.y = TRUE)
      altmed_all <- merge(altmed_counts, denominator, by = "year", all.y = TRUE)
      
      # Set N = 0 for years with no treatments
      altmed_all[is.na(N), N := 0]
      
      # Calculate incidence per 1000 person
      altmed_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
      
      # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
      if (nrow(altmed_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
      if (nrow(altmed_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
      
      # Save data with odd values 
      if(nrow(altmed_all[N > Freq])>0) fwrite(altmed_all[N > Freq], file.path(paths$D5_dir, "1.1_incidence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_num_gt_denominator.csv")))
      if(nrow(altmed_all[Freq == 0 & N != 0])>0) fwrite(altmed_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.1_incidence", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_denominator_zero_numerator_nonzero.csv")))
      
      # Create column marking if rate is computable i.e. if numerator is greater than denominator or if both numerator and denominator = 0
      altmed_all[, rate_computable := !(Freq == 0 & N >= 0)]
      
      # Rename columns
      setnames(altmed_all, c("N", "Freq"), c("n_treated", "n_total"))
      
      # Save dataset 
      saveRDS(dt_combined, file.path(paths$D4_dir, "1.2_altmeds", paste0(pop_prefix, "_", altmed_group_folders[group], "_altmed_data.rds")))
      
      # Save results 
      saveRDS(altmed_all, file.path(paths$D5_dir, "1.2_altmeds", paste0(pop_prefix, "_",altmed_group_folders[group], "_altmed.rds")))
      
    } else {
      
      message(red(paste("No Altmeds were found for :", pop_prefix, "_", altmed_group_folders[group])))
      
    }
  }
}



