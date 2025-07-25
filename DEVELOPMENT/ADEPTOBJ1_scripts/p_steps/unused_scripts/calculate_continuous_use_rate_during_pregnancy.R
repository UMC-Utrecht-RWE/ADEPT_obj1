###############################################################################################################################################################################
# <<< Sub-objective 1.3: Continuous use rate >>> 
# Measure: Annual continuous rate of ASM use during pregnancy
# Numerator: The number of pre-pregnancy users of an ASM within a calendar year that also runs into the first, second and third trimester of pregnancy 
# Denominator: Total number of pregnancies in that calendar year in the data source
# Stratification by: Overall, individual drug substance, drug sub-groups, indication, calendar year, data source

# Pending: Stratification by Overall, indication
###############################################################################################################################################################################

print("===============================================================================================")
print("========================= CALCULATING CONTINUOUS USE DURING PREGNANCY =========================")
print("===============================================================================================")

# List all treatment episode files matching population prefix
files_episodes <- list.files(file.path(paths$D4_dir, "1.3_pre-pregnancy_use_rate"), pattern = "_pre_pregnancy_data\\.rds$")

# Keep only files that match population prefix AND contain "_F_" (female patients)
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# Drop PC_HOSP files if pop_prefix is PC
if(pop_prefix == "PC") files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]

# Pregnancy file loaded and cleaned in pre-pregnancy script


# Loop through each treatment episode file
for (episode in seq_along(files_episodes)) {
  
  # Get name of current ASM
  treatment_name <- gsub("_pre_pregnancy_data\\.rds$", "", files_episodes[episode])
  
  # Print Message
  message("Processing: ", treatment_name)
  
  # Load treatment episodes
  dt <- readRDS(file.path(paths$D4_dir, "1.3_pre-pregnancy_use_rate", files_episodes[episode]))
  
  # Convert episode dates to IDate
  dt[, episode.start := as.IDate(episode.start)][, episode.end := as.IDate(episode.end)]
  
  # Remove true duplicates
  dt <- unique(dt)
  
  ######################################################################
  ######################################################################
  ######################################################################
  # TEST 
  # Make sure dt is a data.table and person_id is character
  dt[person_id == "ConCDM_SIM_200421_00289", 
     `:=` (
       episode.start = as.IDate("2002-06-10"),
       episode.end = as.IDate("2003-05-08")
     )]
  
  ######################################################################
  ######################################################################
  ######################################################################
  
  # Set key for joining
  setkey(dt, person_id)
  
  # Add trimester windows
  dt[, t1_start := pregnancy_start_date][, t1_end := pregnancy_start_date + 90]
  dt[, t2_start := pregnancy_start_date + 91][, t2_end := pregnancy_start_date + 180]
  dt[, t3_start := pregnancy_start_date + 181][, t3_end := pregnancy_end_date]
  
  # create flags for all trimesters
  dt[, overlap_t1 := episode.start <= t1_end & episode.end >= t1_start]
  dt[, overlap_t2 := episode.start <= t2_end & episode.end >= t2_start]
  dt[, overlap_t3 := episode.start <= t3_end & episode.end >= t3_start]
  
  # Optional: flag if episode overlaps **all three trimesters**
  dt_all_trimester_overlap <- dt[overlap_t1 & overlap_t2 & overlap_t3]
  
  # Get list of unique ids 
  preg_ids_allt <- unique(dt_all_trimester_overlap$pregnancy_id)
  
  # Check if any pre-pregnancy ASM use was found 
  if(nrow(dt_all_trimester_overlap)>0){
    
    # Count the number of pregnancies with ASM use in first trimester, grouped by pregnancy year
    continuous_rate_counts <- pregnancies[pregnancy_id %in% preg_ids_allt, .N, by = preg_year]
    
    # Merge with template to get all years 
    continuous_rate_all <- merge(empty_dt[, .(preg_year)], continuous_rate_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    continuous_rate_all <- merge(continuous_rate_all, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    continuous_rate_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate rates
    continuous_rate_all[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Create column marking if rate is computable 
    continuous_rate_all[, rate_computable := Freq > 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(continuous_rate_all[N > Freq]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(continuous_rate_all[Freq == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Save data where odd values 
    if(nrow(continuous_rate_all[N > Freq])>0) fwrite(continuous_rate_all[N > Freq], file.path(paths$D5_dir, "1.3_continuous_use_rate", treatment_name, "_t1_num_gt_denominator.csv"))
    if(nrow(continuous_rate_all[Freq == 0 & N != 0])>0) fwrite(continuous_rate_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_continuous_use_rate", treatment_name, "_t1_denominator_zero_numerator_nonzero.csv"))
    
    # Rename columns 
    setnames(continuous_rate_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save files 
    saveRDS(dt_all_trimester_overlap, file = file.path(paths$D4_dir, "1.3_continuous_use_rate", paste0(treatment_name, "_continuous_use_rate_data.rds")))
    saveRDS(continuous_rate_all, file = file.path(paths$D5_dir, "1.3_continuous_use_rate", paste0(treatment_name, "_continuous_use_rate_counts.rds")))
    
  } else {
    message(red(paste0("There was no continuous use of ", treatment_name, " in the t1 trimester")))
  }
}