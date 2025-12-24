###############################################################################################################################################################################
# <<< Sub-objective 1.3: Continuous use rate >>>
# Measure: Annual continuous rate of ASM use during pregnancy
# Numerator: The number of pre-pregnancy users of an ASM within a calendar year that also runs into the first, second and third trimester of pregnancy
# Denominator: Total number of pregnancies in that calendar year in the data source
# Stratification by: Individual drug substance, drug sub-groups, indication, calendar year, data source

###############################################################################################################################################################################

print("===============================================================================================")
print("========================= CALCULATING CONTINUOUS USE DURING PREGNANCY =========================")
print("===============================================================================================")

# List overlaps
files_overlaps  <- list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE)

# For each overlapping dataset, look for continuous users (any12_6, any 6_0, any_t1, any_t2, any_t3 should all be TRUE)
for (overlap in seq_along(files_overlaps)) {
  
  # Assign overlap name
  overlap_name <- prefix <- sub("\\.rds$", "", basename(files_overlaps[overlap]))
  
  # Load episode 
  dt_overlap <- as.data.table(readRDS(files_overlaps[overlap]))
  
  # Get continuous users 
  if(!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) dt_continuous <- dt_overlap[any_12_6 == TRUE & any_6_0 == TRUE & any_t1 == TRUE & any_t2 == TRUE & any_t3 == TRUE,]
  if( deap_flags$is_EFEMERIS ||  deap_flags$is_FIN_REG) dt_continuous <- dt_overlap[any_before == TRUE & any_t1 == TRUE & any_t2 == TRUE & any_t3 == TRUE,]
  
  # Perform counts
  if(nrow(dt_continuous)>0){
    
    # Print Message
    message(paste0("There is continuous use of ", overlap_name, " in pregnancy"))
    
    # Get list of unique ids
    preg_ids_allt <- unique(dt_continuous$pregnancy_id)
    
    # Count the number of pregnancies with ASM use in first trimester, grouped by pregnancy year
    continuous_rate_counts <- pregnancies[pregnancy_id %in% preg_ids_allt, .N, by = preg_year]
    
    # Merge with template to get all years
    continuous_rate_all <- merge(empty_dt[, .(preg_year)], continuous_rate_counts, by = "preg_year", all.x = TRUE)
    
    # Merge with all pregnancies to get denominator
    continuous_rate_all <- merge(continuous_rate_all, total_preg_by_year, by = "preg_year", all.x = TRUE)
    
    # Set N = 0 and Freq = 0 for years with no counts
    continuous_rate_all[is.na(N), N := 0][is.na(Freq), Freq := 0]
    
    # Calculate rates
    continuous_rate_all[, rate := round(1000 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
    
    # Create column marking if rate is computable
    continuous_rate_all[, rate_computable := Freq > 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(continuous_rate_all[N > Freq]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
    if (nrow(continuous_rate_all[Freq == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
    
    # Save data where odd values
    if (nrow(continuous_rate_all[N > Freq]) > 0) fwrite(continuous_rate_all[N > Freq], file.path(paths$D5_dir, "1.3_pregnancy_continuous", paste0(overlap_name, "_t1_num_gt_denominator.csv")))
    if (nrow(continuous_rate_all[Freq == 0 & N != 0]) > 0) fwrite(continuous_rate_all[Freq == 0 & N != 0], file.path(paths$D5_dir, "1.3_pregnancy_continuous", paste0(overlap_name, "_t1_denominator_zero_numerator_nonzero.csv")))
    
    # Rename columns
    setnames(continuous_rate_all, c("N", "Freq"), c("n_treated", "n_total"))
    
    # Save files
    saveRDS(dt_continuous, file = file.path(paths$D4_dir, "1.3_pregnancy_continuous", paste0(overlap_name, "_continuous_use_rate_in_pregnancy_data.rds")))
    saveRDS(continuous_rate_all, file = file.path(paths$D5_dir, "1.3_pregnancy_continuous", paste0(overlap_name, "_continuous_use_rate_in_pregnancy_counts.rds")))
    
  } else {
    
    # Print Message
    message(paste0("There is NO continuous use of ", overlap_name, " in pregnancy"))
    
  }
  
}