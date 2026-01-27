###############################################################################################################################################################################
# <<< Sub-objective 1.4: Discontinuation rates during pregnancy >>>
# Measure1: Annual pre-pregnancy discontinuation rate of ASM
# Numerator1: The number of pre-pregnancy users of an ASM within a calendar year that does not run into the pregnancy period
# Denominator1: Total number of pre-pregnancy users of an ASM in a calendar year in the data source

# Measure2: Annual early discontinuation rate of ASM during pregnancy (discontinuation during 2nd trimester)
# Numerator2: The number of pre-pregnancy users of an ASM within a calendar year that continued to 1st trimester only.
# Denominator2: Total number of pre-pregnancy users of an ASM in a calendar year in the data source

# Measure3: Annual late discontinuation rate of ASM during pregnancy (discontinuation during 3rd trimester)
# Numerator3: The number of pre-pregnancy users of an ASM within a calendar year that continued to 2nd trimester only.
# Denominator3: Total number of pre-pregnancy users of an ASM in a calendar year in the data source

# Stratification by: Individual drug substance, drug subgroups, calendar year, data source

# Conditions:
### Pre-pregnancy users
###############################################################################################################################################################################
print("======================================================================================================")
print("========================= CALCULATING DISCONTINUATION RATES DURING PREGNANCY =========================")
print("======================================================================================================")

# List overlaps
files_overlaps  <- list.files(file.path(paths$D3_dir, "tmp"), pattern = "\\.rds$", full.names = TRUE)

# List discontinued counts
files_counts       <- list.files(file.path(paths$D5_dir, "1.3_pre-pregnancy_use"))

# For each overlapping dataset, look for discontinued users:
# before - any12_6, any 6_0 should be true, any_t1, any_t2, any_t3 should be FALSE)
# early  - any12_6, any 6_0, any_t1 should be true, any_t2, any_t3 should be FALSE)
# late   - any12_6, any 6_0, any_t1, any_t2 should be true, any_t3 should be FALSE)
for (overlap in seq_along(files_overlaps)) {
  
  # Assign overlap name
  overlap_name <- prefix <- sub("\\.rds$", "", basename(files_overlaps[overlap]))
  
  # Find matching pre-pregnancy denominator file
  denom_file <- grep(paste0("^", overlap_name, "_pre_pregnancy_counts\\.rds$"), files_counts, value = TRUE)
  
  # Skip if no pre pregnancy users
  if (length(denom_file) != 1) next
  
  # Load denominator
  dt_counts <- readRDS(file.path(paths$D5_dir, "1.3_pre-pregnancy_use", denom_file))
  
  # Load episode 
  dt_overlap <- as.data.table(readRDS(files_overlaps[overlap]))
  
  # Get discontinuers
  if(deap_flags$is_EFEMERIS || deap_flags$is_FIN_REG) {
    dt_before <- dt_overlap[any_before==TRUE & any_t1 == FALSE & any_t2 == FALSE & any_t3 == FALSE,]
    dt_t1     <- dt_overlap[any_before==TRUE & any_t1 == TRUE & any_t2 == FALSE & any_t3 == FALSE,]
    dt_t2     <- dt_overlap[any_before==TRUE & any_t1 == TRUE & any_t2 == TRUE & any_t3 == FALSE,]
  } else {
    dt_before <- dt_overlap[any_12_6==TRUE & any_6_0 == TRUE & any_t1 == FALSE & any_t2 == FALSE & any_t3 == FALSE,]
    dt_t1     <- dt_overlap[any_12_6==TRUE & any_6_0 == TRUE & any_t1 == TRUE & any_t2 == FALSE & any_t3 == FALSE,]
    dt_t2     <- dt_overlap[any_12_6==TRUE & any_6_0 == TRUE & any_t1 == TRUE & any_t2 == TRUE & any_t3 == FALSE,]
  }
  
  # Create list of subsets
  discont_list <- list(before = dt_before, t1 = dt_t1, t2 = dt_t2)
  
  # Keep only the non-empty ones
  discont_list <- discont_list[sapply(discont_list, nrow) > 0]
  
  if (length(discont_list) == 0) {
    
    # Print message
    message("Skipping ", overlap_name, ": no discontinuers found in any period")
    
  } else {
    
    for (dt in seq_along(discont_list)) {
      
      # Load subset
      dt_subset <- discont_list[[dt]]
      
      # Print message
      message(sprintf("Processing %s - %s", overlap_name, names(discont_list)[dt]))
      
      # Keep one person per year
      dt_subset_unique <- unique(dt_subset, by = c("pregnancy_id", "preg_year"))
      
      # Count by pregnancy
      discontinuer_counts <- dt_subset_unique[, .(N = .N), by = preg_year]
      
      # Prepare denominator
      dt_counts_copy <- copy(dt_counts)
      dt_counts_copy <- dt_counts_copy[, .(preg_year, n_treated)]
      setnames(dt_counts_copy, "n_treated", "n_total")
      
      # Merge numerator and denominator
      discontinued_all <- merge(discontinuer_counts, dt_counts_copy, by = "preg_year", all.y = TRUE)
      discontinued_all[is.na(N), N := 0]
      
      # Calculate rate
      discontinued_all[, rate := round(100 * N / n_total, 3)]
      discontinued_all[N == 0 & n_total == 0, rate := 0]
      
      # Warnings
      if (nrow(discontinued_all[N > n_total]) > 0) warning(red("Warning: Numerator > Denominator"))
      if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator"))
      
      # Save odd cases
      if (nrow(discontinued_all[N > n_total]) > 0) fwrite(discontinued_all[N > n_total], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(overlap_name, "_", names(discont_list)[dt], "_num_gt_denominator.csv")))
      if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) fwrite(discontinued_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(overlap_name, "_", names(discont_list)[dt], "_denominator_zero_numerator_nonzero.csv")))
      
      # Add rate computable column
      discontinued_all[, rate_computable := n_total > 0]
      
      # Rename columns
      setnames(discontinued_all, "N", "n_treated")
      
      # Save output
      saveRDS(dt_subset, file.path(paths$D4_dir, "1.4_pregnancy_discontinuation", paste0(overlap_name, "_", names(discont_list)[dt], "_discontinuation_in_pregnancies_data.rds")))
      saveRDS(discontinued_all, file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(overlap_name, "_", names(discont_list)[dt], "_discontinuation_in_pregnancies_counts.rds")))
    }
  }
}

# Clean out tmp folder
if (length(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE)) > 0) unlink(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE))
