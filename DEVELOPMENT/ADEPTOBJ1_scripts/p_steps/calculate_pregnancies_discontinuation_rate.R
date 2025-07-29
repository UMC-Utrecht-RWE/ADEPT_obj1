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

# Stratification by: Overall, individual drug substance, drug subgroups, calendar year, data source

###############################################################################################################################################################################
print("======================================================================================================")
print("========================= CALCULATING DISCONTINUATION RATES DURING PREGNANCY =========================")
print("======================================================================================================")

# Read in Pre-pregnancy Data
# List all pre pregnancy data matching population prefix
files_prepregnancy <- list.files(file.path(paths$D4_dir, "1.3_pre-pregnancy_use"), pattern = "_pre_pregnancy_data\\.rds$")

# Drop PC_HOSP files if pop_prefix is PC
if(pop_prefix == "PC") files_prepregnancy <- files_prepregnancy[!grepl("PC_HOSP", files_prepregnancy)]

# Read in Discontinued Episodes
# Discontinued Episodes
files_discontinued_episodes <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), pattern = "\\.rds$")

# Filter exposures for Females only
files_discontinued_episodes <- files_discontinued_episodes[grepl("_F_", files_discontinued_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_discontinued_episodes <- files_discontinued_episodes[!grepl("PC_HOSP", files_discontinued_episodes)]

# Read in Pre-pregnancy Counts 
# List count files files matching population prefix
files_counts <- list.files(file.path(paths$D5_dir, "1.3_pre-pregnancy_use"), pattern = "_pre_pregnancy_counts\\.rds$")

# Drop PC_HOSP files if pop_prefix is PC
if(pop_prefix == "PC") files_counts <- files_counts[!grepl("PC_HOSP", files_counts)]

# === Create maps ===
# Extract treatment name key
get_treatment_key <- function(x, suffix) gsub(suffix, "", x)

treatment_keys <- get_treatment_key(files_prepregnancy, "_pre_pregnancy_data.rds")

# Match corresponding files by treatment key
prepreg_map <- setNames(file.path(paths$D4_dir, "1.3_pre-pregnancy_use", files_prepregnancy), treatment_keys)
discont_map <- setNames(file.path(paths$D4_dir, "1.2_discontinued", files_discontinued_episodes), treatment_keys)
counts_map  <- setNames(file.path(paths$D5_dir, "1.3_pre-pregnancy_use", files_counts), treatment_keys)

for (trt in seq_along(treatment_keys)) {

  # get treatment name 
  treatment <- treatment_keys[trt]
  
  # If none of the file found, skip
  if (!file.exists(prepreg_map[[trt]]) ||
      !file.exists(discont_map[[trt]]) ||
      !file.exists(counts_map[[trt]])) next

  # Read in files
  dt_pre <- readRDS(prepreg_map[[trt]])
  dt_dis <- readRDS(discont_map[[trt]])
  dt_cnt <- readRDS(counts_map[[trt]])
  
  # Merge on person_id
  dt <- merge(dt_pre[,.(person_id, pregnancy_start_date, pregnancy_end_date)], dt_dis, by = "person_id", all = FALSE)
  
  # Convert dates to IDate
  date_cols <- c("pregnancy_start_date", "pregnancy_end_date", "episode.start", "episode.end")
  dt[, (date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]
  
  # Add trimester windows
  dt[, t1_start := pregnancy_start_date][, t1_end := pregnancy_start_date + 90]
  dt[, t2_start := pregnancy_start_date + 91][, t2_end := pregnancy_start_date + 180]
  dt[, t3_start := pregnancy_start_date + 181][, t3_end := pregnancy_end_date]
  
  # Create subsets 
  # Number of pre-pregnancy users of an ASM that does not run into the pregnancy period
  dt_before <- dt[episode.end < pregnancy_start_date,]
  # Number of pre-pregnancy users of an ASM that continued to 1st trimester only
  dt_t1 <- dt[episode.end < t2_start & episode.end >= pregnancy_start_date,]
  # Number of pre-pregnancy users of an ASM that continued to 2nd trimester only. 
  dt_t2 <- dt[episode.end < t3_start & episode.end >= t2_start,]
  
  if(nrow(dt_before)>0){
    
    message("Discontinuers before pregnancy found for " , treatment)
    
    # Assign calendar year of each pregnancy
    dt_before[, preg_year := year(pregnancy_start_date)]
    
    # Remove duplicates: Keep only one person id per year
    dt_before <- unique(dt_before, by = c("person_id", "preg_year"))
    
    # Count number of discontinuers per year
    discontinuer_counts <- dt_before[, .("N" = .N), by = preg_year]
    
    # Prepare denominator
    dt_cnt[,c("n_total", "rate", "rate_computable") := NULL]
    setnames(dt_cnt, "n_treated", "n_total")
    
    # Merge discontinued with pre-pregnancies
    discontinued_all <- merge(discontinuer_counts, dt_cnt, by = "preg_year", all.y = TRUE)
    
    # Set N = 0 for years with no treatments
    discontinued_all[is.na(N), N := 0]
    
    # Calculate discontinued as a rate (*100)
    discontinued_all[, rate := round(100 * N / n_total, 3)][N == 0 & n_total == 0, rate := 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(discontinued_all[N > n_total]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
    if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
    
    # Save data where odd values 
    if(nrow(discontinued_all[N > n_total])>0) fwrite(discontinued_all[N > n_total], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_num_gt_denominator.csv")))
    if(nrow(discontinued_all[n_total == 0 & N != 0])>0) fwrite(discontinued_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_denominator_zero_numerator_nonzero.csv")))
    
    # Create column marking if rate is computable 
    discontinued_all[, rate_computable := n_total > 0]
    
    # rename columns
    setnames(discontinued_all, "N", "n_treated")
    
    # Save dataset 
    saveRDS(dt_before, file.path(paths$D4_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_discontinued_before_pregnancy_data.rds")))
    
    # Save results 
    saveRDS(discontinued_all, file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_discontinued_before_pregnancy_counts.rds")))
    
  } else {
    
    message(red("No discontinuers before pregnancy for" , treatment))
  }
  
  if(nrow(dt_t1)>0){
    
    message("Discontinuers in t1 found for " , treatment)
    
    # Assign calendar year of each pregnancy
    dt_t1[, preg_year := year(pregnancy_start_date)]
    
    # Remove duplicates: Keep only one person id per year
    dt_t1 <- unique(dt_t1, by = c("person_id", "preg_year"))
    
    # Count number of discontinuers per year
    discontinuer_counts <- dt_t1[, .("N" = .N), by = preg_year]
    
    # Prepare denominator
    dt_cnt[,c("n_total", "rate", "rate_computable") := NULL]
    setnames(dt_cnt, "n_treated", "n_total")
    
    # Merge discontinued with pre-pregnancies
    discontinued_all <- merge(discontinuer_counts, dt_cnt, by = "preg_year", all.y = TRUE)
    
    # Set N = 0 for years with no treatments
    discontinued_all[is.na(N), N := 0]
    
    # Calculate discontinued as a rate (*100)
    discontinued_all[, rate := round(100 * N / n_total, 3)][N == 0 & n_total == 0, rate := 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(discontinued_all[N > n_total]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
    if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
    
    # Save data where odd values 
    if(nrow(discontinued_all[N > n_total])>0) fwrite(discontinued_all[N > n_total], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_num_gt_denominator.csv")))
    if(nrow(discontinued_all[n_total == 0 & N != 0])>0) fwrite(discontinued_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_denominator_zero_numerator_nonzero.csv")))
    
    # Create column marking if rate is computable 
    discontinued_all[, rate_computable := n_total > 0]
    
    # rename columns
    setnames(discontinued_all, "N", "n_treated")
    
    # Save dataset 
    saveRDS(dt_t1, file.path(paths$D4_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_discontinued_during_t1_data.rds")))
    
    # Save results 
    saveRDS(discontinued_all, file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_discontinued_during_t1_counts.rds")))
    
  } else {
    
    message(red("No discontinuers in t1 for ", treatment))
  }
  
  if(nrow(dt_t2)>0){
    
    message("Discontinuers in t2 found for" , treatment)
    
    # Assign calendar year of each pregnancy
    dt_t2[, preg_year := year(pregnancy_start_date)]
    
    # Remove duplicates: Keep only one person id per year
    dt_t2 <- unique(dt_t2, by = c("person_id", "preg_year"))
    
    # Count number of discontinuers per year
    discontinuer_counts <- dt_t2[, .("N" = .N), by = preg_year]
    
    # Prepare denominator
    dt_cnt[,c("n_total", "rate", "rate_computable") := NULL]
    setnames(dt_cnt, "n_treated", "n_total")
    
    # Merge discontinued with pre-pregnancies
    discontinued_all <- merge(discontinuer_counts, dt_cnt, by = "preg_year", all.y = TRUE)
    
    # Set N = 0 for years with no treatments
    discontinued_all[is.na(N), N := 0]
    
    # Calculate discontinued as a rate (*100)
    discontinued_all[, rate := round(100 * N / n_total, 3)][N == 0 & n_total == 0, rate := 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(discontinued_all[N > n_total]) > 0) warning(red("Warning: Some numerator values exceed denominator."))
    if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator."))
    
    # Save data where odd values 
    if(nrow(discontinued_all[N > n_total])>0) fwrite(discontinued_all[N > n_total], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_num_gt_denominator.csv")))
    if(nrow(discontinued_all[n_total == 0 & N != 0])>0) fwrite(discontinued_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_denominator_zero_numerator_nonzero.csv")))
    
    # Create column marking if rate is computable 
    discontinued_all[, rate_computable := n_total > 0]
    
    # rename columns
    setnames(discontinued_all, "N", "n_treated")
    
    # Save dataset 
    saveRDS(dt_t2, file.path(paths$D4_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_discontinued_during_t2_data.rds")))
    
    # Save results 
    saveRDS(discontinued_all, file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_discontinued_during_t2_counts.rds")))
    
  } else {
    
    message(red("No discontinuers in t2 for", treatment))
  }
}

