###############################################################################################################################################################################
# <<< Sub-objective 1.4: Switching rate during pregnancy >>> 
# Measure: Switching rate from one ASM to a different ASM or to an alternative medication during pregnancy
# Numerator: The number of pre-pregnancy users of an ASM within a calendar year that switched to a different ASM or alternative medication during the pregnancy period
# Denominator: Total number of pre-pregnancy users of an ASM in a calendar year in the data source 
# Stratification by: Overall, individual drug substance, drug sub-groups, indication, calendar year, data source

# Conditions: 
### Pre-pregnancy users
### 
###############################################################################################################################################################################
print("================================================================================================")
print("========================= CALCULATING SWITCHING RATES DURING PREGNANCY =========================")
print("================================================================================================")

# List files 
# Pre-pregnancy data and counts 
files_prepregnancy <- list.files(file.path(paths$D4_dir, "1.3_pre-pregnancy_use"))
files_counts       <- list.files(file.path(paths$D5_dir, "1.3_pre-pregnancy_use"))
# Switcher episodes 
files_switcher_episodes <- list.files(file.path(paths$D4_dir, "1.2_switching"), pattern = "\\.rds$")

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  files_switcher_episodes <- files_switcher_episodes[grepl("_F_", files_switcher_episodes)] # Female pop
  if(pop_prefix == "PC") files_prepregnancy  <- files_prepregnancy[!grepl("PC_HOSP", files_prepregnancy)] # BIFAP
  if(pop_prefix == "PC") files_counts        <- files_counts[!grepl("PC_HOSP", files_counts)] #BIFAP
  if(pop_prefix=="PC") files_switcher_episodes <- files_switcher_episodes[!grepl("PC_HOSP", files_switcher_episodes)] #BIFAP
}

# Create Maps 
# Set Function
get_treatment_key <- function(x, suffix) gsub(suffix, "", x)

# Prepregnancy keys 
prepreg_keys <- get_treatment_key(files_prepregnancy, "_pre_pregnancy_data.rds")
prepreg_map  <- setNames(file.path(paths$D4_dir, "1.3_pre-pregnancy_use", files_prepregnancy), prepreg_keys)

# Counts keys
counts_keys <- get_treatment_key(files_counts, "_pre_pregnancy_counts.rds")
counts_map  <- setNames(file.path(paths$D5_dir, "1.3_pre-pregnancy_use", files_counts), counts_keys)

# Discontinued keys
switcher_keys <- get_treatment_key(files_switcher_episodes, "_switcher_data.rds")
switcher_map  <- setNames(file.path(paths$D4_dir, "1.2_switching", files_switcher_episodes), switcher_keys)

# 4. Keep only keys that exist in all three
common_keys  <- Reduce(intersect, list(prepreg_keys, counts_keys, switcher_keys))
prepreg_map  <- prepreg_map[common_keys]
counts_map   <- counts_map[common_keys]
switcher_map <- switcher_map[common_keys]


for (trt in seq_along(common_keys)) {
  
  # get treatment name 
  treatment <- common_keys[trt]
  
  # If none of the file found, skip
  if (!file.exists(prepreg_map[[trt]]) ||
      !file.exists(counts_map[[trt]]) ||
      !file.exists(switcher_map[[trt]])) next
  
  # read in the files
  dt_prepreg <- readRDS(prepreg_map[[trt]])
  dt_counts  <- readRDS(counts_map[[trt]])
  dt_switch  <- readRDS(switcher_map[[trt]])
  
  # merge prepregnancy data with discontinuation file
  if(deap_flags$is_EFEMERIS || deap_flags$is_FIN_REG) {
    dt <- merge(dt_prepreg[,.(pregnancy_id)], dt_switch, by = "pregnancy_id", all = FALSE)
  } else {
    dt <- merge(dt_prepreg[,.(person_id, pregnancy_start_date, pregnancy_end_date)], dt_switch, by = "person_id", all = FALSE)
  }
  
  if(nrow(dt)==0) {
    message(red("No switcher records found in pre-pregnancy users for", treatment))
    next
  }
  
  # print message
  message(blue("Switcher records found in pre-pregnancy users for", treatment))
  
  # convert dates to IDate
  date_cols <- c("pregnancy_start_date", "pregnancy_end_date", "rx_date")
  dt[, (date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]

  # create subset 
  # pre-pregnancy users switched to a different ASM or alternative medication during the pregnancy period
  # rx_date is the switch date
  dt_subset <- dt[rx_date >= pregnancy_start_date & rx_date <= pregnancy_end_date,]

  # check if any rows 
  if (nrow(dt_subset) == 0) {
    message("Skipping ", treatment, ": no switchers found during pregnancy")
    next
  } 

  # print message
  message("Processing " , treatment)

  # assign year to count in 
  dt_subset[, preg_year := year(pregnancy_start_date)]

  # keep one person/pregnancy per year
  if (deap_flags$is_EFEMERIS || deap_flags$is_FIN_REG) {
    dt_subset <- unique(dt_subset, by = c("pregnancy_id", "preg_year"))
  } else {
    dt_subset <- unique(dt_subset, by = c("person_id", "preg_year"))
  } 
  
  # count by pregnancy
  switcher_counts <- dt_subset[, .(N = .N), by = preg_year]
    
  # prepare denominator
  dt_counts_copy <- copy(dt_counts)
  dt_counts_copy[, c("n_total", "rate", "rate_computable") := NULL]
  setnames(dt_counts_copy, "n_treated", "n_total")
    
  # merge numerator and denominator
  switcher_all <- merge(switcher_counts, dt_counts_copy, by = "preg_year", all.y = TRUE)
  switcher_all[is.na(N), N := 0]
  
  # Calculate switcher as a rate (*1000)
  switcher_all[, rate := round(1000 * N / n_total, 3)]
  switcher_all[N == 0 & n_total == 0, rate := 0]
    
  # warnings
  if (nrow(switcher_all[N > n_total]) > 0) warning(red("Warning: Numerator > Denominator"))
  if (nrow(switcher_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator"))
  
  # save odd cases
  if (nrow(switcher_all[N > n_total]) > 0) fwrite(switcher_all[N > n_total], file.path(paths$D5_dir, "1.4_pregnancy_switching", paste0(treatment, "_num_gt_denominator.csv")))
  if (nrow(switcher_all[n_total == 0 & N != 0]) > 0) fwrite(switcher_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.4_pregnancy_switching", paste0(treatment, "_denominator_zero_numerator_nonzero.csv")))
  
  # Create column marking if rate is computable 
  switcher_all[, rate_computable := n_total > 0]
  
  # rename columns
  setnames(switcher_all, "N", "n_treated")
  
  # save output
  saveRDS(dt_subset, file.path(paths$D4_dir, "1.4_pregnancy_switching", paste0(treatment, "_switching_in_pregnancies_data.rds")))
  saveRDS(switcher_all, file.path(paths$D5_dir, "1.4_pregnancy_switching", paste0(treatment, "_switching_in_pregnancies_counts.rds")))
}




















