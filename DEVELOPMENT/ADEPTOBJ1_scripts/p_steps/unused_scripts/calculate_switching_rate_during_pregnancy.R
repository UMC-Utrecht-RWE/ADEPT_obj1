###############################################################################################################################################################################
# <<< Sub-objective 1.4: Switching rate during pregnancy >>> 
# Measure: Switching rate from one ASM to a different ASM or to an alternative medication during pregnancy
# Numerator: The number of pre-pregnancy users of an ASM within a calendar year that switched to a different ASM or alternative medication during the pregnancy period
# Denominator: Total number of pre-pregnancy users of an ASM in a calendar year in the data source 
# Stratification by: Overall, individual drug substance, drug sub-groups, indication, calendar year, data source

# Pending: Individual drug substance, calendar year, data source

# Conditions: 
### Pre-pregnancy users
### 
###############################################################################################################################################################################
print("===============================================================================================")
print("========================= CALCULATING SWITCHING RATE DURING PREGNANCY =========================")
print("===============================================================================================")

# Read in Pre-pregnancy Data
# List all pre pregnancy data matching population prefix
files_prepregnancy <- list.files(file.path(paths$D4_dir, "1.3_pre-pregnancy_use_rate"), pattern = "_pre_pregnancy_data\\.rds$")

# Keep only files that match population prefix AND contain "_F_" (female patients)
files_prepregnancy <- files_prepregnancy[grepl(paste0("^", pop_prefix, "_"), files_prepregnancy)]

# Drop PC_HOSP files if pop_prefix is PC
if(pop_prefix == "PC") files_prepregnancy <- files_prepregnancy[!grepl("PC_HOSP", files_prepregnancy)]

# Read in switcher Episodes
# switcher Episodes
files_switcher_episodes <- list.files(file.path(paths$D4_dir, "1.2_switching"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_switcher_episodes <- files_switcher_episodes[grepl(paste0("^", pop_prefix, "_"), files_switcher_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC") files_switcher_episodes <- files_switcher_episodes[!grepl("PC_HOSP", files_switcher_episodes)]

# Read in Pre-pregnancy Counts 
# List count files files matching population prefix
files_counts <- list.files(file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate"), pattern = "_pre_pregnancy_counts\\.rds$")

# Keep only files that match population prefix AND contain "_F_" (female patients)
files_counts <- files_counts[grepl(paste0("^", pop_prefix, "_"), files_counts)]

# Drop PC_HOSP files if pop_prefix is PC
if(pop_prefix == "PC") files_counts <- files_counts[!grepl("PC_HOSP", files_counts)]

# === Create maps ===
# Extract treatment name key
get_treatment_key <- function(x, suffix) gsub(suffix, "", x)

treatment_keys <- get_treatment_key(files_prepregnancy, "_pre_pregnancy_data.rds")

# Match corresponding files by treatment key
prepreg_map <- setNames(file.path(paths$D4_dir, "1.3_pre-pregnancy_use_rate", files_prepregnancy), treatment_keys)
switcher_map <- setNames(file.path(paths$D4_dir, "1.2_switching", files_switcher_episodes), treatment_keys)
counts_map  <- setNames(file.path(paths$D5_dir, "1.3_pre-pregnancy_use_rate", files_counts), treatment_keys)

for (trt in seq_along(treatment_keys)) {
  
  # get treatment name 
  treatment <- treatment_keys[trt]
  
  # If none of the file found, skip
  if (!file.exists(prepreg_map[[trt]]) ||
      !file.exists(switcher_map[[trt]]) ||
      !file.exists(counts_map[[trt]])) next
  
  # Read in files
  dt_pre <- readRDS(prepreg_map[[trt]])
  dt_switch <- readRDS(switcher_map[[trt]])
  dt_cnt <- readRDS(counts_map[[trt]])
  print(treatment)
  print(nrow(dt_pre))
  print(nrow(dt_switch))
  # Merge on person_id
  dt <- merge(dt_pre[,.(person_id, pregnancy_start_date, pregnancy_end_date)], dt_switch, by = "person_id", all = FALSE)
  print(nrow(dt))
  # Convert dates to IDate
  date_cols <- c("pregnancy_start_date", "pregnancy_end_date", "rx_date")
  dt[, (date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]
  
  ######################################################################
  ######################################################################
  ######################################################################
  # TEST 
  # Make sure dt is a data.table and person_id is character
  dt[person_id == "ConCDM_SIM_200421_00025", 
     `:=` (
       rx_date = as.IDate("2009-06-04")
     )]
  
  ######################################################################
  ######################################################################
  ######################################################################
  
  # Create subsets 
  # Number of pre-pregnancy users of an ASM that does not run into the pregnancy period
  dt <- dt[rx_date >= pregnancy_start_date & rx_date <= pregnancy_end_date,]
  
  
  if(nrow(dt)>0){
    
    message("Switchers in pregnancy is found for " , treatment)
    
    # Assign calendar year of each pregnancy
    dt[, preg_year := year(pregnancy_start_date)]
    
    # Remove duplicates: Keep only one person id per year
    dt <- unique(dt, by = c("person_id", "year"))
    
    # Count number of switchers per year
    switcher_counts <- dt[, .("N" = .N), by = preg_year]
    
    # Prepare denominator
    dt_cnt[,c("n_total", "rate", "rate_computable") := NULL]
    setnames(dt_cnt, "n_treated", "n_total")
    
    # Merge switcher with pre-pregnancies
    switcher_all <- merge(switcher_counts, dt_cnt, by = "preg_year", all.y = TRUE)
    
    # Set N = 0 for years with no treatments
    switcher_all[is.na(N), N := 0]
    
    # Calculate switcher as a rate (*100)
    switcher_all[, rate := round(100 * N / n_total, 3)][N == 0 & n_total == 0, rate := 0]
    
    # Set warnings if Numerator > than Denominator or if Denominator is 0 and Numerator is >0
    if (nrow(switcher_all[N > n_total]) > 0) {warning(red("Warning: Some numerator values exceed denominator."))}
    if (nrow(switcher_all[n_total == 0 & N != 0]) > 0) {warning(red("Warning: Denominator zero with non-zero numerator."))}
    
    # Save data where odd values 
    if(nrow(switcher_all[N > n_total])>0) fwrite(switcher_all[N > n_total], file.path(paths$D5_dir, "1.4_switcher_use_rate", paste0(current_prefix, "_num_gt_denominator.csv")))
    if(nrow(switcher_all[n_total == 0 & N != 0])>0) fwrite(switcher_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.4_switcher_use_rate", paste0(current_prefix, "_denominator_zero_numerator_nonzero.csv")))
    
    # Create column marking if rate is computable 
    switcher_all[, rate_computable := n_total > 0]
    
    # rename columns
    setnames(switcher_all, "N", "n_treated")
    
    # Save dataset 
    saveRDS(dt_before, file.path(paths$D4_dir, "1.4_switcher_use_rate", paste0(unique_prefixes[pfx], "_switcher_in_pregnancy_data.rds")))
    
    # Save results 
    saveRDS(switcher_all, file.path(paths$D5_dir, "1.4_switcher_use_rate", paste0(unique_prefixes[pfx], "_switcher_in_pregnancy_counts.rds")))
    
  } else {
    
    message(red("No switchers before pregnancy for" , treatment))
  }
  
}

