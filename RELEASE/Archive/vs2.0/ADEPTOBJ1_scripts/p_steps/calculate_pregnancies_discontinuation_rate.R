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

#=== List files ===
# Pre-pregnancy data and counts 
files_prepregnancy <- list.files(file.path(paths$D4_dir, "1.3_pre-pregnancy_use"))
files_counts       <- list.files(file.path(paths$D5_dir, "1.3_pre-pregnancy_use"))

# filter for BIFAP subpops
if(pop_prefix == "PC") files_prepregnancy  <- files_prepregnancy[!grepl("PC_HOSP", files_prepregnancy)]
if(pop_prefix == "PC") files_counts <- files_counts[!grepl("PC_HOSP", files_counts)]

# Discontinued episodes 
files_discontinued_episodes <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), pattern = "\\.rds$")
# filter for Female subpop only 
files_discontinued_episodes <- files_discontinued_episodes[grepl("_F_", files_discontinued_episodes)]
# filter for BIFAP subpops
if(pop_prefix=="PC") files_discontinued_episodes <- files_discontinued_episodes[!grepl("PC_HOSP", files_discontinued_episodes)]

# === Create maps ===
# extract treatment name key
get_treatment_key <- function(x, suffix) gsub(suffix, "", x)
treatment_keys <- get_treatment_key(files_prepregnancy, "_pre_pregnancy_data.rds")

# match corresponding files by treatment key
prepreg_map <- setNames(file.path(paths$D4_dir, "1.3_pre-pregnancy_use", files_prepregnancy), treatment_keys)
counts_map  <- setNames(file.path(paths$D5_dir, "1.3_pre-pregnancy_use", files_counts), treatment_keys)
discont_map <- setNames(file.path(paths$D4_dir, "1.2_discontinued", files_discontinued_episodes), treatment_keys)

for (trt in seq_along(treatment_keys)) {
  
  # get treatment name 
  treatment <- treatment_keys[trt]
  
  # If none of the file found, skip
  if (!file.exists(prepreg_map[[trt]]) ||
      !file.exists(counts_map[[trt]]) ||
      !file.exists(discont_map[[trt]])) next
  
  # read in the files
  dt_prepreg <- readRDS(prepreg_map[[trt]])
  dt_counts  <- readRDS(counts_map[[trt]])
  dt_discont <- readRDS(discont_map[[trt]])
  
  # merge prepregnancy data with discontinuation file
  dt <- merge(dt_prepreg[,.(person_id, pregnancy_start_date, pregnancy_end_date)], dt_discont, by = "person_id", all = FALSE)
  
  if(nrow(dt)>0){
    # print message
    message(blue("Discontinued records found in pre-pregnancy users for", treatment))
    
    # convert dates to IDate
    date_cols <- c("pregnancy_start_date", "pregnancy_end_date", "episode.start", "episode.end")
    dt[, (date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]
    
    # add trimester windows
    dt[, t1_start := pregnancy_start_date][, t1_end := pregnancy_start_date + 90]
    dt[, t2_start := pregnancy_start_date + 91][, t2_end := pregnancy_start_date + 180]
    dt[, t3_start := pregnancy_start_date + 181][, t3_end := pregnancy_end_date]
    
    # create subsets 
    # pre-pregnancy users whose ASM use does not run into the pregnancy period
    dt_before <- dt[episode.end < pregnancy_start_date,]
    # discontinuation during 2nd trimester
    dt_t2 <- dt[episode.end >= t2_start & episode.end < t2_end,]
    # discontinuation during 3rd trimester
    dt_t3 <- dt[episode.end >= t3_start & episode.end < t3_end,]
    
    # create list of subsets
    discont_list <- list(before = dt_before, t2 = dt_t2, t3 = dt_t3)
    
    for (dt in seq_along(discont_list)) {
  
      # load subset 
      dt_subset <- discont_list[[dt]]
      
      # check if any rows 
      if (nrow(dt_subset) == 0) {
        message("Skipping ", treatment, " - ", names(discont_list)[dt], ": no discontinuers found in this period")
        next
      }
      
      # print message
      message(sprintf("Processing %s - %s", treatment, names(discont_list)[dt]))
      
      # assign year to count in 
      dt_subset[, preg_year := year(pregnancy_start_date)]
      
      # keep one person per year
      dt_subset <- unique(dt_subset, by = c("person_id", "preg_year"))
      
      # count by pregnancy
      discontinuer_counts <- dt_subset[, .(N = .N), by = preg_year]
      
      # prepare denominator
      dt_counts_copy <- copy(dt_counts)
      dt_counts_copy[, c("n_total", "rate", "rate_computable") := NULL]
      setnames(dt_counts_copy, "n_treated", "n_total")
      
      # merge numerator and denominator
      discontinued_all <- merge(discontinuer_counts, dt_counts_copy, by = "preg_year", all.y = TRUE)
      discontinued_all[is.na(N), N := 0]
      
      # calculate rate
      discontinued_all[, rate := round(1000 * N / n_total, 3)]
      discontinued_all[N == 0 & n_total == 0, rate := 0]
      
      # warnings
      if (nrow(discontinued_all[N > n_total]) > 0) warning(red("Warning: Numerator > Denominator"))
      if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) warning(red("Warning: Denominator zero with non-zero numerator"))
      
      # save odd cases
      if (nrow(discontinued_all[N > n_total]) > 0) fwrite(discontinued_all[N > n_total], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_", names(discont_list)[dt], "_num_gt_denominator.csv")))
      if (nrow(discontinued_all[n_total == 0 & N != 0]) > 0) fwrite(discontinued_all[n_total == 0 & N != 0], file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_", names(discont_list)[dt], "_denominator_zero_numerator_nonzero.csv")))
      
      # add rate computable column
      discontinued_all[, rate_computable := n_total > 0]
      # rename columns 
      setnames(discontinued_all, "N", "n_treated")
      
      # save output
      saveRDS(dt_subset, file.path(paths$D4_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_", names(discont_list)[dt], "_discontinuation_in_pregnancies_data.rds")))
      saveRDS(discontinued_all, file.path(paths$D5_dir, "1.4_pregnancy_discontinuation", paste0(treatment, "_", names(discont_list)[dt], "_discontinuation_in_pregnancies_counts.rds")))
      
    }
  } else {
    
    message(red("No discontinued records found in pre-pregnancy users for", treatment))

  }
}
























