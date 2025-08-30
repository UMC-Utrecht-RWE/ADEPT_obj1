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

# List files 
# Pre-pregnancy data and counts 
files_prepregnancy <- list.files(file.path(paths$D4_dir, "1.3_pre-pregnancy_use"))
files_counts       <- list.files(file.path(paths$D5_dir, "1.3_pre-pregnancy_use"))

# filter for BIFAP subpops
if(pop_prefix == "PC") files_prepregnancy  <- files_prepregnancy[!grepl("PC_HOSP", files_prepregnancy)]
if(pop_prefix == "PC") files_counts <- files_counts[!grepl("PC_HOSP", files_counts)]

# Discontinued episodes 
files_discontinued_episodes <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), pattern = "\\.rds$")

if(!deap_flags$is_EFEMERIS){
  files_discontinued_episodes <- files_discontinued_episodes[grepl("_F_", files_discontinued_episodes)] # Female subpop
  if(pop_prefix=="PC") files_discontinued_episodes <- files_discontinued_episodes[!grepl("PC_HOSP", files_discontinued_episodes)] #BIFAP
}

# Create maps
# Set function 
get_treatment_key <- function(x, suffix) gsub(suffix, "", x)

# Prepreg keys 
prepreg_keys <- get_treatment_key(files_prepregnancy, "_pre_pregnancy_data.rds")
prepreg_map  <- setNames(file.path(paths$D4_dir, "1.3_pre-pregnancy_use", files_prepregnancy), prepreg_keys)

# Counts keys
counts_keys <- get_treatment_key(files_counts, "_pre_pregnancy_counts.rds")
counts_map  <- setNames(file.path(paths$D5_dir, "1.3_pre-pregnancy_use", files_counts), counts_keys)

# Discontinued keys
discont_keys <- get_treatment_key(files_discontinued_episodes, "_discontinued_data.rds")
discont_map  <- setNames(file.path(paths$D4_dir, "1.2_discontinued", files_discontinued_episodes), discont_keys)

# 4. Keep only keys that exist in all three
common_keys <- Reduce(intersect, list(prepreg_keys, counts_keys, discont_keys))
prepreg_map <- prepreg_map[common_keys]
counts_map  <- counts_map[common_keys]
discont_map <- discont_map[common_keys]


for (trt in seq_along(common_keys)) {
  
  # get treatment name 
  treatment <- common_keys[trt]
  
  # read in the files
  dt_prepreg <- readRDS(prepreg_map[[trt]])
  dt_counts  <- readRDS(counts_map[[trt]])
  dt_discont <- readRDS(discont_map[[trt]])
  
  # merge prepregnancy data with discontinuation file
  if (!deap_flags$is_EFEMERIS) dt <- merge(dt_prepreg[,.(person_id, pregnancy_start_date, pregnancy_end_date, episode.start, episode.end)], dt_discont, by = c("person_id", "episode.start", "episode.end"), all = FALSE)
  if (deap_flags$is_EFEMERIS)  dt <- merge(dt_prepreg[,.(person_id, pregnancy_id, episode.start, episode.end)], dt_discont, by = c("pregnancy_id", "episode.start", "episode.end"), all = FALSE)
  
  # Print message if no discontinuers found
  if (nrow(dt) == 0) {
    message(red("No discontinued records found in pre-pregnancy users for", treatment))
    next
  }
  
  # print message
  message(blue("Discontinued records found in pre-pregnancy users for", treatment))
  
  # convert dates to IDate
  date_cols <- c("pregnancy_start_date", "pregnancy_end_date", "episode.start", "episode.end")
  dt[, (date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]
  
  # add trimester windows
  dt[, t1_start := pregnancy_start_date]
  dt[, t1_end   := pmin(pregnancy_start_date + 90, pregnancy_end_date)]
  dt[, t2_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 91, pregnancy_start_date + 91, as.IDate(NA))]
  dt[, t2_end   := fifelse(!is.na(t2_start), pmin(pregnancy_start_date + 180, pregnancy_end_date), as.IDate(NA))]
  dt[, t3_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 181, pregnancy_start_date + 181, as.IDate(NA))]
  dt[, t3_end   := fifelse(!is.na(t3_start), pregnancy_end_date, as.IDate(NA))]
  
  # create subsets
  # Pre-pregnancy: episode ends before pregnancy starts
  dt_before <- dt[episode.end < pregnancy_start_date]
  # Trimester 1 discontinuation: only if T1 exists
  dt_t1 <- dt[!is.na(t1_start) & !is.na(t1_end) & episode.end >= t1_start & episode.end < t1_end]
  # Trimester 2 discontinuation: only if T2 exists
  dt_t2 <- dt[!is.na(t2_start) & !is.na(t2_end) & episode.end >= t2_start & episode.end < t2_end]
  
  
  # create list of subsets
  discont_list <- list(before = dt_before, t1 = dt_t1, t2 = dt_t2)
  
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
    if(!deap_flags$is_EFEMERIS) dt_subset <- unique(dt_subset, by = c("person_id", "preg_year"))
    if(deap_flags$is_EFEMERIS)  dt_subset <- unique(dt_subset, by = c("pregnancy_id", "preg_year"))
    
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
  
}
























