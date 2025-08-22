###############################################################################################################################################################################
# <<< Sub-objective 1.5: Annual mean weighted daily dose of ASM - 3-month before pregnancy >>> 
# Measure: Annual proportion of pregnancies with low, middle, and high DDD of ASM during pre-pregnancy
# Numerator: Total number of pregnant women who are continuous users of ASM during pregnancy or late discontinuers of ASM 
#            during pregnancy in a calendar year in the data source which have a mean daily dose value 
#            of <0.5 (low), 0.5-1.49 (middle), and >=1.5 DDD (high) during pre-pregnancy
# Denominator: Total number of ASM users during pregnancy (i.e., continuous users, and late discontinuers) in that calendar year in the data source
# Stratification by: Individual drug substance, calendar year, data source


# <<< Sub-objective 1.5: Mean weighted daily dose of ASM -  1st trimester >>> 
# Measure: Annual proportion of pregnancies with low, middle, and high DDD of ASM during 1st trimester
# Numerator: Total number of pregnant women who are either continuous users of ASM during pregnancy or late discontinuers of ASM 
#            during pregnancy in a calendar year in the data source which have a mean daily dose value 
#            of <0.5 (low), 0.5-1.49 (middle), and >= 1.5 DDD (high) during the 1st trimester
# Denominator: Total number of ASM users during pregnancy (i.e., continuous users, and late discontinuers) in that calendar year in the data source
# Stratification by: Individual drug substance, calendar year, data source


# <<< Sub-objective 1.5: Mean weighted daily dose of ASM - 2nd trimester >>> 
# Measure: Annual proportion of pregnancies with low, middle, and high DDD of ASM during 1st trimester
# Numerator: Total number of pregnant women who are either continuous users of ASM during pregnancy or late discontinuers of ASM 
#            during pregnancy in a calendar year in the data source which have a mean daily dose value 
#            of <0.5 (low), 0.5-1.49 (middle), and >= 1.5 DDD (high) during the 2nd trimester
# Denominator: Total number of ASM users during pregnancy (i.e., continuous users, and late discontinuers) in that calendar year in the data source
# Stratification by: Individual drug substance, calendar year, data source


# <<< Sub-objective 1.5: Mean weighted daily dose of ASM - 3rd trimester >>> 
# Measure: Annual proportion of pregnancies with low, middle, and high DDD of ASM during 1st trimester
# Numerator: Total number of pregnant women who are either continuous users of ASM during pregnancy or late discontinuers of ASM 
#            during pregnancy in a calendar year in the data source which have a mean daily dose value 
#            of <0.5 (low), 0.5-1.49 (middle), and >=1.5 DDD (high) during the 3rd trimester
# Denominator: Total number of ASM users during pregnancy (i.e., continuous users, and late discontinuers) in that calendar year in the data source
# Stratification by: Individual drug substance, calendar year, data source

###############################################################################################################################################################################
# List subgroups to exclude
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

###################################################################################################################################
# Get lists of all needed files and load them 
# pre-pregnancy continuous users
continuous_files <- list.files(file.path(paths$D4_dir, "1.3_pregnancy_continuous"), full.names = TRUE) 
continuous_files <- continuous_files[grepl(paste0("^", pop_prefix, "_"), basename(continuous_files))]# keep current pop prefix
continuous_files <- continuous_files[!grepl(paste(exclude, collapse = "|"), basename(continuous_files))] # exclude groups
dt_continuous   <- rbindlist(lapply(continuous_files, readRDS), use.names = TRUE, fill = TRUE) # read in and bind all files 
# pre-pregnancy discontinuers
discontinued_files <- list.files(file.path(paths$D4_dir, "1.4_pregnancy_discontinuation"), full.names = TRUE)
discontinued_files <- discontinued_files[grepl(paste0("^", pop_prefix, "_"), basename(discontinued_files))] # keep current pop prefix
discontinued_files <- discontinued_files[grepl("t2", basename(discontinued_files), ignore.case = TRUE)] # keep only late discontinuers
discontinued_files <- discontinued_files[!grepl(paste(exclude, collapse = "|"), basename(discontinued_files))] # exclude groups
dt_discontinued    <- rbindlist(lapply(discontinued_files, readRDS), use.names = TRUE, fill = TRUE) # read in and bind all files 
# exposure files 
exposure_files <- list.files(file.path(paths$D3_dir, "exposure"))
# products table 
dt_products <- rbindlist(lapply(list.files(file.path(CDM_dir), pattern = "^PRODUCTS", full.names = TRUE), fread), use.names = TRUE, fill = TRUE)
dt_products <- unique(dt_products, by = "medicinal_product_id")
####################################################################################################################################
# Prepare for calculations
cols_needed <- c("person_id", "episode.start", "episode.end", "atc_group", "code", "pregnancy_start_date", "pregnancy_end_date")
if (nrow(dt_continuous) > 0 & nrow(dt_discontinued) > 0) dt_all <- rbindlist(list(dt_continuous[, ..cols_needed], dt_discontinued[, ..cols_needed]), use.names = TRUE, fill = TRUE)
if (nrow(dt_continuous) > 0 & nrow(dt_discontinued) == 0) dt_all <- copy(dt_continuous[, ..cols_needed])
if (nrow(dt_continuous) == 0 & nrow(dt_discontinued) > 0) dt_all <- copy(dt_discontinued[, ..cols_needed])
if (nrow(dt_continuous) == 0 & nrow(dt_discontinued) == 0)  message("No continuous or late discontinuer users were found")

if(nrow(dt_all)>0){

  # set intervals for overlapping in both dt_all and dt_exp
  dt_all[,window_start:= as.IDate(episode.start)][,window_end := as.IDate(episode.end)][,preg_start_year:=year(pregnancy_start_date)]
  
  # reorder columns in dt_all so first three are person_id, window_start, window_end - needed for foroverlaps
  setcolorder(dt_all, c("person_id", "window_start", "window_end", setdiff(names(dt_all), c("person_id", "window_start", "window_end"))))
  
  # set keys on both tables by the interval columns
  setkey(dt_all, person_id, window_start, window_end)
  
  # merge dt_all with matching exposure file to get cols for calculations  
  merged_list <- list()
  
  for (atc in unique(dt_all$atc_group)) {
    # find matching exposure file
    file_match <- exposure_files[grepl(paste0("^", pop_prefix, "_F_", atc, "\\.rds$"), exposure_files)]
    
    # read in matching exposure file
    dt_exp <- unique(readRDS(file.path(paths$D3_dir, "exposure", file_match)))
    
    # set intervals for overlapping in both dt_all and dt_exp
    dt_exp[,event_start:=as.IDate(rx_date)][,event_end:=as.IDate(rx_date)]
    
    # set keys on both tables by the interval columns
    setkey(dt_exp, person_id, event_start, event_end)
    
    # merge data sets to get rx's that make up treatment episode
    merged <- foverlaps(dt_exp[,.(person_id, event_start, event_end, rx_date, medicinal_product_id, disp_number_medicinal_product, presc_quantity_per_day, assumed_duration)], 
                        dt_all, 
                        by.x = c("person_id", "event_start", "event_end"), 
                        by.y = c("person_id", "window_start", "window_end"), 
                        nomatch = 0)  # only keep overlaps
    
    merged_list[[atc]] <- merged
  }
  
  
  # combine all merged ATC groups
  dt <- rbindlist(merged_list, use.names = TRUE, fill = TRUE)
  
  # drop cols you dont need
  dt[, c("window_start", "window_end", "event_start", "event_end") := NULL]
  
  # keep unique rows 
  dt <- unique(dt, by = c("person_id", "pregnancy_start_date", "pregnancy_end_date", "atc_group", "rx_date")) 
  
  # set as datatable
  setDT(dt)
  setDT(dt_products)
  
  # merge with products table - inner join 
  dt <- merge(dt, dt_products, by = "medicinal_product_id")
  
  # order by person_id, atc_group and rx date 
  setorder(dt, person_id, atc_group, rx_date)
  # create intervals for the rx: rx_start to 1 day before the next rx_start. If last rx_start, then the end is the episode end
  dt[, rx_start := rx_date][, rx_end := shift(rx_date, type = "lead") - 1, by = .(person_id, atc_group)][is.na(rx_end), rx_end := episode.end]
  
  # Add pre-pregnancy and trimester windows cols
  dt[, t0_start := pregnancy_start_date - 90][, t0_end := pregnancy_start_date - 1]
  dt[, t1_start := pregnancy_start_date][, t1_end := pmin(pregnancy_start_date + 90, pregnancy_end_date)]
  dt[, t2_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 91, pregnancy_start_date + 91, as.IDate(NA))][, t2_end   := fifelse(!is.na(t2_start), pmin(pregnancy_start_date + 180, pregnancy_end_date), as.IDate(NA))]
  dt[, t3_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 181, pregnancy_start_date + 181, as.IDate(NA))][, t3_end   := fifelse(!is.na(t3_start), pregnancy_end_date, as.IDate(NA))]
  
  # calculate period lengths (inclusive of both ends)
  dt[, t0_len := as.integer(t0_end - t0_start + 1)]
  dt[, t1_len := as.integer(t1_end - t1_start + 1)]
  dt[, t2_len := fifelse(!is.na(t2_start) & !is.na(t2_end), as.integer(t2_end - t2_start + 1), NA_integer_)]
  dt[, t3_len := fifelse(!is.na(t3_start) & !is.na(t3_end), as.integer(t3_end - t3_start + 1), NA_integer_)]
  
  # compute rx overlap days between two intervals function
  overlap_days <- function(start1, end1, start2, end2) fifelse(!is.na(start2) & !is.na(end2), pmax(0L, as.integer(pmin(end1, end2) - pmax(start1, start2) + 1L)), 0L)
  
  # apply for each period
  dt[, t0 := overlap_days(rx_start, rx_end, t0_start, t0_end)]
  dt[, t1 := overlap_days(rx_start, rx_end, t1_start, t1_end)]
  dt[, t2 := overlap_days(rx_start, rx_end, t2_start, t2_end)]
  dt[, t3 := overlap_days(rx_start, rx_end, t3_start, t3_end)]
  
  # remove rows where all cols are 0
  dt <- dt[!(t0 == 0 & t1 == 0 & t2 == 0 & t3 == 0)]
  
  # Get only the subset of meds we will be doing calculations for 
  dt_subset <- dt[code %in% c("N02BF01", "N03AA02", "N03AF01", "N03AG01", "N03AX09", "N03AX11","N03AX14", "N03AE01"), preg_start_year:=year(pregnancy_start_date)]

  # calculations per period 
  periods <- c("t0", "t1", "t2", "t3")
  
  for (p in periods) {
    # calculate proportion of rx days to duration - cap it to 1
    dt_subset[, paste0("prop_", p) := pmin(get(p) / assumed_duration, 1)] 
    
    # calculate the amount of medication taken during this period
    dt_subset[, paste0("amount_", p) := get(paste0("prop_", p)) * disp_number_medicinal_product]
    
    #compute strength considering formulation amount
    dt_subset[, paste0("strength_", p) := get(paste0("amount_", p)) * subst1_amount_per_form]

    # sum strength per person per pregnancy
    dt_subset[, paste0("strength_", p, "_sum") := sum(get(paste0("strength_", p))), by = .(person_id, pregnancy_start_date, atc_group)]
    
    # compute daily dose 
    dt_subset[, paste0("daily_dose_", p) := get(paste0("strength_", p, "_sum")) / get(paste0(p, "_len"))]

    # convert to DDD 
    dt_subset[code == "N02BF01", paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / 1800] # Gabapentin
    dt_subset[code == "N03AA02", paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / 100]  # Phenobarbital
    dt_subset[code == "N03AE01", paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / 8]    # Clonazepam
    dt_subset[code == "N03AF01", paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / 1000] # Carbamazepine
    dt_subset[code == "N03AG01", paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / 1500] # Valproic Acid
    dt_subset[code == "N03AX09", paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / 300]  # Lamotrigine
    dt_subset[code == "N03AX11", paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / 300]  # Topiramate
    dt_subset[code == "N03AX14", paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / 1500] # Levetiracetam

    
    # categorize dose groups
    dt_subset[, paste0("dose_group_", p) := fcase(
      is.na(get(paste0("dose_DDD_", p))), "missing",
      get(paste0("dose_DDD_", p)) <= 0.01 | get(paste0("dose_DDD_", p)) > 5, "invalid",
      get(paste0("dose_DDD_", p)) <= 0.49, "low",
      get(paste0("dose_DDD_", p)) < 1.5, "mid",
      get(paste0("dose_DDD_", p)) >= 1.5, "high"
    )]
    
  }
  
  # count the number of people per group per period
  for (p in periods) {
    
    # Numerator: count unique person-atc-year-dose
    numerator <- dt_subset[, .(n_in_period = uniqueN(person_id)), by = .(preg_start_year, atc_group, get(paste0("dose_group_", p)))]
    # Denominator: total unique ASM users per year
    denominator <- dt_all[, .(unique_persons_continuers_and_late_discontinuers_in_that_year = uniqueN(person_id)), by = preg_start_year]
    
    # Merge numerator and denominator to calculate proportion
    counts <- merge(numerator, denominator, by = "preg_start_year")
    counts[, proportion := n_in_period / unique_persons_continuers_and_late_discontinuers_in_that_year]
  
    # Proportion
    counts[, proportion := n_in_period / unique_persons_continuers_and_late_discontinuers_in_that_year]
    
    # Save counts
    saveRDS(counts, file = file.path(paths$D5_dir, "1.5_mean_weighted_daily_dose", paste0("dose_group_summary_", p, "counts.rds")))
  }
  # Save data 
  saveRDS(dt_subset, file = file.path(paths$D4_dir, "1.5_mean_weighted_daily_dose", paste0("dose_group_data.rds")))
}









