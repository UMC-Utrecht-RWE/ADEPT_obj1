###################################################################################################################################################
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

######################################################################################################################################################
print("========================================================================================")
print("========================= CALCULATING MEAN WEIGHTED DAILY DOSE =========================")
print("========================================================================================")

# List of ATCs of interest
target_atcs <- c("N02BF01", "N03AX12", "N03AA02", "N03AE01", "N03AF01", "N03AG01", "N03AX09", "N03AX11", "N03AX14")

# Lookup table for DDD denominators
ddd_lookup <- data.table(
  code = target_atcs,
  ddd = c(
    1800, # N02BF01 Gabapentin
    1800, # N03AX12 Gabapentin
    100,  # N03AA02 Phenobarbital
    8,    # N03AE01 Clonazepam
    1000, # N03AF01 Carbamazepine
    1500, # N03AG01 Valproic Acid
    300,  # N03AX09 Lamotrigine
    300,  # N03AX11 Topiramate
    1500  # N03AX14 Levetiracetam
  )
)

# List of subgroups - to be excluded 
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# Load Input Files (continuous users and late discontinuers)
# Continuous Users 
continuous_files <- list.files(file.path(paths$D4_dir, "1.3_pregnancy_continuous"), full.names = TRUE)         # all continuous files in folder
continuous_files <- continuous_files[grepl(paste0("^", pop_prefix, "_"), basename(continuous_files))]          # current pop prefix
continuous_files <- continuous_files[!grepl(paste(exclude, collapse = "|"), basename(continuous_files))]       # exclude subgroups
dt_continuous    <- as.data.table(rbindlist(lapply(continuous_files, readRDS), use.names = TRUE, fill = TRUE)) # read in and bind all files, set as data table
dt_continuous    <- unique(dt_continuous)                                                                      # remove true duplicates
# Keep only episodes that contribute to the determination of the continued episode
dt_continuous<- dt_continuous[episode.end >= pregnancy_start_date,]

# Late discontinuers
discontinued_files <- list.files(file.path(paths$D4_dir, "1.4_pregnancy_discontinuation"), full.names = TRUE)      # all late discontinuer files in folder 
discontinued_files <- discontinued_files[grepl(paste0("^", pop_prefix, "_"), basename(discontinued_files))]        # current pop prefix
discontinued_files <- discontinued_files[!grepl(paste(exclude, collapse = "|"), basename(discontinued_files))]     # exclude subgroups
discontinued_files <- discontinued_files[grepl("t2", basename(discontinued_files), ignore.case = TRUE)]            # filter for late discontinuers only (t2)
dt_discontinued    <- as.data.table(rbindlist(lapply(discontinued_files, readRDS), use.names = TRUE, fill = TRUE)) # read in and bind all files, set as data table
dt_discontinued    <- unique(dt_discontinued)                                                                      # remove true duplicates
# Keep only episodes that contribute to the determination of the discontinued episode
dt_discontinued <- dt_discontinued[episode.end >= pregnancy_start_date,]

# Bind both continuous user and late discontinuer users into 1 file
cols_needed <- c("person_id", "pregnancy_id" ,"episode.start", "episode.end", "atc_group", "code", "pregnancy_start_date", "pregnancy_end_date") # list of columns we want to keep
if (nrow(dt_continuous) >  0 && nrow(dt_discontinued) >  0) dt_all <- rbindlist(list(dt_continuous[, ..cols_needed], dt_discontinued[, ..cols_needed]), use.names = TRUE, fill = TRUE)
if (nrow(dt_continuous) >  0 && nrow(dt_discontinued) == 0) dt_all <- copy(dt_continuous[, ..cols_needed])
if (nrow(dt_continuous) == 0 && nrow(dt_discontinued) >  0) dt_all <- copy(dt_discontinued[, ..cols_needed])
if (nrow(dt_continuous) == 0 && nrow(dt_discontinued) == 0) message("No continuous or late discontinuer users were found")

# Check if dt_all exists and has at least one row
if (exists("dt_all") && nrow(dt_all) > 0) {
  
  # Exposure files
  exposure_files <- list.files(file.path(paths$D3_dir, "exposure"))
  
  # Create helper function to compute number of overlapping days
  overlap_days <- function(start1, end1, start2, end2) fifelse(!is.na(start2) & !is.na(end2), pmax(0L, as.integer(pmin(end1, end2) - pmax(start1, start2) + 1L)), 0L)
  
  # Prepare empty table for counts
  dose_levels <- c("low", "mid", "high", "invalid", "missing")
  all_years <- seq(year(start_study_date) + 1, year(end_study_date))  # start one year after study start
  all_combinations <- CJ(preg_start_year = all_years, dose_group = dose_levels, unique = TRUE)
  
  # Loop over target atc's 
  for (atc in target_atcs) {
    
    # Print message
    message("Processing ATC: ", atc)
    
    # Create a subset of current ATC
    dt_all_atc <- dt_all[code == atc, ]
    
    # Remove true duplicates
    dt_all_atc <- unique(dt_all_atc)
    
    if (nrow(dt_all_atc) == 0) {
      message(red("No continuers or late discontinuers for: ", atc))
      next
    }
    
    ############################################################################
    # Add pregnancy_start year
    dt_all_atc[, preg_start_year := year(pregnancy_start_date)]
    
    ############################################################################
    # Create pre-pregnancy and trimester windows
    dt_all_atc[, t0_start := pregnancy_start_date - 91][, t0_end := pregnancy_start_date - 1]
    dt_all_atc[, t1_start := pregnancy_start_date][, t1_end := pmin(pregnancy_start_date + 90, pregnancy_end_date)]
    dt_all_atc[, t2_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 90, pregnancy_start_date + 91, as.IDate(NA))][, t2_end   := fifelse(!is.na(t2_start), pmin(pregnancy_start_date + 181, pregnancy_end_date), as.IDate(NA))]
    dt_all_atc[, t3_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 181, pregnancy_start_date + 182, as.IDate(NA))][, t3_end   := fifelse(!is.na(t3_start), pregnancy_end_date, as.IDate(NA))]
    
    ############################################################################
    # Calculate period lengths (inclusive of both ends)
    dt_all_atc[, t0_len := as.integer(t0_end - t0_start + 1)]
    dt_all_atc[, t1_len := as.integer(t1_end - t1_start + 1)]
    dt_all_atc[, t2_len := fifelse(!is.na(t2_start) & !is.na(t2_end), as.integer(t2_end - t2_start + 1), NA_integer_)]
    dt_all_atc[, t3_len := fifelse(!is.na(t3_start) & !is.na(t3_end), as.integer(t3_end - t3_start + 1), NA_integer_)]
    
    ############################################################################
    # Read in corresponding exposure file (with prescription dates, not treatment episodes)
    # Filter for matching exposure file 
    if ( deap_flags$is_FIN_REG ||  deap_flags$is_EFEMERIS) file_match <- exposure_files[grepl(unique(dt_all_atc$atc_group), exposure_files)]
    if (!deap_flags$is_FIN_REG && !deap_flags$is_EFEMERIS) file_match <- exposure_files[grepl(paste0("_F_", unique(dt_all_atc$atc_group)), exposure_files)]
    
    # Exclude subgroups 
    file_match <- file_match[!grepl(paste(exclude, collapse = "|"), file_match)]
    
    # Skip to next if not found - should not be possible but just in case 
    if (length(file_match) == 0) {
      message("No exposure data for ATC: ", atc)
      next
    }
    
    # Read in matching exposure file
    dt_exp <- readRDS(file.path(paths$D3_dir, "exposure", file_match))
    dt_exp <- unique(dt_exp) # remove true duplicates
    
    ############################################################################
    # Prepare for overlaps 
    setcolorder(dt_all_atc, c("person_id", "episode.start", "episode.end", setdiff(names(dt_all_atc), c("person_id", "episode.start", "episode.end")))) # reorder cols
    setkey(dt_all_atc, person_id, episode.start, episode.end) # set keys
    
    # dt_exposure
    dt_exp[, event_start := as.IDate(rx_date)][, event_end  := as.IDate(rx_date)] # set intervals
    setcolorder(dt_exp, c("person_id", "event_start", "event_end", setdiff(names(dt_exp), c("person_id", "event_start", "event_end")))) # reorder cols
    setkey(dt_exp, person_id, event_start, event_end) # set keys
    
    # overlap join 
    overlap_dt <- foverlaps(
      dt_exp[, .(person_id, event_start, event_end, rx_date, medicinal_product_id, disp_number_medicinal_product, presc_quantity_per_day, assumed_duration, presc_duration_days)],              
      dt_all_atc,         
      by.x = c("person_id", "event_start", "event_end"),   
      by.y = c("person_id", "episode.start", "episode.end"), 
      nomatch = 0        
    )
    
    # Order by person_id, pregnancy_start_date, rx_date
    setorder(overlap_dt, person_id, pregnancy_start_date, rx_date)
    
    # Create intervals for the rx: rx_start to 1 day before the next rx_start. If last rx_start, then the end is the episode end
    overlap_dt[, rx_start := rx_date]
    overlap_dt[, rx_end := shift(rx_date, type = "lead") - 1, by = .(person_id, pregnancy_start_date)]
    overlap_dt[is.na(rx_end), rx_end := episode.end]
    
    # For same-day prescriptions, expand rx_end to the max Rx_end of that day
    overlap_dt[, rx_end := max(rx_end, na.rm = TRUE), by = .(person_id, pregnancy_start_date, rx_date)]
    
    # Calculate overlap in days
    overlap_dt[, t0 := overlap_days(rx_start, rx_end, t0_start, t0_end)]
    overlap_dt[, t1 := overlap_days(rx_start, rx_end, t1_start, t1_end)]
    overlap_dt[, t2 := overlap_days(rx_start, rx_end, t2_start, t2_end)]
    overlap_dt[, t3 := overlap_days(rx_start, rx_end, t3_start, t3_end)]
    
    # Remove rows where overlap is 0 for all cols
    overlap_dt <- overlap_dt[!(t0 == 0 & t1 == 0 & t2 == 0 & t3 == 0)]
    
    # Drop cols you dont need
    dt <- overlap_dt[, c("event_start", "event_end") := NULL]
    
    
    ############################################################################
    #################### EHR + COHORT DATABASES ################################
    ############################################################################
    # Use Products Table
    if (deap_flags$is_BIFAP    || 
        deap_flags$is_CPRD     || 
        deap_flags$is_EFEMERIS ||
        deap_flags$is_VID) {
    
      # Load Products Table 
      products_files <- list.files(file.path(CDM_dir), pattern = "^PRODUCTS", full.names = TRUE) # list product tables in folder
      dt_products    <- rbindlist(lapply(products_files, fread), use.names = TRUE, fill = TRUE)  # read in table
      dt_products    <- unique(dt_products)                                                      # remove true duplicates
      dt_products[, medicinal_product_id := as.character(medicinal_product_id)]                  # make sure product id is character
    
      # Prepare dt for merge
      dt[, medicinal_product_id := as.character(medicinal_product_id)]
    
      # Left join: keep all prescriptions, add product info
      dt <- merge(dt, dt_products, by = "medicinal_product_id", all.x = TRUE)
    
      # Check again after merge
      message("Rows with unmatched products: ", nrow(dt[!medicinal_product_id %in% dt_products$medicinal_product_id]))
    
      ############################################################################
      # Dose Calculations Per Period
      # Set periods and DDD values
      periods <- c("t0", "t1", "t2", "t3")
      ddd_val <- ddd_lookup[code == atc, ddd]
      
      # Copy dt 
      dt_periods <- copy(dt)
      
      for (p in periods) {
        
        # Calculate proportion of rx days to duration - cap it to 1
        dt_periods[, paste0("prop_", p) := pmin(get(p) / assumed_duration, 1)]
        
        # Calculate the amount of medication taken during this period
        if (deap_flags$is_CPRD) dt_periods[, paste0("amount_", p) := get(paste0("prop_", p)) * disp_number_medicinal_product]
        
        if (deap_flags$is_BIFAP || deap_flags$is_VID || deap_flags$is_EFEMERIS) {
          if (deap_flags$is_EFEMERIS) dt_periods[is.na(disp_number_medicinal_product) & year(pregnancy_start_date)<2012, disp_number_medicinal_product:=1]
          dt_periods[, paste0("amount_", p) := get(paste0("prop_", p)) * disp_number_medicinal_product * unit_of_presentation_num]
        }
        
        # Compute strength considering formulation amount
        dt_periods[, paste0("strength_", p) := get(paste0("amount_", p)) * subst1_amount_per_form]
        
        # Sum strength per person per pregnancy
        dt_periods[, paste0("strength_", p, "_sum") := if(all(is.na(get(paste0("strength_", p))))) NA_real_
                                                       else sum(get(paste0("strength_", p)), na.rm = TRUE), 
                                                       by = .(person_id, pregnancy_start_date)]
        
        # Compute daily dose
        dt_periods[, paste0("daily_dose_", p) := get(paste0("strength_", p, "_sum")) / get(paste0(p, "_len"))]
        
        # Compute dose in DDD
        dt_periods[, paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / ddd_val]
        
        # Categorize dose
        dt_periods[get(paste0("dose_DDD_", p)) >= 0 & get(paste0("dose_DDD_", p)) <= 0.49, paste0("dose_group_", p) := "low"]
        dt_periods[get(paste0("dose_DDD_", p)) > 0.49 & get(paste0("dose_DDD_", p)) < 1.5, paste0("dose_group_", p) := "mid"]
        dt_periods[get(paste0("dose_DDD_", p)) >= 1.5 & get(paste0("dose_DDD_", p)) <= 5, paste0("dose_group_", p) := "high"]
        dt_periods[get(paste0("dose_DDD_", p)) > 5 | get(paste0("dose_DDD_", p)) < 0, paste0("dose_group_", p) := "invalid"] # not missing but implausible
        dt_periods[is.na(get(paste0("dose_DDD_", p))) | is.infinite(get(paste0("dose_DDD_", p))), paste0("dose_group_", p) := "missing"] # missing
      }
    }
    
    ############################################################################
    #################### ADMINISTRATIVE DATABASES ##############################
    ############################################################################
    if (deap_flags$is_FIN_REG || 
        deap_flags$is_NOR_REG || 
        deap_flags$is_VAL_PAD) {
      
      ############################################################################
      # Dose Calculations Per Period
      # Set periods and DDD values
      periods <- c("t0", "t1", "t2", "t3")
      ddd_val <- ddd_lookup[code == atc, ddd]

      # Copy dt 
      dt_periods <- copy(dt)
      
      for (p in periods) {
        
        # Calculate proportion of rx days to duration - cap it to 1
        dt_periods[, paste0("prop_", p) := pmin(get(p) / assumed_duration, 1)]
        
        # Calculate the amount of medication taken during this period
        if (deap_flags$is_FIN_REG)                          dt_periods[, paste0("amount_ddd_", p) := get(paste0("prop_", p)) * disp_number_medicinal_product * presc_duration_days]
        if (deap_flags$is_NOR_REG || deap_flags$is_VAL_PAD) dt_periods[, paste0("amount_ddd_", p) := get(paste0("prop_", p)) * presc_duration_days]
        
        # Sum strength per person per pregnancy
        dt_periods[, paste0("amount_ddd_", p, "_sum") := sum(get(paste0("amount_ddd_", p))), by = .(person_id, pregnancy_start_date)]
        
        # Compute daily dose
        dt_periods[, paste0("dose_DDD_", p) := get(paste0("amount_ddd_", p, "_sum")) / get(paste0(p, "_len"))]
        
        # Categorize dose
        dt_periods[get(paste0("dose_DDD_", p)) >= 0 & get(paste0("dose_DDD_", p)) <= 0.49, paste0("dose_group_", p) := "low"]
        dt_periods[get(paste0("dose_DDD_", p)) > 0.49 & get(paste0("dose_DDD_", p)) < 1.5, paste0("dose_group_", p) := "mid"]
        dt_periods[get(paste0("dose_DDD_", p)) >= 1.5 & get(paste0("dose_DDD_", p)) <= 5, paste0("dose_group_", p) := "high"]
        dt_periods[get(paste0("dose_DDD_", p)) > 5 | get(paste0("dose_DDD_", p)) < 0, paste0("dose_group_", p) := "invalid"] # not missing but implausible
        dt_periods[is.na(get(paste0("dose_DDD_", p))) | is.infinite(get(paste0("dose_DDD_", p))), paste0("dose_group_", p) := "missing"] # missing
      }
    }
    
    ########################################################################
    # Numerator & denominator per period
    ########################################################################
    
    for (p in periods) {
      
      # Prepare Numerator
      # Deduplicate by pregnancy_id and current dose group, keep year for counting
      dt_num <- unique(dt_periods[, .(pregnancy_id, preg_start_year, dose_group = get(paste0("dose_group_", p)))], by = c("pregnancy_id", "dose_group"))
      
      # Count unique pregnancies per dose group per year
      num_counts <- dt_num[, .(n_in_period = .N), by = .(preg_start_year, dose_group)]
      
      # Merge with all combinations to fill missing dose groups
      numerator <- merge(all_combinations, num_counts, by = c("preg_start_year", "dose_group"), all.x = TRUE)
      numerator[is.na(n_in_period), n_in_period := 0]
      
      # Prepare denominator 
      dt_denom <- unique(dt_all_atc, by = c("pregnancy_id"))
      denom_counts <- dt_denom[, .(n_overall = .N), by = preg_start_year]
      
      # TEST - check consistency per year
      check <- numerator[, .(sum_n_in_period = sum(n_in_period)), by = preg_start_year]
      check <- merge(check, denom_counts, by = "preg_start_year", all.x = TRUE)
      check[is.na(n_overall), n_overall := 0]
      
      # Print mismatches
      mismatch <- check[sum_n_in_period != n_overall]
      if (nrow(mismatch) > 0) {
        message("WARNING: For period ", p, " the following years don't add up:")
        print(mismatch)
      } else {
        message("OK: For period ", p, " sums match denominator per year.")
      }
      
      # Merge numerator and denominator
      counts <- merge(numerator, denom_counts, by = "preg_start_year", all.x = TRUE)
      counts[is.na(n_overall), n_overall := 0]
      
      # Compute proportion
      counts[, proportion := n_in_period / n_overall]
      counts[n_overall == 0, proportion := 0][, rate_computable := proportion > 0]
      
      # Add metadata
      counts[, `:=`(atc_group = atc, period = p)]
      
      # Save
      saveRDS(counts, file = file.path(paths$D5_dir, "1.5_mean_weighted_daily_dose", paste0(unique(dt_all_atc$atc_group), "_dose_group_summary_", p, ".rds")))
    }
    
    # Save detailed data
    saveRDS(dt_periods, file = file.path(paths$D4_dir, "1.5_mean_weighted_daily_dose", paste0(unique(dt_all_atc$atc_group), "_dose_group_data.rds")))
  }
}
