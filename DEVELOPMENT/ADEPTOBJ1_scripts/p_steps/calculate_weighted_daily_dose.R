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

# Exclude subgroups not needed
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

#####################################################################################################
# Load input files
#####################################################################################################
# Continuous users
continuous_files <- list.files(file.path(paths$D4_dir, "1.3_pregnancy_continuous"), full.names = TRUE)
continuous_files <- continuous_files[grepl(paste0("^", pop_prefix, "_"), basename(continuous_files))]# keep current pop prefix
continuous_files <- continuous_files[!grepl(paste(exclude, collapse = "|"), basename(continuous_files))] # exclude groups
dt_continuous    <- rbindlist(lapply(continuous_files, readRDS), use.names = TRUE, fill = TRUE) # read in and bind all files

# Late discontinuers
discontinued_files <- list.files(file.path(paths$D4_dir, "1.4_pregnancy_discontinuation"), full.names = TRUE)
discontinued_files <- discontinued_files[grepl(paste0("^", pop_prefix, "_"), basename(discontinued_files))] # keep current pop prefix
discontinued_files <- discontinued_files[grepl("t2", basename(discontinued_files), ignore.case = TRUE)] # keep only late discontinuers
discontinued_files <- discontinued_files[!grepl(paste(exclude, collapse = "|"), basename(discontinued_files))] # exclude groups
dt_discontinued    <- rbindlist(lapply(discontinued_files, readRDS), use.names = TRUE, fill = TRUE) # read in and bind all files

# Exposure files
exposure_files <- list.files(file.path(paths$D3_dir, "exposure"))

# Products table
dt_products <- rbindlist(lapply(list.files(file.path(CDM_dir), pattern = "^PRODUCTS", full.names = TRUE), fread), use.names = TRUE, fill = TRUE)
dt_products <- as.data.table(unique(dt_products, by = "medicinal_product_id"))
# change medicinal_product_id to character
dt_products[, medicinal_product_id := as.character(medicinal_product_id)]

#####################################################################################################
# Helper: compute overlap days
#####################################################################################################
overlap_days <- function(start1, end1, start2, end2) fifelse(!is.na(start2) & !is.na(end2), pmax(0L, as.integer(pmin(end1, end2) - pmax(start1, start2) + 1L)), 0L)

#####################################################################################################
# Levels
#####################################################################################################
dose_levels <- c("low", "mid", "high", "invalid")
all_years <- seq(year(start_study_date) + 1, year(end_study_date))  # start one year after study start
all_combinations <- CJ(preg_start_year = all_years, dose_group = dose_levels, unique = TRUE)

#####################################################################################################
# Bind both files
#####################################################################################################

cols_needed <- c("person_id", "episode.start", "episode.end", "atc_group", "code", "pregnancy_start_date", "pregnancy_end_date")
if (nrow(dt_continuous) > 0 && nrow(dt_discontinued) > 0) dt_all <- rbindlist(list(dt_continuous[, ..cols_needed], dt_discontinued[, ..cols_needed]), use.names = TRUE, fill = TRUE)
if (nrow(dt_continuous) > 0 && nrow(dt_discontinued) == 0) dt_all <- copy(dt_continuous[, ..cols_needed])
if (nrow(dt_continuous) == 0 && nrow(dt_discontinued) > 0) dt_all <- copy(dt_discontinued[, ..cols_needed])
if (nrow(dt_continuous) == 0 && nrow(dt_discontinued) == 0)  message("No continuous or late discontinuer users were found")


#####################################################################################################
# Loop per ATC
#####################################################################################################
if (nrow(dt_all) > 0) {
  for (atc in target_atcs) {
    message("Processing ATC: ", atc)
    # Create a subset of atc code
    dt_all_atc <- dt_all[code == atc, ]
    # Remove true duplicates
    dt_all_atc <- unique(dt_all_atc)
    if (nrow(dt_all_atc) == 0) {
      message(red("No continuers or late discontinuers for: ", atc))
      next
    }
    # Prepare for overlaps
    ########################
    ### dt_all_atc
    ########################
    # set intervals
    dt_all_atc[, window_start := as.IDate(episode.start)]
    dt_all_atc[, window_end := as.IDate(episode.end)]
    # create preg_start_year column
    dt_all_atc[, preg_start_year := year(pregnancy_start_date)]
    # reorder columns in dt_all so first three are person_id, window_start, window_end - needed for foroverlaps
    setcolorder(dt_all_atc, c("person_id", "window_start", "window_end", setdiff(names(dt_all_atc), c("person_id", "window_start", "window_end"))))
    # set keys
    setkey(dt_all_atc, person_id, window_start, window_end)
    # Add pre-pregnancy and trimester windows cols
    dt_all_atc[, t0_start := pregnancy_start_date - 91][, t0_end := pregnancy_start_date - 1]
    dt_all_atc[, t1_start := pregnancy_start_date][, t1_end := pmin(pregnancy_start_date + 90, pregnancy_end_date)]
    dt_all_atc[, t2_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 90, pregnancy_start_date + 91, as.IDate(NA))][, t2_end   := fifelse(!is.na(t2_start), pmin(pregnancy_start_date + 181, pregnancy_end_date), as.IDate(NA))]
    dt_all_atc[, t3_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 181, pregnancy_start_date + 182, as.IDate(NA))][, t3_end   := fifelse(!is.na(t3_start), pregnancy_end_date, as.IDate(NA))]
    # calculate period lengths (inclusive of both ends)
    dt_all_atc[, t0_len := as.integer(t0_end - t0_start + 1)]
    dt_all_atc[, t1_len := as.integer(t1_end - t1_start + 1)]
    dt_all_atc[, t2_len := fifelse(!is.na(t2_start) & !is.na(t2_end), as.integer(t2_end - t2_start + 1), NA_integer_)]
    dt_all_atc[, t3_len := fifelse(!is.na(t3_start) & !is.na(t3_end), as.integer(t3_end - t3_start + 1), NA_integer_)]
    ########################
    ### dt_exposure
    ########################
    # find matching exposure file
    if (deap_flags$is_FIN_REG)  file_match <- exposure_files[grepl(unique(dt_all_atc$atc_group), exposure_files)]
    if (!deap_flags$is_FIN_REG) file_match <- exposure_files[grepl(paste0("_F_", unique(dt_all_atc$atc_group)), exposure_files)]
    # exclude unwanted subgroups
    file_match <- file_match[!grepl(paste(exclude, collapse = "|"), file_match)]
    if (length(file_match) == 0) {
      message("No exposure data for ATC: ", atc)
      next
    }
    # read in matching exposure file
    dt_exp <- unique(readRDS(file.path(paths$D3_dir, "exposure", file_match)))
    # prepare medicinal product id column for merge
    dt_exp[, medicinal_product_id := as.character(medicinal_product_id)]
    # set intervals
    dt_exp[, event_start := as.IDate(rx_date)]
    dt_exp[, event_end  := as.IDate(rx_date)]
    # reorder columns so first three are person_id, event_start, event_start - needed for foroverlaps
    setcolorder(dt_exp, c("person_id", "event_start", "event_end", setdiff(names(dt_exp), c("person_id", "event_start", "event_end"))))
    # set keys on both tables by the interval columns
    setkey(dt_exp, person_id, event_start, event_end)
    # Loop through person_id/pregnancy_start pairs to get rx that match the episode and pregnancy
    # Create a data.table of unique combinations
    unique_pairs <- unique(dt_all_atc[, .(person_id, pregnancy_start_date)])
    # Create a list to store results for each pair
    results_list <- vector("list", nrow(unique_pairs))
    # Loop over each unique pair
    for (i in seq_len(nrow(unique_pairs))) {
      # Subset dt_all_atc for this person and pregnancy
      subset_dt <- dt_all_atc[person_id == unique_pairs$person_id[i] & pregnancy_start_date == unique_pairs$pregnancy_start_date[i]]
      # Perform foverlaps with prescriptions
      overlap_dt <- foverlaps(
        dt_exp[, .(person_id, event_start, event_end, rx_date, medicinal_product_id,
                   disp_number_medicinal_product, presc_quantity_per_day, assumed_duration, presc_duration_days)],
        subset_dt,
        by.x = c("person_id", "event_start", "event_end"),
        by.y = c("person_id", "window_start", "window_end"),
        nomatch = 0
      )
      if (nrow(overlap_dt) == 0) {
        message(red("No overlaps found for: ", atc))
        next
      }
      # order by person_id, pregnancy_start_date, rx_date
      setorder(overlap_dt, person_id, pregnancy_start_date, rx_date)
      # create intervals for the rx: rx_start to 1 day before the next rx_start. If last rx_start, then the end is the episode end
      overlap_dt[, rx_start := rx_date]
      overlap_dt[, rx_end := shift(rx_date, type = "lead") - 1, by = .(person_id, pregnancy_start_date)]
      overlap_dt[is.na(rx_end), rx_end := episode.end]
      # calculate overlap in days
      overlap_dt[, t0 := overlap_days(rx_start, rx_end, t0_start, t0_end)]
      overlap_dt[, t1 := overlap_days(rx_start, rx_end, t1_start, t1_end)]
      overlap_dt[, t2 := overlap_days(rx_start, rx_end, t2_start, t2_end)]
      overlap_dt[, t3 := overlap_days(rx_start, rx_end, t3_start, t3_end)]
      # remove rows where all cols are 0
      overlap_dt <- overlap_dt[!(t0 == 0 & t1 == 0 & t2 == 0 & t3 == 0)]
      if (nrow(overlap_dt) == 0) {
        message(red("no overlaps found for any period for ATC: ", atc))
        next
      }
      # Store result in list
      results_list[[i]] <- overlap_dt
    }
    # Combine all results into a single data.table
    dt <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
    # drop cols you dont need
    dt[, c("window_start", "window_end", "event_start", "event_end") := NULL]
    # keep unique rows
    dt <- unique(dt, by = c("person_id", "pregnancy_start_date", "rx_date"))
    ################################################################################################################################################
    #################### EHR DATABASES #############################################################################################################
    ################################################################################################################################################
    if (deap_flags$is_BIFAP || deap_flags$is_CPRD || deap_flags$is_VID) {
      # left join: keep all prescriptions, add product info
      dt <- merge(dt, dt_products, by = "medicinal_product_id", all.x = TRUE)
      # Check again after merge
      message("Rows with unmatched products: ", nrow(dt[!medicinal_product_id %in% dt_products$medicinal_product_id]))
      if (nrow(dt) == 0) {
        message(red("No product code matches for: ", atc))
        next
      }
      ########################################################################
      # Dose calculations per period
      ########################################################################
      periods <- c("t0", "t1", "t2", "t3")
      ddd_val <- ddd_lookup[code == atc, ddd]
      # copy dt for a subset
      dt_subset <- copy(dt)
      for (p in periods) {
        # calculate proportion of rx days to duration - cap it to 1
        dt_subset[, paste0("prop_", p) := pmin(get(p) / assumed_duration, 1)]
        # calculate the amount of medication taken during this period
        if (deap_flags$is_CPRD || deap_flags$is_PHARMO) dt_subset[, paste0("amount_", p) := get(paste0("prop_", p)) * disp_number_medicinal_product]
        if (deap_flags$is_BIFAP || deap_flags$is_VID) {
          dt_subset[, paste0("amount_", p) := get(paste0("prop_", p)) * disp_number_medicinal_product * unit_of_presentation_num]
        }
        # compute strength considering formulation amount
        dt_subset[, paste0("strength_", p) := get(paste0("amount_", p)) * subst1_amount_per_form]
        # sum strength per person per pregnancy
        dt_subset[, paste0("strength_", p, "_sum") := sum(get(paste0("strength_", p))), by = .(person_id, pregnancy_start_date)]
        # compute daily dose
        dt_subset[, paste0("daily_dose_", p) := get(paste0("strength_", p, "_sum")) / get(paste0(p, "_len"))]
        # compute dose in DDD
        dt_subset[, paste0("dose_DDD_", p) := get(paste0("daily_dose_", p)) / ddd_val]
        # categorize dose
        dt_subset[get(paste0("dose_DDD_", p)) >= 0 & get(paste0("dose_DDD_", p)) <= 0.49, paste0("dose_group_", p) := "low"]
        dt_subset[get(paste0("dose_DDD_", p)) > 0.49 & get(paste0("dose_DDD_", p)) < 1.5, paste0("dose_group_", p) := "mid"]
        dt_subset[get(paste0("dose_DDD_", p)) >= 1.5 & get(paste0("dose_DDD_", p)) <= 5, paste0("dose_group_", p) := "high"]
        # If dose_DDD not missing but implausible
        dt_subset[get(paste0("dose_DDD_", p)) > 5 | get(paste0("dose_DDD_", p)) < 0, paste0("dose_group_", p) := "invalid"]
        # if dose_DDD is missing
        dt_subset[is.na(get(paste0("dose_DDD_", p))) | is.infinite(get(paste0("dose_DDD_", p))), paste0("dose_group_", p) := "missing"]
      }
    }
    ################################################################################################################################################
    #################### ADMINISTRATIVE DATABASES #############################################################################################################
    ################################################################################################################################################
    if (deap_flags$is_NOR_REG || deap_flags$is_FIN_REG || deap_flags$is_VAL_PAD) {
      ########################################################################
      # Dose calculations per period
      ########################################################################
      periods <- c("t0", "t1", "t2", "t3")
      ddd_val <- ddd_lookup[code == atc, ddd]
      # copy dt for a subset
      dt_subset <- copy(dt)
      for (p in periods) {
        # calculate proportion of rx days to duration - cap it to 1
        dt_subset[, paste0("prop_", p) := pmin(get(p) / assumed_duration, 1)]
        if (deap_flags$is_FIN_REG) dt_subset[, paste0("amount_ddd_", p) := get(paste0("prop_", p)) * disp_number_medicinal_product * presc_duration_days]
        if (deap_flags$is_NOR_REG || deap_flags$is_VAL_PAD) dt_subset[, paste0("amount_ddd_", p) := get(paste0("prop_", p)) * presc_duration_days]
        # sum strength per person per pregnancy
        dt_subset[, paste0("amount_ddd_", p, "_sum") := sum(get(paste0("amount_ddd_", p))), by = .(person_id, pregnancy_start_date)]
        # compute daily dose
        dt_subset[, paste0("dose_DDD_", p) := get(paste0("amount_ddd_", p, "_sum")) / get(paste0(p, "_len"))]
        # categorize dose
        dt_subset[get(paste0("dose_DDD_", p)) >= 0 & get(paste0("dose_DDD_", p)) <= 0.49, paste0("dose_group_", p) := "low"]
        dt_subset[get(paste0("dose_DDD_", p)) > 0.49 & get(paste0("dose_DDD_", p)) < 1.5, paste0("dose_group_", p) := "mid"]
        dt_subset[get(paste0("dose_DDD_", p)) >= 1.5 & get(paste0("dose_DDD_", p)) <= 5, paste0("dose_group_", p) := "high"]
        # If dose_DDD not missing but implausible
        dt_subset[get(paste0("dose_DDD_", p)) > 5 | get(paste0("dose_DDD_", p)) < 0, paste0("dose_group_", p) := "invalid"]
        # if dose_DDD is missing
        dt_subset[is.na(get(paste0("dose_DDD_", p))) | is.infinite(get(paste0("dose_DDD_", p))), paste0("dose_group_", p) := "missing"]
      }
    }
    ########################################################################
    # Numerator & denominator per period
    ########################################################################
    for (p in periods) {
      # numerator
      numerator <- dt_subset[, .(n_in_period = uniqueN(paste(person_id, pregnancy_start_date, sep = "_"))), by = .(preg_start_year, dose_group = get(paste0("dose_group_", p)))]
      # merge with all combinations to fill missing dose groups
      numerator <- merge(all_combinations, numerator, by = c("preg_start_year", "dose_group"), all.x = TRUE)
      numerator[is.na(n_in_period), n_in_period := 0]
      # denominator
      denominator <- dt_all_atc[, .(n_overall = uniqueN(paste(person_id, pregnancy_start_date, sep = "_"))), by = preg_start_year]
      # TEST - check consistency per year
      check <- numerator[, .(sum_n_in_period = sum(n_in_period)), by = preg_start_year]
      check <- merge(check, denominator, by = "preg_start_year", all.x = TRUE)
      # print mismatches
      mismatch <- check[sum_n_in_period != n_overall]
      if (nrow(mismatch) > 0) {
        message("WARNING: For period ", p, " the following years don't add up:")
        print(mismatch)
      } else {
        message("OK: For period ", p, " sums match denominator per year.")
      }
      # merge numerator and denominator
      counts <- merge(numerator, denominator, by = "preg_start_year", all.x = TRUE)
      counts[is.na(n_overall), n_overall := 0]
      # compute proportion
      counts[, proportion := n_in_period / n_overall]
      counts[n_overall == 0, proportion := 0][, rate_computable := proportion > 0]
      # add metadata
      counts[, `:=`(atc_group = atc, period = p)]
      # save
      saveRDS(counts, file = file.path(paths$D5_dir, "1.5_mean_weighted_daily_dose", paste0(unique(dt_all_atc$atc_group), "_dose_group_summary_", p, ".rds")))
    }
    # Save detailed data
    saveRDS(dt_subset, file = file.path(paths$D4_dir, "1.5_mean_weighted_daily_dose", paste0(unique(dt_all_atc$atc_group), "_dose_group_data.rds")))
  }
}
