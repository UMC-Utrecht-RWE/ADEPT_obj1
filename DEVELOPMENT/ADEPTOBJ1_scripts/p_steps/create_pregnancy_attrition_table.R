print("=============================================================================")
print("========================= PREGNANCY ATTRITION TABLE =========================")
print("=============================================================================")

# List exposure files to exclude
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# List exposure files
files_exposures <- list.files(file.path(paths$D3_dir, "exposure"), pattern = "\\.rds$", full.names = FALSE) 
files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)] # keep files of current pop prefix
files_exposures <- files_exposures[!grepl(paste(exclude, collapse = "|"), files_exposures)] # Exclude subgroups
if(!deap_flags$is_EFEMERIS && ! deap_flags$is_FIN_REG) files_exposures <- files_exposures[grepl("_F_", files_exposures, fixed = TRUE)] # Keep female records only 

# Bind all exposure files in list
dt_exposures <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "exposure", files_exposures), readRDS), fill = TRUE)) # read and bind datasets
dt_exposures <- unique(dt_exposures) # remove true duplicates

if (!deap_flags$is_EFEMERIS && !deap_flags$is_FIN_REG) {
  
  # Load pregnancies file
  load(file.path(preg_dir, "D3_pregnancy_final.RData"))
  pregnancies <- as.data.table(D3_pregnancy_final)
  
  # Remove duplicates
  pregnancies <- unique(pregnancies)
  
  # Convert pregnancy dates to IDate
  pregnancies[, pregnancy_start_date := as.IDate(pregnancy_start_date)][, pregnancy_end_date   := as.IDate(pregnancy_end_date)]
  
  # Merge pregnancies with study population to get start and end follow up. We want to keep only pregnancy starts within this period
  pregnancies <- merge(pregnancies, study_population[, .(person_id, start_follow_up, end_follow_up, entry_date, exit_date)], by = "person_id", allow.cartesian = TRUE)
  
  #<<< Flow chart >>> #
  # Total population (base cohort)
  total_population_base_cohort <- uniqueN(study_population$person_id)
  
  # Population without pregnancy
  female_study_population_without_pregnancy <- uniqueN(study_population$person_id) - uniqueN(pregnancies$person_id)
  
  # Population with pregnancy
  unique_pregnant_persons_in_study_population <- uniqueN(pregnancies$person_id)
  unique_pregnancies_in_study_population <- uniqueN(pregnancies$pregnancy_id)
  
  ###########################################################################################
  # Exclude pregnancies that start before study start or pregnancies that start after endfu
  pregnancy_end_before_study_start <- pregnancies[
    !( 
      between(pregnancy_start_date, start_study_date, end_study_date) &
        between(pregnancy_end_date,   start_study_date, end_study_date)
    )
  ]
  
  pregnancy_end_before_study_start_unique_persons <- uniqueN(pregnancy_end_before_study_start$person_id)
  pregnancy_end_before_study_start_unique_pregnancies <- uniqueN(pregnancy_end_before_study_start$pregnancy_id)
  
  #Exclude those outside study period
  pregnancies <- pregnancies[
      between(pregnancy_start_date, start_study_date, end_study_date) &
      between(pregnancy_end_date,   start_study_date, end_study_date)
  ]
  
  # Population with pregnancy and at least 12 months of fu
  unique_pregnant_persons_with_preg_start_after_study_start <- uniqueN(pregnancies$person_id)
  unique_pregnancies_with_preg_start_after_study_start <- uniqueN(pregnancies$pregnancy_id)
  
  ###########################################################################################
  # Exclude pregnancies that are before entry into cohort date (max of study start, op start and date min)
  pregnancy_start_before_CED <- pregnancies[
    !( 
      between(pregnancy_start_date, entry_date, exit_date)
    )
  ]
  
  pregnancy_start_before_CED_unique_persons <- uniqueN(pregnancy_start_before_CED$person_id)
  pregnancy_start_before_CED_unique_pregnancies <- uniqueN(pregnancy_start_before_CED$pregnancy_id)
  
  #Exclude those outside study period
  pregnancies <- pregnancies[
    between(pregnancy_start_date, entry_date, exit_date)
  ]
  
  # Population with pregnancy and at least 12 months of fu
  unique_persons_after_CED <- uniqueN(pregnancies$person_id)
  unique_pregnancies_after_CED_dates <- uniqueN(pregnancies$pregnancy_id)
  
  #########################################################
  # Exclude pregnancies that start after endfu
  #<<< flow chart >>> #
  pregnancy_start_after_endfu <- pregnancies[pregnancy_start_date >= end_follow_up, ] # pregnancies
  nr_of_pregnancy_starts_after_endfu_unique_persons <- uniqueN(pregnancy_start_after_endfu$person_id) # unique pregnancy persons with pregnancy after endfu
  nr_of_pregnancy_starts_after_endfu_unique_pregnancies <- uniqueN(pregnancy_start_after_endfu$pregnancy_id) # unique pregnancy persons with pregnancy after endfu
  
  # Exclude records where no lookback of at least a year
  pregnancies <- pregnancies[pregnancy_start_date < end_follow_up, ]
  
  # Population with pregnancy and at least 12 months of fu
  unique_pregnant_persons_in_study_population_with_pregstart_before_endfu <- uniqueN(pregnancies$person_id)
  unique_pregnancies_in_study_population_with_pregstart_before_endfu <- uniqueN(pregnancies$pregnancy_id)
  
  ##############################################################
  # Exclude pregnancies that  do not have at least a year of lookback -> pregnancy start needs to be equal or after startfu
  # Get Pregnancies with at least a year of lookback - pregnancy start needs after startfu
  #<<< flow chart >>> #
  pregnancies_wo_12_mnths_before_pregnancy_start <- pregnancies[pregnancy_start_date >= entry_date & pregnancy_start_date < start_follow_up, ] # pregnancies
  no_info_12_mnths_before_pregnancy_start_unique_persons <- uniqueN(pregnancies_wo_12_mnths_before_pregnancy_start$person_id) # unique pregnancy persons wo 12 months info prior
  no_info_12_mnths_before_pregnancy_start_unique_pregnancies <- uniqueN(pregnancies_wo_12_mnths_before_pregnancy_start$pregnancy_id) # unique pregnancy persons wo 12 months info prior
  
  # Exclude records where no lookback of at least a year
  pregnancies <- pregnancies[pregnancy_start_date >= start_follow_up, ]
  
  # Population with pregnancy and at least 12 months of fu
  unique_pregnant_persons_in_study_population_with_at_least_12_mnths_fu <- uniqueN(pregnancies$person_id)
  unique_pregnancies_in_study_population_with_at_least_12_mnths_fu <- uniqueN(pregnancies$pregnancy_id)
  
   #################################################################
  # Merge with dt_exposure to get pregnancy persons with rx a year before pregnancy start or within pregnancy
  pregnancies <- merge(pregnancies, dt_exposures[, .(person_id, code, rx_date)], by = "person_id", allow.cartesian = TRUE)
  
  # Prescriptions that occur within 1 year of pregnancy start or during pregnancy
  pregnancies <- pregnancies[rx_date >= as.IDate(as.Date(pregnancy_start_date) - lookback_period) & rx_date < pregnancy_end_date]
  
  #<<< Flow chart >>> #
  pregnant_persons_with_ASM_use    <- uniqueN(pregnancies$person_id)
  pregnancies_with_ASM_use         <- uniqueN(pregnancies$pregnancy_id)
  
  pregnant_persons_with_no_ASM_use <- unique_pregnant_persons_in_study_population_with_at_least_12_mnths_fu - pregnant_persons_with_ASM_use
  pregnancies_with_no_ASM_use      <- unique_pregnancies_in_study_population_with_at_least_12_mnths_fu - pregnancies_with_ASM_use
  

  # FLOWCHART  
  flow_data <- data.table(
    Step = c(
      "1. BASE COHORT",
      "   Total female study population",
      "",
      "2. PREGNANCY STATUS",
      "   Excluded: Females without pregnancy",
      "   Remaining: pregnant persons in study population (persons)",
      "   Remaining: pregnacies in study population (pregnancies)",
      "",
      "3. INSIDE STUDY PERIOD EXCLUSION - pregnancies outside the study period",
      "   Excluded: Pregnant persons outside study period (persons)",
      "   Excluded: Pregnancies outside study period (pregnancies)",
      "   Remaining: Pregnant persons within study period (persons)",
      "   Remaining: Pregnancies within study period (pregnancies)",
      "",
      "4. BEFORE CED Exclusion - pregnancies before Cohort Entry Date - entry-exit is determined per person",
      "   Excluded: Persons with pregnancy start before CED and after study start (persons)",
      "   Excluded: Pregnancies with pregnancy start before CED after study start (pregnancies)",
      "   Remaining: Pregnant Persons with pregnancies after CED (persons)",
      "   Remaining: Pregnancies after CED (pregnancies)",
      "",
      "5. AFTER FOLLOW UP EXCLUSION - pregnancies that start after endfu",
      "   Excluded: Pregnancy start after end FU (persons)",
      "   Excluded: Pregnancy start after end FU (pregnancies)",
      "   Remaining: Final pregnancy cohort (persons)",
      "   Remaining: Final pregnancy cohort (pregnancies)",
      "",
      
      "6. LOOKBACK EXCLUSION - pregnancies with less than 12 months lookback",
      "   Excluded: No 12mo lookback (persons)",
      "   Excluded: No 12mo lookback (pregnancies)",
      "   Remaining: With >=12mo lookback (persons)",
      "   Remaining: With >=12mo lookback (pregnancies)",
      "",
      "7. ASM EXPOSURE - pregnancies with ASM use",
      "   Excluded: Pregnant persons WITHOUT ASM use",
      "   Excluded: Pregnancies WITHOUT ASM use",
      "   Remaining: ASM Users (persons)",
      "   Remaining: ASM Users (pregnancies)"
    ),
    Count = c(
      "",
      total_population_base_cohort,
      "",
      "",
      female_study_population_without_pregnancy,
      unique_pregnant_persons_in_study_population,
      unique_pregnancies_in_study_population,
      "",
      "",
      pregnancy_end_before_study_start_unique_persons,
      pregnancy_end_before_study_start_unique_pregnancies,
      unique_pregnant_persons_with_preg_start_after_study_start,
      unique_pregnancies_with_preg_start_after_study_start,
      "",
      "",
      pregnancy_start_before_CED_unique_persons,
      pregnancy_start_before_CED_unique_pregnancies,
      unique_persons_after_CED,
      unique_pregnancies_after_CED_dates,
      "",
      "",
      nr_of_pregnancy_starts_after_endfu_unique_persons,
      nr_of_pregnancy_starts_after_endfu_unique_pregnancies,
      unique_pregnant_persons_in_study_population_with_pregstart_before_endfu,
      unique_pregnancies_in_study_population_with_pregstart_before_endfu,
      "",
      "",
      no_info_12_mnths_before_pregnancy_start_unique_persons,
      no_info_12_mnths_before_pregnancy_start_unique_pregnancies,
      unique_pregnant_persons_in_study_population_with_at_least_12_mnths_fu,
      unique_pregnancies_in_study_population_with_at_least_12_mnths_fu,
      "",
      "",
      pregnant_persons_with_no_ASM_use,
      pregnancies_with_no_ASM_use,
      pregnant_persons_with_ASM_use,
      pregnancies_with_ASM_use
    )
  )
  
  # Save table
  saveRDS(flow_data, file.path(paths$D5_dir, "flowcharts", paste0(pop_prefix, "_study_pop_to_Pregnant_ASM_users_flowchart.rds")))
  
} else {
  
  # For EFEMERIS and FINLAND
  #<<< Flow chart >>> #
  unique_pregnant_persons_in_study_population <- uniqueN(study_population$person_id)
  unique_pregnancies_in_study_population      <- uniqueN(study_population$pregnancy_id)
  
  # Merge with dt_exposure to get pregnancy persons with rx a year before pregnacy start or within pregnancy
  pregnancies <- merge(study_population, dt_exposures[, .(pregnancy_id, code, rx_date)], by = "pregnancy_id", allow.cartesian = TRUE)
  
  # Filter for rx that occur between op start (pregnancy start - lookback, and op_end_date/pregnancy_end_date)
  pregnancies <- pregnancies[rx_date >= op_start_date & rx_date < op_end_date]
  
  #<<< Flow chart >>> #
  pregnant_persons_with_ASM_use    <- uniqueN(pregnancies$person_id)
  pregnancies_with_ASM_use         <- uniqueN(pregnancies$pregnancy_id)
  pregnant_persons_with_no_ASM_use <- unique_pregnant_persons_in_study_population - pregnant_persons_with_ASM_use
  pregnancies_with_no_ASM_use      <- unique_pregnancies_in_study_population - pregnancies_with_ASM_use
  flow_data <- data.table(
    step = c(
      "unique_pregnant_persons_in_study_population",
      "unique_pregnancies_in_study_population",
      "pregnant_persons_with_no_ASM_use",
      "pregnant_persons_with_ASM_use",
      "pregnancies_with_no_ASM_use",
      "pregnancies_with_ASM_use"
    ),
    count = c(
      unique_pregnant_persons_in_study_population,
      unique_pregnancies_in_study_population,
      pregnant_persons_with_no_ASM_use,
      pregnant_persons_with_ASM_use,
      pregnancies_with_no_ASM_use,
      pregnancies_with_ASM_use
    )
  )
  
  # Save table
  saveRDS(flow_data, file.path(paths$D5_dir, "flowcharts", paste0(pop_prefix, "_study_pop_to_Pregnant_ASM_users_flowchart.rds")))
}
