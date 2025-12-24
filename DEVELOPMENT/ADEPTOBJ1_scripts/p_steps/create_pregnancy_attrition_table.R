print("=============================================================================")
print("========================= PREGNANCY ATTRITION TABLE =========================")
print("=============================================================================")

# List exposure files to exclude
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# List exposure files
files_exposures <- list.files(file.path(paths$D3_dir, "exposure"), pattern = "\\.rds$", full.names = FALSE) 
files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)] # keep files of current pop prefix
files_exposures <- files_exposures[!grepl(paste(exclude, collapse = "|"), files_exposures)] # Exclude subgroups
files_exposures <- files_exposures[grepl("_F_", files_exposures, fixed = TRUE)] # Keep female records only 

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
  
  # Exclude pregnancies that  do not have at least a year of lookback -> pregnancy start needs to be equal or after startfu
  # Get Pregnancies with at least a year of lookback - pregnancy start needs to be equal or after startfu
  #<<< flow chart >>> #
  pregnancies_wo_12_mnths_before_pregnancy_start <- pregnancies[pregnancy_start_date < start_follow_up, ] # pregnancies
  no_info_12_mnths_before_pregnancy_start <- uniqueN(pregnancies_wo_12_mnths_before_pregnancy_start$person_id) # unique pregnancy persons wo 12 months info prior
  
  # Exclude records where no lookback of at least a year
  pregnancies <- pregnancies[pregnancy_start_date >= start_follow_up, ]
  
  # Population with pregnancy and at least 12 months of fu
  unique_pregnant_persons_in_study_population_with_at_least_12_mnths_fu <- uniqueN(pregnancies$person_id)
  unique_pregnancies_in_study_population_with_at_least_12_mnths_fu <- uniqueN(pregnancies$pregnancy_id)
  
  # Merge with dt_exposure to get pregnancy persons with rx a year before pregnancy start or within pregnancy
  pregnancies <- merge(pregnancies, dt_exposures[, .(person_id, code, rx_date)], by = "person_id", allow.cartesian = TRUE)
  
  # Prescriptions that occur within 1 year of pregnancy start or during pregnancy
  pregnancies <- pregnancies[rx_date >= as.IDate(as.Date(pregnancy_start_date) - lookback_period) & rx_date < pregnancy_end_date]
  
  #<<< Flow chart >>> #
  pregnant_persons_with_ASM_use <- uniqueN(pregnancies$person_id)
  pregnancies_with_ASM_use <- uniqueN(pregnancies$pregnancy_id)
  pregnant_persons_with_no_ASM_use <- unique_pregnant_persons_in_study_population_with_at_least_12_mnths_fu - pregnant_persons_with_ASM_use
  flow_data <- data.table(
    step = c(
      "total_FEMALE_study_population_base_cohort",
      "female_study_population_without_pregnancy",
      "unique_pregnant_persons_in_study_population",
      "unique_pregnancies_in_study_population",
      "no_info_12_mnths_before_pregnancy_start",
      "unique_pregnant_persons_in_study_population_with_at_least_12_mnths_fu",
      "unique_pregnancies_in_study_population_with_at_least_12_mnths_fu",
      "pregnant_persons_with_no_ASM_use",
      "pregnant_persons_with_ASM_use",
      "pregnancies_with_ASM_use"
    ),
    count = c(
      total_population_base_cohort,
      female_study_population_without_pregnancy,
      unique_pregnant_persons_in_study_population,
      unique_pregnancies_in_study_population,
      no_info_12_mnths_before_pregnancy_start,
      unique_pregnant_persons_in_study_population_with_at_least_12_mnths_fu,
      unique_pregnancies_in_study_population_with_at_least_12_mnths_fu,
      pregnant_persons_with_no_ASM_use,
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
