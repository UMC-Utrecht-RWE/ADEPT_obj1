# Pregnancy Attrition Table 

# << Total population (base cohort) >> #
total_population_base_cohort <- uniqueN(study_population$person_id)

# For Attrition Table
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS") # subgroups for exclusion

files_exposures <- list.files(file.path(paths$D3_dir, "exposure"), pattern = "\\.rds$", full.names = FALSE) #list files in exposure folder
files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)] # keeo files of current pop prefix
if (pop_prefix == "PC") files_exposures <- files_exposures[!grepl("PC_HOSP", files_exposures)] #BIFAP
files_exposures <- files_exposures[!grepl(paste(exclude, collapse = "|"), files_exposures)] # Exclude subgroups
dt_exposures <- as.data.table(rbindlist(lapply(file.path(paths$D3_dir, "exposure", files_exposures), readRDS),fill = TRUE)) # read and bind datasets
dt_exposures <- unique(dt_exposures) # remove true duplicates 

# Load pregnancies file
load(file.path(preg_dir, "D3_pregnancy_final.RData"))
pregnancies <- as.data.table(D3_pregnancy_final)

# Remove duplicates
pregnancies <- unique(pregnancies)

# Convert pregnancy dates to IDate
pregnancies[, pregnancy_start_date := as.IDate(pregnancy_start_date)]
pregnancies[, pregnancy_end_date   := as.IDate(pregnancy_end_date)]

# Merge pregnancies with study population to get start and end follow up. We want to keep only pregnancy starts within this period
pregnancies <- merge(pregnancies, study_population[, .(person_id, start_follow_up, end_follow_up, entry_date, exit_date)], by = "person_id", allow.cartesian = TRUE)

#<<< flow chart >>> #
female_study_population_without_pregnancy <- uniqueN(study_population$person_id) - uniqueN(pregnancies$person_id)
no_info_12_mnths_before_pregnancy_start <- uniqueN(pregnancies[pregnancy_start_date < start_follow_up,]$person_id)

# Get Pregnancies with at least a year of lookback - pregnancy start needs to be equal or after startfu
pregnancies <- pregnancies[pregnancy_start_date >= start_follow_up,]

#<<< flow chart >>> #
unique_pregnant_persons_in_study_population <- uniqueN(pregnancies$person_id)
unique_pregnancies_in_study_population <- uniqueN(pregnancies$pregnancy_id)

# Merge with dt_exposure to get pregnancy persons with rx a year before pregnacy start or within pregnancy
pregnancies <- merge(pregnancies, dt_exposures[, .(person_id, code, rx_date)], by = "person_id", allow.cartesian = TRUE)
# filter for pregnancies that occur within 1 year of pregnancy start or during pregnancy
pregnancies <- pregnancies[rx_date >=as.IDate(as.Date(pregnancy_start_date) - lookback_period) & rx_date<pregnancy_end_date]

#<<< flow chart >>> #
pregnant_persons_with_ASM_use <- uniqueN(pregnancies$person_id)
pregnancies_with_ASM_use <- uniqueN(pregnancies$pregnancy_id)
pregnant_persons_with_no_ASM_use <- unique_pregnant_persons_in_study_population - pregnant_persons_with_ASM_use

flow_data <- data.table(
  step = c(
    "total_FEMALE_study_population_base_cohort",
    "female_study_population_without_pregnancy",
    "no_info_12_mnths_before_pregnancy_start",
    "unique_pregnant_persons_in_study_population",
    "unique_pregnancies_in_study_population",
    "pregnant_persons_with_no_ASM_use",
    "pregnant_persons_with_ASM_use",
    "pregnancies_with_ASM_use"
  ),
  count = c(
    total_population_base_cohort,
    female_study_population_without_pregnancy, 
    no_info_12_mnths_before_pregnancy_start,
    unique_pregnant_persons_in_study_population,
    unique_pregnancies_in_study_population,
    pregnant_persons_with_no_ASM_use,
    pregnant_persons_with_ASM_use,
    pregnancies_with_ASM_use
  )
)

# save table 
saveRDS(flow_data, file.path(paths$D5_dir,"flowcharts" ,paste0(pop_prefix, "_study_pop_to_Pregnant_ASM_users_flowchart.rds")))

