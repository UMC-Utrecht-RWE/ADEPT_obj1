print("============================================================================")
print("========================= CREATING BASELINE TABLES =========================")
print("============================================================================")


################################### Individual ATC Codes #######################################

# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "individual"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]}

for (episode in seq_along(files_episodes)) {
  
  # Read the treatment episode file
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", "individual", files_episodes[episode]))
  
  # Print Message
  message("Processing: ", gsub("_treatment_episode\\.rds$", "", files_episodes[episode]))
  
  # Keep only unique persons id
  dt <- unique(dt, by = "person_id")
  
  # 1. Create columns 
  
  # Make sure all dates are IDate
  dt[, (c("birth_date", "start_follow_up", "end_follow_up")) := lapply(.SD, as.IDate), .SDcols = c("birth_date", "start_follow_up", "end_follow_up")]
  
  # Calculate follow-up time in days
  dt[, followup_days := as.numeric(difftime(end_follow_up, start_follow_up, units = "days"))]
  
  # Calculate age at start_follow_up
  dt[, age_at_start_follow_up := floor(as.numeric(difftime(start_follow_up, birth_date, units = "days")) / 365.25)]
  
  # Create age groups
  dt[, age_group := fifelse(age_at_start_follow_up >= 12 & age_at_start_follow_up < 19, "12-18.99",
                            fifelse(age_at_start_follow_up >= 19 & age_at_start_follow_up < 35, "19-34.99",
                                    fifelse(age_at_start_follow_up >= 35 & age_at_start_follow_up < 55, "35-54.99",
                                            fifelse(age_at_start_follow_up >= 55 & age_at_start_follow_up < 75, "55-74.99",
                                                    fifelse(age_at_start_follow_up >= 75, "75+", NA_character_)))))]
  
  
  # 2. summary statistics
  
  # Calculates median of followup in years 
  fu_median      <- median(dt$followup_days)/365.25
  fu_IQR         <- IQR(dt$followup_days)/365.25
  fu_min         <- min(dt$followup_days)/365.25
  fu_max         <- max(dt$followup_days)/365.25
  max_endfu_date <- max(dt$end_follow_up)
  
  # Mean Age
  age_at_start_fu_mean <-mean(dt$age_at_start_follow_up)
  age_at_start_fu_SD   <-sd(dt$age_at_start_follow_up)
  
  # Counts Per Age_Group 
  age_group_12_18.99_count <- sum(dt$age_group == "12-18.99")
  age_group_19_34.99_count <- sum(dt$age_group == "19-34.99")
  age_group_35_54.99_count <- sum(dt$age_group == "35-54.99")
  age_group_55_74.99_count <- sum(dt$age_group == "55-74.99")
  age_group_above_75_count <- sum(dt$age_group == "75+")
  
  # Calculates percentages
  age_group_12_18.99_perc <- (age_group_12_18.99_count/nrow(dt)) * 100
  age_group_19_34.99_perc <- (age_group_19_34.99_count/nrow(dt)) * 100
  age_group_35_54.99_perc <- (age_group_35_54.99_count/nrow(dt)) * 100
  age_group_55_74.99_perc <- (age_group_55_74.99_count/nrow(dt)) * 100
  age_group_above_75_perc <- (age_group_above_75_count/nrow(dt)) * 100

  # Create Baseline Table 
  names <- c("Follow-up, years - median",
             "Follow-up, years - IQR",
             "Follow-up, years - min",
             "Follow-up, years - max",
             
             "Max end-fu date",
             
             "Age at start fu - mean",
             "Age at start fu - sd",
             
             "age_group_12_18.99_count",
             "age_group_12_18.99_perc", 
             "age_group_19_34.99_count",
             "age_group_19_34.99_perc",
             "age_group_35_54.99_count",
             "age_group_35_54.99_perc", 
             "age_group_55_74.99_count",
             "age_group_55_74.99_perc",
             "age_group_above_75_count", 
             "age_group_above_75_perc")
  
  values<-c(as.character(round(fu_median,1)),
            as.character(round(fu_IQR,1)),
            as.character(round(fu_min,2)),
            as.character(round(fu_max,2)),
            
            as.character(max_endfu_date),
            
            as.character(round(age_at_start_fu_mean,1)), 
            as.character(round(age_at_start_fu_SD, 1)),
            
            as.character(age_group_12_18.99_count),
            as.character(round(age_group_12_18.99_perc,1)),
            as.character(age_group_19_34.99_count),
            as.character(round(age_group_19_34.99_perc,1)),
            as.character(age_group_35_54.99_count),
            as.character(round(age_group_35_54.99_perc,1)),
            as.character(age_group_55_74.99_count),
            as.character(round(age_group_55_74.99_perc,1)),
            as.character(age_group_above_75_count),
            as.character(round(age_group_above_75_perc,1))
  )
  
  # Join names and values 
  baseline_table <-data.table(names, values)
  
  # Save baseline table 
  saveRDS(baseline_table, file.path(paths$D5_dir, "baseline_tables", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_baseline_table.rds")))
}


################################### Grouped ATC Codes #######################################


# List all episode files 
files_episodes <- list.files(file.path(paths$D3_dir, "tx_episodes", "groups"), pattern = "\\.rds$")

# Filter exposures for current pop_prefix only
files_episodes <- files_episodes[grepl(paste0("^", pop_prefix, "_"), files_episodes)]

# If pop_prefix is PC, then drop any that are PC_HOSP
if(pop_prefix=="PC"){files_episodes <- files_episodes[!grepl("PC_HOSP", files_episodes)]}

for (episode in seq_along(files_episodes)) {
  
  # Read the treatment episode file
  dt <- readRDS(file.path(paths$D3_dir, "tx_episodes", "groups", files_episodes[episode]))
  
  # Print Message
  message("Processing: ", gsub("_treatment_episode\\.rds$", "", files_episodes[episode]))
  
  # Keep only unique persons id
  dt <- unique(dt, by = "person_id")
  
  # 1. Create columns 
  
  # Make sure all dates are IDate
  dt[, (c("birth_date", "start_follow_up", "end_follow_up")) := lapply(.SD, as.IDate), .SDcols = c("birth_date", "start_follow_up", "end_follow_up")]
  
  # Calculate follow-up time in days
  dt[, followup_days := as.numeric(difftime(end_follow_up, start_follow_up, units = "days"))]
  
  # Calculate age at start_follow_up
  dt[, age_at_start_follow_up := floor(as.numeric(difftime(start_follow_up, birth_date, units = "days")) / 365.25)]
  
  # Create age groups
  dt[, age_group := fifelse(age_at_start_follow_up >= 12 & age_at_start_follow_up < 19, "12-18.99",
                            fifelse(age_at_start_follow_up >= 19 & age_at_start_follow_up < 35, "19-34.99",
                                    fifelse(age_at_start_follow_up >= 35 & age_at_start_follow_up < 55, "35-54.99",
                                            fifelse(age_at_start_follow_up >= 55 & age_at_start_follow_up < 75, "55-74.99",
                                                    fifelse(age_at_start_follow_up >= 75, "75+", NA_character_)))))]
  
  
  # 2. summary statistics
  
  # Calculates median of followup in years 
  fu_median      <- median(dt$followup_days)/365.25
  fu_IQR         <- IQR(dt$followup_days)/365.25
  fu_min         <- min(dt$followup_days)/365.25
  fu_max         <- max(dt$followup_days)/365.25
  max_endfu_date <- max(dt$end_follow_up)
  
  # Mean Age
  age_at_start_fu_mean <-mean(dt$age_at_start_follow_up)
  age_at_start_fu_SD   <-sd(dt$age_at_start_follow_up)
  
  # Counts Per Age_Group - Sex
  
  
  # Counts Per Age_Group 
  age_group_12_18.99_count <- sum(dt$age_group == "12-18.99")
  age_group_19_34.99_count <- sum(dt$age_group == "19-34.99")
  age_group_35_54.99_count <- sum(dt$age_group == "35-54.99")
  age_group_55_74.99_count <- sum(dt$age_group == "55-74.99")
  age_group_above_75_count <- sum(dt$age_group == "75+")
  
  # Calculates percentages
  age_group_12_18.99_perc <- (age_group_12_18.99_count/nrow(dt)) * 100
  age_group_19_34.99_perc <- (age_group_19_34.99_count/nrow(dt)) * 100
  age_group_35_54.99_perc <- (age_group_35_54.99_count/nrow(dt)) * 100
  age_group_55_74.99_perc <- (age_group_55_74.99_count/nrow(dt)) * 100
  age_group_above_75_perc <- (age_group_above_75_count/nrow(dt)) * 100
  
  # Create Baseline Table 
  names <- c("Follow-up, years - median",
             "Follow-up, years - IQR",
             "Follow-up, years - min",
             "Follow-up, years - max",
             
             "Max end-fu date",
             
             "Age at start fu - mean",
             "Age at start fu - sd",
             
             "age_group_12_18.99_count",
             "age_group_12_18.99_perc", 
             "age_group_19_34.99_count",
             "age_group_19_34.99_perc",
             "age_group_35_54.99_count",
             "age_group_35_54.99_perc", 
             "age_group_55_74.99_count",
             "age_group_55_74.99_perc",
             "age_group_above_75_count", 
             "age_group_above_75_perc")
  
  values<-c(as.character(round(fu_median,1)),
            as.character(round(fu_IQR,1)),
            as.character(round(fu_min,2)),
            as.character(round(fu_max,2)),
            
            as.character(max_endfu_date),
            
            as.character(round(age_at_start_fu_mean,1)), 
            as.character(round(age_at_start_fu_SD, 1)),
            
            as.character(age_group_12_18.99_count),
            as.character(round(age_group_12_18.99_perc,1)),
            as.character(age_group_19_34.99_count),
            as.character(round(age_group_19_34.99_perc,1)),
            as.character(age_group_35_54.99_count),
            as.character(round(age_group_35_54.99_perc,1)),
            as.character(age_group_55_74.99_count),
            as.character(round(age_group_55_74.99_perc,1)),
            as.character(age_group_above_75_count),
            as.character(round(age_group_above_75_perc,1))
  )
  
  # Join names and values 
  baseline_table <-data.table(names, values)
  
  # Save baseline table 
  saveRDS(baseline_table, file.path(paths$D5_dir, "baseline_tables", paste0(gsub("_treatment_episode\\.rds$", "", files_episodes[episode]), "_baseline_table.rds")))
}


######################### Study Population #########################
# Calculate Stats for Overall Population
# Make sure all dates are IDate
study_population[, (c("birth_date", "start_follow_up", "end_follow_up")) := lapply(.SD, as.IDate), .SDcols = c("birth_date", "start_follow_up", "end_follow_up")]

# Calculate follow-up time in days
study_population[, followup_days := as.numeric(difftime(end_follow_up, start_follow_up, units = "days"))]

# Calculate age at start_follow_up
study_population[, age_at_start_follow_up := floor(as.numeric(difftime(start_follow_up, birth_date, units = "days")) / 365.25)]

# Create age groups
study_population[, age_group := fifelse(age_at_start_follow_up >= 12 & age_at_start_follow_up < 19, "12-18.99",
                                        fifelse(age_at_start_follow_up >= 19 & age_at_start_follow_up < 35, "19-34.99",
                                                fifelse(age_at_start_follow_up >= 35 & age_at_start_follow_up < 55, "35-54.99",
                                                        fifelse(age_at_start_follow_up >= 55 & age_at_start_follow_up < 75, "55-74.99",
                                                                fifelse(age_at_start_follow_up >= 75, "75+", NA_character_)))))]


# 2. summary statistics

# Calculates median of followup in years 
fu_median      <- median(study_population$followup_days)/365.25
fu_IQR         <- IQR(study_population$followup_days)/365.25
fu_min         <- min(study_population$followup_days)/365.25
fu_max         <- max(study_population$followup_days)/365.25
max_endfu_date <- max(study_population$end_follow_up)

# Mean Age
age_at_start_fu_mean <-mean(study_population$age_at_start_follow_up)
age_at_start_fu_SD   <-sd(study_population$age_at_start_follow_up)

# Counts Per Age_Group 
age_group_12_18.99_count <- sum(study_population$age_group == "12-18.99")
age_group_19_34.99_count <- sum(study_population$age_group == "19-34.99")
age_group_35_54.99_count <- sum(study_population$age_group == "35-54.99")
age_group_55_74.99_count <- sum(study_population$age_group == "55-74.99")
age_group_above_75_count <- sum(study_population$age_group == "75+")

# Calculates percentages
age_group_12_18.99_perc <- (age_group_12_18.99_count/nrow(study_population)) * 100
age_group_19_34.99_perc <- (age_group_19_34.99_count/nrow(study_population)) * 100
age_group_35_54.99_perc <- (age_group_35_54.99_count/nrow(study_population)) * 100
age_group_55_74.99_perc <- (age_group_55_74.99_count/nrow(study_population)) * 100
age_group_above_75_perc <- (age_group_above_75_count/nrow(study_population)) * 100

# Create Baseline Table 
# Create Baseline Table 
names <- c("Follow-up, years - median",
           "Follow-up, years - IQR",
           "Follow-up, years - min",
           "Follow-up, years - max",
           
           "Max end-fu date",
           
           "Age at start fu - mean",
           "Age at start fu - sd",
           
           "age_group_12_18.99_count",
           "age_group_12_18.99_perc", 
           "age_group_19_34.99_count",
           "age_group_19_34.99_perc",
           "age_group_35_54.99_count",
           "age_group_35_54.99_perc", 
           "age_group_55_74.99_count",
           "age_group_55_74.99_perc",
           "age_group_above_75_count", 
           "age_group_above_75_perc")

values<-c(as.character(round(fu_median,1)),
          as.character(round(fu_IQR,1)),
          as.character(round(fu_min,2)),
          as.character(round(fu_max,2)),
          
          as.character(max_endfu_date),
          
          as.character(round(age_at_start_fu_mean,1)), 
          as.character(round(age_at_start_fu_SD, 1)),
          
          as.character(age_group_12_18.99_count),
          as.character(round(age_group_12_18.99_perc,1)),
          as.character(age_group_19_34.99_count),
          as.character(round(age_group_19_34.99_perc,1)),
          as.character(age_group_35_54.99_count),
          as.character(round(age_group_35_54.99_perc,1)),
          as.character(age_group_55_74.99_count),
          as.character(round(age_group_55_74.99_perc,1)),
          as.character(age_group_above_75_count),
          as.character(round(age_group_above_75_perc,1))
)

# Join names and values 
baseline_table <-data.table(names, values)

# Save baseline table 
saveRDS(baseline_table, file.path(paths$D5_dir, "baseline_tables", paste0(pop_prefix, "_study_population_baseline_table.rds")))











