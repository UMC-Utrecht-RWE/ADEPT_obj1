##############################################################
############### DAP SPECIFIC ASSUMED DURATIONS ###############
##############################################################
### DEFAULT ###
dt[, assumed_duration := ifelse(is.na(presc_duration_days) | presc_duration_days < 30, 30, presc_duration_days)]

#<<< BIFAP >>>#
# 1. presc_duration_days (based on some local algorithm).
if(deap_flags$is_BIFAP){
  
  dt[, assumed_duration := as.numeric(presc_duration_days)]
  # If any NA's or value of 0, we take the value of 30 days 
  dt[is.na(assumed_duration) | assumed_duration <= 0 | is.infinite(assumed_duration), assumed_duration := 30]
  
}


#<<< CPRD >>>#

if(deap_flags$is_CPRD){
  
  ################################################################################################################
  # Step 1. Use disp_number_medicinal_product/presc_quantity_per_day
  ## a. If presc_quantity_per_day was -0-, replace it with 0.5 (an as-needed regimen).
  dt[presc_quantity_per_day==0, presc_quantity_per_day := 0.5]
  
  ################################################################################################################
  # Step 1. Use disp_number_medicinal_product/presc_quantity_per_day
  # .	If presc_quantity_per_day is missing, replace it with a median value of all records (2 for older, newer AEDs and Gabas, and 1 for Benzos).
  
  # read in the subgroups to get median values
  antiepinew <- readRDS(file.path(paths$D3_dir, "exposure", paste0(pop_prefix, "_DP_ANTIEPINEW_algo_med.rds")))
  antiepiold <- readRDS(file.path(paths$D3_dir, "exposure", paste0(pop_prefix, "_DP_ANTIEPIOLD_algo_med.rds")))
  benzos     <- readRDS(file.path(paths$D3_dir, "exposure", paste0(pop_prefix, "_DP_BENZOANTIEPILEPTIC_algo_med.rds")))
  gabas      <- readRDS(file.path(paths$D3_dir, "exposure", paste0(pop_prefix, "_DP_GABAPENTINOIDS_algo_med.rds")))
  
  # build lookup table with medians per subgroup
  median_lookup <- data.table(
    subgroup = c("DP_ANTIEPINEW", 
                 "DP_ANTIEPIOLD", 
                 "DP_BENZOANTIEPILEPTIC", 
                 "DP_GABAPENTINOIDS"),
    median_presc_quantity_per_day   = c(median(antiepinew$presc_quantity_per_day,  na.rm = TRUE),
                                        median(antiepiold$presc_quantity_per_day,  na.rm = TRUE),
                                        median(benzos$presc_quantity_per_day,      na.rm = TRUE),
                                        median(gabas$presc_quantity_per_day,       na.rm = TRUE))
  )
  
  
  # map each algorithm's medicines to its subgroup
  antiepinew_meds <- algorithm_map[Algorithm == "DP_ANTIEPINEW", VariableName]
  antiepiold_meds <- algorithm_map[Algorithm == "DP_ANTIEPIOLD", VariableName]
  benzo_meds      <- algorithm_map[Algorithm == "DP_BENZOANTIEPILEPTIC", VariableName]
  gaba_meds       <- algorithm_map[Algorithm == "DP_GABAPENTINOIDS", VariableName]
  
  meds_map <- rbindlist(list(
    data.table(VariableName = antiepinew_meds, subgroup = "DP_ANTIEPINEW"),
    data.table(VariableName = antiepiold_meds, subgroup = "DP_ANTIEPIOLD"),
    data.table(VariableName = benzo_meds,      subgroup = "DP_BENZOANTIEPILEPTIC"),
    data.table(VariableName = gaba_meds,       subgroup = "DP_GABAPENTINOIDS")
  ))
  
  # merge lookup so every med (and subgroup itself) has a median
  meds_map <- merge(meds_map, median_lookup, by = "subgroup", all.x = TRUE)
  # merge with dt
  dt <- merge(dt, meds_map[, .(VariableName, median_presc_quantity_per_day)], by.x = "Varname", by.y = "VariableName", all.x = TRUE)
  # replace only if missing or if value greater than 5
  dt[is.na(presc_quantity_per_day) | presc_quantity_per_day > 5, presc_quantity_per_day := median_presc_quantity_per_day]
  # drop helper col if not needed
  dt[,median_presc_quantity_per_day:=NULL]
  
  # We need to cap disp_number_medicinal_product for old, new and gabas to 112; for benzos to 56
  dt[Varname %in% c(antiepinew_meds, antiepiold_meds, gaba_meds) & disp_number_medicinal_product > 112, disp_number_medicinal_product := 112]
  dt[Varname %in% benzo_meds & disp_number_medicinal_product > 56, disp_number_medicinal_product := 56]
  
  ################################################################################################################
  # Step 1. Use disp_number_medicinal_product/presc_quantity_per_day
  # If disp_number_medicinal_product (qty) was -0- or missing, go to step 2.
  dt[!is.na(disp_number_medicinal_product) & disp_number_medicinal_product > 0, assumed_duration := as.numeric(as.numeric(disp_number_medicinal_product)/as.numeric(presc_quantity_per_day))]
  
  ################################################################################################################
  # Step 2. Use presc_duration_days;
  dt[is.na(assumed_duration) & !is.na(presc_duration_days) & presc_duration_days > 0 & presc_duration_days < 57, assumed_duration := presc_duration_days]
  
  ################################################################################################################
  # If any assumed duration is still missing or less than 1 or infinite, then use 30 days
  dt[is.na(assumed_duration) | assumed_duration < 1 | is.infinite(assumed_duration), assumed_duration := 30]
  
}


#<<< EFEMERIS >>>#
# 1. fixed duration of 30 days.
if(deap_flags$is_EFEMERIS) dt[, assumed_duration:=30]


#<<< FIN REG >>>#
# 1. presc_duration_days (based on DDDs);
# 2. fixed duration of 90 days unless Benzos then 30.
if(deap_flags$is_FIN_REG){  
  
  dt[, assumed_duration := as.numeric(presc_duration_days)]
  
  if(atc_group %in% c("DP_BENZOANTIEPILEPTIC", "DP_CLOBAZAM", "DP_CLONAZEPAM", "DP_DIAZEPAM","DP_LORAZEPAM", "DP_MIDAZOLAM")){
    dt[is.na(assumed_duration) | assumed_duration <= 0 | is.infinite(assumed_duration), assumed_duration := 30]
  } else {
    dt[is.na(assumed_duration) | assumed_duration <= 0 | is.infinite(assumed_duration), assumed_duration := 90]
  }
}


#<<< NORWAY >>>#
# 1. presc_duration_days (based on DDDs);
# 2. fixed duration of 90 days unless Benzos then 30.
if(deap_flags$is_NOR_REG){
  
  dt[, assumed_duration := as.numeric(presc_duration_days)]
  
  if(atc_group %in% c("DP_BENZOANTIEPILEPTIC", "DP_CLOBAZAM", "DP_CLONAZEPAM", "DP_DIAZEPAM","DP_LORAZEPAM", "DP_MIDAZOLAM")){
    dt[is.na(assumed_duration) | assumed_duration <= 0 | is.infinite(assumed_duration), assumed_duration := 30]
  } else {
    dt[is.na(assumed_duration) | assumed_duration <= 0 | is.infinite(assumed_duration), assumed_duration := 90]
  }
}


#<<< PHARMO >>>#
# 1. fixed duration of 30 days.
if(deap_flags$is_PHARMO) dt[, assumed_duration:=30]



#<<< SIDIAP >>>#
# 1. presc_duration_days;
# 2. fixed duration of 30 days.
if(deap_flags$is_SIDIAP){
  
  dt[, assumed_duration := as.numeric(presc_duration_days)]
  
  # If any NA's or value of 0, we take the value of 30 days 
  dt[is.na(assumed_duration) | assumed_duration <= 0 | is.infinite(assumed_duration), assumed_duration := 30]
  
}

# 1. presc_duration_days;
# 2. fixed duration of 30 days.
# <<< VAL PADANA >>>#
if(deap_flags$is_VAL_PAD){
  
  dt[, assumed_duration := as.numeric(presc_duration_days)]
  # If any NA's or value of 0, we take the value of 30 days 
  dt[is.na(assumed_duration) | assumed_duration <= 0 | is.infinite(assumed_duration), assumed_duration := 30]
}


#<<< FISABIO >>>#
# 1. presc_duration_days (based on some local algorithm).
if(deap_flags$is_VID){
  
  dt[, assumed_duration := as.numeric(presc_duration_days)]
  # If any NA's or value of 0, we take the value of 30 days 
  dt[is.na(assumed_duration) | assumed_duration <= 0 | is.infinite(assumed_duration), assumed_duration := 30]
}


