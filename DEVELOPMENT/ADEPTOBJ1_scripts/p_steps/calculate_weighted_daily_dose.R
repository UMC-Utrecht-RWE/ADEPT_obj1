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

# Get lists of all needed files and load them 
# pre-pregnancy continuous users
continuous_list <- list.files(file.path(paths$D4_dir, "1.3_pregnancy_continuous"), full.names = TRUE) 
continuous_list <- continuous_list[grepl(paste0("^", pop_prefix, "_"), basename(continuous_list))]# keep current pop prefix
continuous_list <- continuous_list[!grepl(paste(exclude, collapse = "|"), basename(continuous_list))] # exclude groups
dt_continuous   <- rbindlist(lapply(continuous_list, readRDS), use.names = TRUE, fill = TRUE) # read in and bind all files 
# pre-pregnancy discontinuers
discontinued_list <- list.files(file.path(paths$D4_dir, "1.4_pregnancy_discontinuation"), full.names = TRUE)
discontinued_list <- discontinued_list[grepl(paste0("^", pop_prefix, "_"), basename(discontinued_list))] # keep current pop prefix
discontinued_list <- discontinued_list[grepl("t2", basename(discontinued_list), ignore.case = TRUE)] # keep only late discontinuers
discontinued_list <- discontinued_list[!grepl(paste(exclude, collapse = "|"), basename(discontinued_list))] # exclude groups
dt_discontinued   <- rbindlist(lapply(discontinued_list, readRDS), use.names = TRUE, fill = TRUE) # read in and bind all files 
# products table 
products_list <- list.files(file.path(CDM_dir), pattern = "^PRODUCTS", full.names = TRUE)
dt_products <- rbindlist(lapply(products_list, fread), use.names = TRUE, fill = TRUE)

# Clean up continuer and late discontinuer files
dt_continuous <- dt_continuous[,.(person_id, episode.start, episode.duration, episode.end, atc_group, code, pregnancy_start_date, pregnancy_end_date, sex_at_instance_creation, birth_date)]
dt_discontinued <- dt_discontinued[,.(person_id, episode.start, episode.duration, episode.end, atc_group, code, pregnancy_start_date, pregnancy_end_date, sex_at_instance_creation, birth_date)]

# add column to say if continuous or discontinued
dt_continuous[,type:= "continuous user"]
dt_discontinued[,type:="late discontinuer"]
# Bind the two datasets 
dt_all <- rbind(dt_continuous, dt_discontinued,use.names = TRUE, fill = TRUE)

exposure_files <- list.files(file.path(paths$D3_dir, "exposure"))
# Merge only with the same exposure
merged_list <- list()

for (atc in unique(dt_all$atc_group)) {
  
  # Find matching exposure file
  file_match <- exposure_files[grepl(paste0("^", pop_prefix, "_F_", atc, "\\.rds$"), exposure_files)]
  
  # Read exposure data
  dt_exp <- readRDS(file.path(paths$D3_dir, "exposure", file_match))
  dt_exp <- unique(dt_exp)
  # Convert date
  dt_all[,window_start:= as.IDate(episode.start)]
  dt_all[,window_end := as.IDate(episode.end)]
  dt_exp[,event_start:=as.IDate(rx_date)]
  dt_exp[,event_end:=as.IDate(rx_date)]
  
  # Reorder columns in dt_all so first three are person_id, window_start, window_end
  setcolorder(dt_all, c("person_id", "window_start", "window_end", setdiff(names(dt_all), c("person_id", "window_start", "window_end"))))
  
  # Key both tables by the interval columns
  setkey(dt_all, person_id, window_start, window_end)
  setkey(dt_exp, person_id, event_start, event_end)
  
  merged <- foverlaps(dt_exp[,.(person_id, event_start,event_end, rx_date, medicinal_product_id, disp_number_medicinal_product, presc_quantity_per_day, presc_quantity_unit, presc_duration_days)], 
                      dt_all, 
                         by.x = c("person_id", "event_start", "event_end"), 
                         by.y = c("person_id", "window_start", "window_end"), 
                         nomatch = 0)  # only keep overlaps
  
  merged_list[[atc]] <- merged
}

# Combine all merged ATC groups
final_dt <- rbindlist(merged_list, use.names = TRUE, fill = TRUE)

# Drop cols
final_dt[, c("window_start", "window_end", "event_start", "event_end", "sex_at_instance_creation", "birth_date") := NULL]

final_dt <- unique(final_dt, by = c("person_id", "atc_group", "rx_date", "pregnancy_start_date", "pregnancy_end_date"))

setorder(final_dt, person_id, atc_group, rx_date)
final_dt[, rx_start := rx_date]
final_dt[, rx_end := shift(rx_date, type = "lead")-1, by = .(person_id, atc_group)]
final_dt[is.na(rx_end), rx_end := episode.end]

# Add windows
final_dt[, t0_start := pregnancy_start_date-90]
final_dt[, t0_end   := pregnancy_start_date-1]
final_dt[, t1_start := pregnancy_start_date]
final_dt[, t1_end   := pmin(pregnancy_start_date + 90, pregnancy_end_date)]
final_dt[, t2_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 91, pregnancy_start_date + 91, as.IDate(NA))]
final_dt[, t2_end   := fifelse(!is.na(t2_start), pmin(pregnancy_start_date + 180, pregnancy_end_date), as.IDate(NA))]
final_dt[, t3_start := fifelse(pregnancy_end_date >= pregnancy_start_date + 181, pregnancy_start_date + 181, as.IDate(NA))]
final_dt[, t3_end   := fifelse(!is.na(t3_start), pregnancy_end_date, as.IDate(NA))]


# compute overlap days between two intervals
overlap_days <- function(start1, end1, start2, end2) fifelse(!is.na(start2) & !is.na(end2), pmax(0L, as.integer(pmin(end1, end2) - pmax(start1, start2) + 1L)), 0L)


# Apply for each period
final_dt[, t0 := overlap_days(rx_start, rx_end, t0_start, t0_end)]
final_dt[, t1 := overlap_days(rx_start, rx_end, t1_start, t1_end)]
final_dt[, t2 := overlap_days(rx_start, rx_end, t2_start, t2_end)]
final_dt[, t3 := overlap_days(rx_start, rx_end, t3_start, t3_end)]

# remove rows where all cols are 0
final_dt <- final_dt[!(t0 == 0 & t1 == 0 & t2 == 0 & t3 == 0)]

calculate period lengths (inclusive of both ends)
final_dt[, t0_len := as.integer(t0_end - t0_start + 1)]
final_dt[, t1_len := as.integer(t1_end - t1_start + 1)]
final_dt[, t2_len := fifelse(!is.na(t2_start) & !is.na(t2_end), as.integer(t2_end - t2_start + 1), NA_integer_)]
final_dt[, t3_len := fifelse(!is.na(t3_start) & !is.na(t3_end), as.integer(t3_end - t3_start + 1), NA_integer_)]


# final_dt[, t0_pct := 100 * t0 / t0_len]
# final_dt[, t1_pct := 100 * t1 / t1_len]
# final_dt[, t2_pct := fifelse(!is.na(t2_len), 100 * t2 / t2_len, NA_real_)]
# final_dt[, t3_pct := fifelse(!is.na(t3_len), 100 * t3 / t3_len, NA_real_)]


# Merge with products table - keep all rows in rx_in overlap
setDT(final_dt)
setDT(dt_products)

# Left join: keep all rows from rx_in_overlap
final_dt <- merge(final_dt, dt_products, by = "medicinal_product_id")

final_dt[,dur:= disp_number_medicinal_product/presc_quantity_per_day] #assumed duration
# if 0 , replace by 0.5 
final_dt[,prop_t0:=t0/dur]
final_dt[,amount_t0:=prop_t0*disp_number_medicinal_product]
# consider unit column
final_dt[,strength_t0:=amount_t0*subst1_amount_per_form]#check for unit - if gram check amount *1000
#what if syrup?
final_dt[, strength_t0_sum := sum(strength_t0), by = .(person_id, pregnancy_start_date)]


final_dt[, daily_dose_t0:=strength_t0_sum/t0_len]
final_dt[code=="N03AG01", dose_DDD_t0:=daily_dose_t0/1500]# for 8 DDD
final_dt[dose_DDD_t0>0.01 & dose_DDD_t0<=0.49, dose_group_t0:= "low"]
final_dt[dose_DDD_t0>0.49 & dose_DDD_t0< 1.5, dose_group_t0:= "mid"]
final_dt[dose_DDD_t0>1.5, dose_group_t0:= "high"]
final_dt[is.na(dose_DDD_t0) := "missing"] #or above cap - for Shahab
final_dt[dose_DDD_t0 <=0.01| dose_DDD_t0 > 5:= "invalid"] #or above cap - for Shahab
# counts is 

# make unique by patient, pregnacy, start
# TODO low_strength = nrow== low/total number of rows in final 

# create column with ranges per ATC in DDD
# sum cols per person_id/pregancy then divide by leng
saveRDS(final_dt,"test_rx_in_prods.rds" )
