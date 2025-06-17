
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021



if(SUBP) SCHEME_06 <- subpopulation_meanings[, ':=' (file_in = paste0(subpopulations,"_study_population.rds"), 
                                                     file_out = paste0(subpopulations,"_study_population.rds"), 
                                                     folder_in = file.path(paths$D3_dir, "study_population"), 
                                                     folder_out = file.path(paths$D3_dir, "study_population")) ]

if(!SUBP) SCHEME_06 <- data.frame(subpopulations = c("ALL"), 
                                  file_in = "ALL_study_population.rds", 
                                  file_out = "ALL_study_population.rds", 
                                  folder_in = file.path(paths$D3_dir, "study_population"), 
                                  folder_out = file.path(paths$D3_dir, "study_population"))

# SCHEME_06$nrows <- as.integer(NA)
# SCHEME_06$ncols <- as.integer(NA)
# SCHEME_06$ncolsneeded <- 23
# 
# SCHEME_06$nrows_CPT <- as.integer(NA)
# SCHEME_06$ncols_CPT <- as.integer(NA)
# SCHEME_06$ncolsneeded_CPT <- 23


Analyse_dates <- c("start_follow_up", "end_follow_up", "birth_date")

for(i in 1:nrow(SCHEME_06)){
  
  print(paste0("Read Study population table for population ",SCHEME_06[["subpopulations"]][i]," from intermediate"))
  study_population <- readRDS(paste0(SCHEME_06[["folder_in"]][i], "/", SCHEME_06[["file_in"]][i]))
  
  #Create new Columns here
  ###################################################################################
  
  print("Set date variables to day, month and year")
  
  for(j in Analyse_dates){
    study_population <- study_population[,  paste0(j,"_month") := month(get(j))]
    study_population <- study_population[,  paste0(j,"_year") := year(get(j))]
    study_population <- study_population[,  paste0(j,"_day") := day(get(j))]
  }
  
  ###
  
  T0 <- "birth_date"
  T1 <- "op_start_date"
  T2 <- "op_end_date"
  
  vars <- c("person_id",T0,T1,T2)
  
  print(paste0("Calculate time differences between ",T0,"/",T1,"/",T2))
  
  study_population <- study_population[ , diff_T1_T0_W := floor((get(T1) - get(T0)) / 7)] # days
  study_population <- study_population[ , diff_T2_T1_M := floor((get(T2) - get(T1)) / 30.4)] # weeks
  study_population <- study_population[ , PY := round((end_follow_up - start_follow_up) / 365.25, 2)]
  study_population <- study_population[ , year_op_start := year(op_start_date)]
  study_population <- study_population[ , year_start_fu := year(start_follow_up)]
  
  print(paste0("Write Study population table for population ",SCHEME_06[["subpopulations"]][i]," to intermediate"))

  saveRDS(study_population,file = paste0(SCHEME_06[["folder_out"]][i], "/", SCHEME_06[["file_out"]][i]))
}

saveRDS(SCHEME_06, file = file.path(paths$D5_dir, "flowcharts", "scheme_06.rds"))