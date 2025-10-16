# libraries 
library(data.table)

# find all files matching "_indication_counts.rds" recursively
ind_files <- list.files("C:/Users/mgamb/Desktop/D5_results", pattern = "_indication_counts\\.rds$", recursive = TRUE, full.names = TRUE)

# exclude all files found in the baseline_tables folder and in the corrected_indication folder
ind_files <- ind_files[!grepl("/baseline_tables/|/corrected_indications/|/backup_originals/", ind_files, ignore.case = TRUE)]

# loop over each indication file
for (ind in seq_along(ind_files)) {

  ##################################################
  # read in the indication file
  dt_ind <- readRDS(ind_files[ind])

  ##################################################
  # find the matching denominator file 
  
  # identify the parent folder in which we look for the file with the denominator value (one level up from stratified)
  parent_dir <- dirname(dirname(ind_files[ind])) 

  # get file base name 
  file_base <- basename(ind_files[ind])

  # extract prefix from file name - to be able to match with denominator file
  # different for polytherapy files so we need to get the prefix conditionally
  if (grepl("polytherapy_indication_counts\\.rds$", file_base)) {
    # polytherapy files
    prefix <- sub("polytherapy_indication_counts\\.rds$", "OVERALL_polytherapy", file_base)
    
  } else {
    # regular files
    prefix <- sub("_indication_counts\\.rds$", "", file_base)
    
  }
  
  # build the full denominator path
  denom_file <- file.path(parent_dir, paste0(prefix, "_counts.rds"))
  
  # sanity check - does file exist?
  if (!file.exists(denom_file)) stop("Denominator file not found for ", prefix)
  
  # read in denominator file 
  dt_denom <- readRDS(denom_file)
  
  # prepare denominator file for merging 
  # if the file is pregnancy related, the column year is actually preg_year so that needs to be changed
  if (grepl("pregnancy", ind_files[ind], ignore.case = TRUE)) dt_denom[, year := preg_year]
  
  # keep only needed columns and rename 
  dt_denom <- dt_denom[, .(year, correct_Freq = n_treated)]
  
  ##################################################
  # Merge dt_ind with dt_denom
  dt <- merge(dt_ind, dt_denom, by = "year", all.x = TRUE)

  # sanity check: stop if any Freq is greater than correct_Freq
  if (any(dt$Freq > dt$correct_Freq, na.rm = TRUE)) stop( "Check failed in file: ", ind_files[ind], " sum exceeds correct denominator for some rows")
  
  ##################################################
  # Create column with the difference between the correct_Freq and Freq (calculated as the sum of N in a year)
  # This value is equal to O_NEUROPATHICPAINALG_COV counts 
  dt[,missing_count:=correct_Freq-Freq]
  
  # replace the count value in "O_NEUROPATHICPAINALG_COV" with the value of the missing count where Freq (sum(N) is less than correct_Freq)
  dt[indication == "O_NEUROPATHICPAINALG_COV" & correct_Freq > Freq, N:=missing_count]
  
  ##################################################
  # Sanity check - does sum of N now equal the correct_Freq
  check <- dt[, .(sumN = sum(N), correct_Freq = unique(correct_Freq)), by = year]
  mismatch <- check[sumN != correct_Freq]
  if (nrow(mismatch) > 0) stop("Mismatch in file ", ind_files[ind], " for year(s): ", paste(mismatch$year, collapse = ", "))

  ##################################################
  # Clean up dt file 
  # drop columns you no longer need 
  indication_counts <- dt[,.(year, indication, N, correct_Freq)]
  
  # rename columns 
  setnames(indication_counts, "correct_Freq", "Freq")
  
  # if is.na(N), replace it with 0
  indication_counts[is.na(N), N := 0]
  
  # if is.na(Freq), replace it with 0
  indication_counts[is.na(Freq), Freq := 0]
  
  # calculate rate, if N = 0 and Freq = 0 then change the rate to 0 
  indication_counts[, rate := round(100 * N / Freq, 3)][N == 0 & Freq == 0, rate := 0]
  
  # create a column marking if rate is computable aka TRUE. It will be false if denominator is 0
  indication_counts[, rate_computable := Freq > 0]
  
  # Create corrected folder if it doesn't exist 
  corrected_dir <- file.path(dirname(ind_files[ind]), "corrected_indications")
  if (!dir.exists(corrected_dir)) dir.create(corrected_dir)

  # build path to corrected folder 
  corrected_file <- file.path(corrected_dir, file_base)

  # delete old file if it exists
  if (file.exists(corrected_file)) file.remove(corrected_file)

  # save the new corrected file
  saveRDS(dt_ind, corrected_file)
 
  # print message
  cat("Saved corrected file to:", corrected_file, "\n")
  
}
