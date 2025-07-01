print("=========================================================================================")
print("========================= FINDING SWITCHERS EXPOSURE - EXPOSURE =========================")
print("=========================================================================================")

# Files to exclude (prefixes before "_discontinued_data.rds")
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

# List and filter files
files <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), full.names = TRUE)
files <- files[!basename(files) %like% paste0("^(", paste(exclude, collapse = "|"), ")_")]

# Read and bind
dt_discontinued <- rbindlist(lapply(files, readRDS), use.names = TRUE, fill = TRUE)
dt_discontinued[, window_start := episode.end][, window_end := episode.end + 120]
dt_discontinued[,c("episode.start", "episode.ID", "end.episode.gap.days", "episode.duration", "episode.end", "entry_date", "exit_date", "next_start", "discontinuer_flag", "year"):= NULL]
setkey(dt_discontinued, person_id, window_start, window_end)

# Load All Exposure Medication
exposure_files <- list.files(file.path(paths$D3_dir, "exposure"), full.names = TRUE)

for (expo in exposure_files) {
  
  message("Processing exposure file: ", basename(expo))
  
  dt_exp <- readRDS(expo)
  
  # Keep only needed columns
  dt_exp <- dt_exp[, .(person_id, code, rx_date)]
  

  # Add source file name column (without extension)
  dt_exp[, group_switched_to := sub("\\.rds$", "", basename(expo))]

  # Prepare for overlap join
  dt_exp[, window_start := rx_date]
  dt_exp[, window_end := rx_date]
  setkey(dt_exp, person_id, window_start, window_end)

  # Find overlaps within 30 days after discontinuation
  switchers <- foverlaps(dt_exp, dt_discontinued, type = "within", nomatch = 0)
  switchers <- switchers[code != i.code][, c("i.window_start", "i.window_end"):=NULL]
  setnames(switchers, "i.code", "ATC_switched_to")
  
  if(nrow(switchers)>0){

    # Save switches result
  saveRDS(switchers, file = file.path(file.path(paths$D4_dir, "1.2_switching"), basename(expo)))
    
  } else {
    
    message("No Switches in: ", basename(expo))
    
  }
}

