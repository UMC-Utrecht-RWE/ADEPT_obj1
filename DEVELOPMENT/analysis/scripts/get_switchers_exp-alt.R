print("========================================================================================================")
print("========================= FINDING SWITCHERS EXPOSURE - ALTERNATIVE MEDICATIONS =========================")
print("========================================================================================================")

# Load All Altmed Medication
altmed_files <- list.files(file.path(paths$D4_dir, "1.2_altmeds"), full.names = TRUE)
altmed_files <- altmed_files[
  grepl(paste0("^", pop_prefix, "_"), basename(altmed_files)) & 
  (pop_prefix != "PC" | !grepl("^PC_HOSP_", basename(altmed_files)))
]


if(nrow(dt_discontinued)>0){
  
  for (altmed in altmed_files) {
    
    message("Processing exposure file: ", basename(altmed))
    
    dt_altmed <- readRDS(altmed)
    
    # Keep only needed columns
    dt_altmed <- dt_altmed[, .(person_id, code, rx_date, source)]
    
    # Prepare for overlap join
    dt_altmed[, window_start := rx_date]
    dt_altmed[, window_end := rx_date]
    setkey(dt_altmed, person_id, window_start, window_end)
    
    # Find overlaps within 30 days after discontinuation
    switchers <- foverlaps(dt_altmed, dt_discontinued, type = "within", nomatch = 0)
    switchers <- switchers[code != i.code][, c("i.window_start", "i.window_end"):=NULL]
    setnames(switchers, c("i.code", "source"), c("ATC_switched_to", "group_switched_to"))
    
    if(nrow(switchers)>0){
      
      # Save switches result
      saveRDS(switchers, file = file.path(file.path(paths$D4_dir, "1.2_switching"), sub("_altmeds_data\\.rds$", ".rds", basename(altmed))))
      
    } else {
      
      message("No Switchers in: ", basename(altmed))
      
    }
  }

}
