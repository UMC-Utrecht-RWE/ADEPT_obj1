print("===========================================================================")
print("========================= GET POLYTHERAPY =================================")
print("===========================================================================")


# List all .rds files
input_dir <- file.path(paths$D3_dir, "tx_episodes", "individual")
output_dir <- file.path(paths$D4_dir, "1.2_polytherapy")

files <- list.files(input_dir, pattern = paste0("^", pop_prefix, ".*\\.rds$"), full.names = TRUE)
files <- files[(pop_prefix != "PC" | !grepl("^PC_HOSP_", basename(files)))]

# Loop through each unique file pair
for (i in seq_along(files)) {
  for (j in seq_along(files)) {
    if (i >= j) next  # Skip same or duplicate pairs
    
    name_i <- sub("_treatment_episode\\.rds$", "", basename(files[i]))
    name_j <- sub("_treatment_episode\\.rds$", "", basename(files[j]))
    
    message("Processing: ", name_i, " and ", name_j)
    
    # Load treatment episodes
    dt1 <- readRDS(files[i])
    dt2 <- readRDS(files[j])
    
    # Ensure required columns exist
    required_cols <- c("person_id", "episode.start", "episode.end")
    if (!all(required_cols %in% names(dt1)) || !all(required_cols %in% names(dt2))) next
    
    # Convert to Date
    dt1[, `:=`(start1 = as.Date(episode.start), end1 = as.Date(episode.end))]
    dt2[, `:=`(start2 = as.Date(episode.start), end2 = as.Date(episode.end))]
    
    # Keep only overlapping person_ids
    common_ids <- intersect(unique(dt1$person_id), unique(dt2$person_id))
    if (length(common_ids) == 0) next
    
    dt1_sub <- dt1[person_id %in% common_ids, .(person_id, start1, end1, code)]
    dt2_sub <- dt2[person_id %in% common_ids, .(person_id, start2, end2, code)]
    
    # Prepare for foverlaps
    setnames(dt1_sub, c("start1", "end1"), c("start", "end"))
    setnames(dt2_sub, c("start2", "end2"), c("start", "end"))
    setkey(dt1_sub, person_id, start, end)
    setkey(dt2_sub, person_id, start, end)
    
    # Find overlaps
    overlap_dt <- foverlaps(dt1_sub, dt2_sub, type = "any", nomatch = 0L)
    
    # Calculate overlap duration
    overlap_dt[, overlap_start := pmax(start, i.start)]
    overlap_dt[, overlap_end := pmin(end, i.end)]
    overlap_dt[, overlap_days := as.numeric(overlap_end - overlap_start) + 1]
    
    # Filter ≥182 days and same calendar year
    # overlap_dt <- overlap_dt[overlap_days >= 5]
    #overlap_dt <- overlap_dt[overlap_days >= 5 & year(overlap_start) == year(overlap_end)]
    # Save if any overlap found
    if (nrow(overlap_dt) > 0) {
      out_file <- file.path(output_dir, paste0(name_i, "-", name_j, ".rds"))
      saveRDS(overlap_dt, out_file)
    }
  }
}