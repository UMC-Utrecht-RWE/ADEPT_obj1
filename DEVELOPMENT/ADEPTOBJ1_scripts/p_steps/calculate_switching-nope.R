############################################################################################################
# <<< Sub-objective 1.2: Switching rate >>>
# Annual switching rate from one ASM to another or to an alternative medication
# Numerator: ASM users who discontinued and started a new ASM or alternative within 120 days
# Denominator: Prevalent ASM users in that year
# Stratification: drug, year, data source
############################################################################################################

print("===============================================================================")
print("========================= CALCULATE SWITCHING =================================")
print("===============================================================================")

### Load Data Sets ###
exclude <- c("DP_ANTIEPINEW", "DP_ANTIEPIOLD", "DP_BENZOANTIEPILEPTIC", "DP_GABAPENTINOIDS")

files_discontinued_episodes <- list.files(file.path(paths$D4_dir, "1.2_discontinued"), pattern = "\\.rds$")
files_discontinued_episodes <- files_discontinued_episodes[grepl(paste0("^", pop_prefix, "_"), files_discontinued_episodes)]
if(pop_prefix=="PC") files_discontinued_episodes <- files_discontinued_episodes[!grepl("PC_HOSP", files_discontinued_episodes)]
files_discontinued_episodes <- files_discontinued_episodes[!(gsub(paste0("^", pop_prefix, "_|_discontinued_data\\.rds$"), "", files_discontinued_episodes)) %in% exclude]

files_exposures <- list.files(file.path(paths$D3_dir, "exposure"))
files_exposures <- files_exposures[grepl(paste0("^", pop_prefix, "_"), files_exposures)]
if(pop_prefix=="PC") files_exposures <- files_exposures[!grepl("PC_HOSP", files_exposures)]

files_altmeds <- list.files(file.path(paths$D4_dir, "1.2_altmeds"))
files_altmeds <- files_altmeds[grepl(paste0("^", pop_prefix, "_"), files_altmeds)]
if(pop_prefix=="PC") files_altmeds <- files_altmeds[!grepl("PC_HOSP", files_altmeds)]

files_prevalence_counts <- list.files(file.path(paths$D5_dir, "1.1_prevalence"), pattern = "\\.rds$")
files_prevalence_counts <- files_prevalence_counts[grepl(paste0("^", pop_prefix, "_"), files_prevalence_counts)]
if(pop_prefix=="PC") files_prevalence_counts <- files_prevalence_counts[!grepl("PC_HOSP", files_prevalence_counts)]

for(episode in seq_along(files_discontinued_episodes)){
  dt_discontinued <- readRDS(file.path(paths$D4_dir, "1.2_discontinued", files_discontinued_episodes[episode]))
  dt_discontinued[, c("episode.ID", "end.episode.gap.days", "episode.duration", "next_start", "discontinuer_flag", "year") := NULL]
  dt_discontinued[, `:=`(window_start = episode.end, window_end = episode.end + 120)]
  setkey(dt_discontinued, person_id, window_start, window_end)
  
  switchers_all <- list()
  
  for(exposure in seq_along(files_exposures)){
    if(gsub("_discontinued_data\\.rds$", "", files_discontinued_episodes[episode]) == gsub("\\.rds$", "", files_exposures[exposure])) next
    
    dt_exposures <- readRDS(file.path(paths$D3_dir, "exposure", files_exposures[exposure]))
    dt_exposures <- dt_exposures[, .(person_id, code, Varname, rx_date)]
    dt_exposures[, `:=`(window_start = rx_date, window_end = rx_date)]
    setkey(dt_exposures, person_id, window_start, window_end)
    
    switchers <- foverlaps(dt_exposures, dt_discontinued, type = "within", nomatch = 0)
    switchers <- switchers[code != i.code]
    if (nrow(switchers)>0){
      switchers[, c("i.window_start", "i.window_end") := NULL]
      setnames(switchers, c("i.code", "Varname"), c("code_switched_to", "atc_group_switched_to"))
      switchers_all[[length(switchers_all)+1]] <- switchers
    }
  }
  
  for (altmed in seq_along(files_altmeds)){
    dt_altmeds <- readRDS(file.path(paths$D4_dir, "1.2_altmeds", files_altmeds[altmed]))
    dt_altmeds <- dt_altmeds[, .(person_id, code, Varname, rx_date)]
    dt_altmeds[, `:=`(window_start = rx_date, window_end = rx_date)]
    setkey(dt_altmeds, person_id, window_start, window_end)
    
    switchers <- foverlaps(dt_altmeds, dt_discontinued, type = "within", nomatch = 0)
    switchers <- switchers[code != i.code]
    if (nrow(switchers)>0){
      alt_group_name <- gsub("_altmed_data\\.rds$", "", files_altmeds[altmed])
      alt_group_name <- gsub(paste0(pop_prefix, "_"), "", alt_group_name)
      switchers[, atc_group_switched_to := alt_group_name]
      switchers[, c("i.window_start", "i.window_end") := NULL]
      setnames(switchers, "i.code", "code_switched_to")
      switchers_all[[length(switchers_all)+1]] <- switchers
    }
  }
  
  # Combine and deduplicate per person_id and discontinued episode
  if (length(switchers_all) > 0) {
    switchers_all <- rbindlist(switchers_all, use.names = TRUE, fill = TRUE)
    switchers_all <- switchers_all[order(person_id, episode.start, rx_date)]
    switchers_all <- switchers_all[, .SD[1], by = .(person_id, episode.start)]  # one switch per episode
    saveRDS(switchers_all, file.path(paths$D4_dir, "1.2_switching", gsub("_discontinued_data\\.rds$", "_switchers_data.rds", files_discontinued_episodes[episode])))
  }
}

# Combine and calculate switching rate
files_switchers <- list.files(file.path(paths$D4_dir, "1.2_switching"), pattern = "_switchers_data\\.rds$")
files_switchers <- files_switchers[grepl(paste0("^", pop_prefix, "_"), files_switchers)]
if(pop_prefix=="PC") files_switchers <- files_switchers[!grepl("PC_HOSP", files_switchers)]

prefixes <- gsub("_switchers_data\\.rds$", "", files_switchers)

for (pfx in prefixes) {
  switchers <- readRDS(file.path(paths$D4_dir, "1.2_switching", paste0(pfx, "_switchers_data.rds")))
  switchers <- switchers[rx_date >= start_follow_up & rx_date <= end_follow_up]
  if(nrow(switchers)>0){
    switchers[, year := year(rx_date)]
    switchers <- unique(switchers, by = .(person_id, year))  # still only one per person per year
    switcher_counts <- switchers[, .(N = .N), by = year]
    
    matched_file <- files_prevalence_counts[gsub("_prevalence\\.rds$", "", files_prevalence_counts) == pfx]
    if (length(matched_file) == 1) {
      prev_counts <- readRDS(file.path(paths$D5_dir, "1.1_prevalence", matched_file))
      prev_counts[, c("n_total", "rate", "rate_computable") := NULL]
      setnames(prev_counts, "n_treated", "n_total")
      switcher_all <- merge(switcher_counts, prev_counts, by = "year", all.y = TRUE)
      switcher_all[is.na(N), N := 0]
      switcher_all[, rate := round(100 * N / n_total, 3)][N == 0 & n_total == 0, rate := 0]
      if (nrow(switcher_all[N > n_total]) > 0) warning("Numerator > Denominator")
      if (nrow(switcher_all[n_total == 0 & N != 0]) > 0) warning("Non-zero numerator with zero denominator")
      switcher_all[, rate_computable := n_total > 0]
      setnames(switcher_all, "N", "n_treated")
      saveRDS(switcher_all, file.path(paths$D5_dir, "1.2_switching", paste0(pfx, "_switched.rds")))
    }
  }
}

if(length(list.files(file.path(paths$D3_dir, "tmp"))) > 0) unlink(list.files(file.path(paths$D3_dir, "tmp"), full.names = TRUE))
