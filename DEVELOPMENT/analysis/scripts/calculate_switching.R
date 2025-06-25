# SWITCHING - EXPOSURE VS ALTERNATIVE MEDS

# Read in needed files 
discontinued_data <- rbindlist(lapply(list.files(file.path(paths$D4_dir, "1.2_discontinued"), pattern = "\\.rds$", full.names = TRUE, ignore.case = TRUE), readRDS), use.names = TRUE, fill = TRUE)
altmed_data <- rbindlist(lapply(list.files(file.path(paths$D3_dir, "tx_episodes", "alternatives"), pattern = "\\.rds$", full.names = TRUE, ignore.case = TRUE), readRDS), use.names = TRUE, fill = TRUE)

# Remove columns you do not need
discontinued_data[,c("episode.ID", "end.episode.gap.days", "episode.duration", "next_start", "discontinuer_flag", "year"):=NULL]
altmed_data[,c("episode.ID", "end.episode.gap.days", "episode.duration", "entry_date", "exit_date", "start_follow_up", "end_follow_up"):=NULL]

# Rename columns for clarity
setnames(discontinued_data, c("episode.start", "episode.end"), c("discontinued.episode.start", "discontinued.episode.end"))
setnames(altmed_data, c("episode.start", "episode.end", "ATC"), c("altmed.episode.start", "altmed.episode.end", "ATC.alt"))

# Add column to keep original episode.end (discontinued) and episode.start (altmed) info
discontinued_data[,orig.discontinued.end.date:=discontinued.episode.end]
altmed_data[,orig.altmed.episode.start:=altmed.episode.start]

# Sort and key tables appropriately
setkey(discontinued_data, person_id, discontinued.episode.end)
setkey(altmed_data, person_id, altmed.episode.start)

# Perform a non-equi join:
## Conditions: 
### altmed.episode.start >= discontinued episode.end 
### altmed.episode.start <= discontinued.episode.end + 120

# Create a max join window for altmed episode to be within 120 days after discontinuation
discontinued_data[, join_window_end := discontinued.episode.end + 120]

# Perform non-equi join: altmed episode starts within [discontinued_end, discontinued_end + 120]
switching_dt <- altmed_data[discontinued_data,
                           on = .(person_id,
                                  altmed.episode.start >= discontinued.episode.end,
                                  altmed.episode.start <= join_window_end),
                           nomatch = 0L,
                           allow.cartesian = TRUE
]

# Remove any duplicates
switching_dt <- unique(switching_dt)




