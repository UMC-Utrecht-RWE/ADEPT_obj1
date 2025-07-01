# Read and bind them into one data.table
dt_switching <- rbindlist(lapply(list.files(file.path(paths$D4_dir, "1.2_switching"), pattern = "\\.rds$", full.names = TRUE), readRDS), use.names = TRUE, fill = TRUE)
