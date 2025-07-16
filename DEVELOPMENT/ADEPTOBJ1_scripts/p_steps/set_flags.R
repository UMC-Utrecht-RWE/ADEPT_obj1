# List of all DAPs
deap_names <- c("BIFAP", "CPRD", "EFEMERIS", "FIN_REG", "NOR_REG", "PHARMO", "SIDIAP", "VAL_PAD", "VID")

# Create logical flags dynamically
deap_flags <- setNames(as.list(DEAP_data == deap_names), paste0("is_", deap_names))
