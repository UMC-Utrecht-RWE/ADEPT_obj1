print("Preparing denominator...")

if (nrow(study_population) > 0) {
  # Convert start and end follow up to IDate
  study_population[, start_follow_up := as.IDate(start_follow_up)]
  study_population[, end_follow_up := as.IDate(end_follow_up)]
  
  # Get study period boundaries
  start_year <- pmax(year(study_population$start_follow_up), year(start_study_date))
  end_year   <- pmin(year(study_population$end_follow_up), year(end_study_date))
  
  # Generate vector of years of follow-up per person
  studyFUyears <- unlist(mapply(seq, start_year, end_year, SIMPLIFY = FALSE))
  
  # Tabulate frequency of persons observed per year
  FUyears_dt <- data.table(year = studyFUyears)[, .N, by = year]
  
  # Rename count column
  setnames(FUyears_dt, "N", "Freq") 
  
  # Save denominator RDS file
  saveRDS(FUyears_dt, file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))
  
  # Create denominator plots directory if it does not exist
  denominator_plot_dir <- file.path(paths$D5_dir, "plots", "denominator")
  if (!dir.exists(denominator_plot_dir)) dir.create(denominator_plot_dir, recursive = TRUE)
  
  # Sort years ascending
  FUyears_dt <- FUyears_dt[order(year)]
  
  # Plot denominator over years and save as PDF
  pdf(file.path(denominator_plot_dir, paste0(pop_prefix, "_denominator.pdf")), width = 8, height = 4)
  
  plot(FUyears_dt$year, FUyears_dt$Freq,
       type = "b",       # both line and points
       ylab = "Persons Observed per year",
       xlab = "year",
       xaxt = "n",       # suppress default x-axis
       pch = 16,         # solid circle points
       lwd = 2,          # line width
       col = "darkblue")
  
  axis(1, at = FUyears_dt$year, labels = FUyears_dt$year)
  
  dev.off()
  
} else {
  
  message("No study population rows found; skipping denominator creation.")
  
}
