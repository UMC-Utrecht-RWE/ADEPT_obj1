if(nrow(study_population)>0){
  
  # years follow-up sequence 
  start_year <- year(as.IDate(study_population$entry_date, format = "%Y%m%d"))
  end_year   <- year(as.IDate(study_population$exit_date, format = "%Y%m%d"))
  
  # Create vector of all years from start to end for all subjects
  FUyears <- unlist(mapply(seq, start_year, end_year, SIMPLIFY = FALSE))
  
  # Filter by study period years
  studyFUyears <- FUyears[FUyears >= year(start_study_date) & FUyears <= year(end_study_date)]
  
  # Tabulate years frequency
  FUyears_dt <- data.table(Year = studyFUyears)[, .N, by = Year]

  # rename columns 
  setnames(FUyears_dt, "N", "Freq") 
  
  # save File 
  saveRDS(FUyears_dt, file.path(paths$D3_dir, "denominator", paste0(pop_prefix, "_denominator.rds")))
  
  # Create denominator subfolder if it doesn't exist
  denominator_plot_dir <- file.path(paths$D5_dir, "plots", "denominator")
  
  if (!dir.exists(denominator_plot_dir)) dir.create(denominator_plot_dir, recursive = TRUE)
  
  # full path to the denominator plot folder 
  plot_file <- file.path(denominator_plot_dir, paste0(pop_prefix, "_denominator.pdf"))
  
  # Sort by ear
  FUyears_dt <- FUyears_dt[order(Year)]
  
  # Save plot to PDF
  pdf(file.path(paths$D5_dir, "plots", "denominator", paste0(pop_prefix, "_denominator.pdf")), width = 8, height = 4)
  
  # Plot with both lines and points, suppressing default x-axis
  plot(FUyears_dt$Year, FUyears_dt$Freq,
       type = "b",                     # both line and points
       ylab = "Persons Observed per Year",
       xlab = "Year",
       xaxt = "n",                     # suppress automatic x-axis
       pch = 16,                       # solid circle points
       lwd = 2,                        # line width
       col = "darkblue")
  
  # Add custom x-axis with all years
  axis(1, at = FUyears_dt$Year, labels = FUyears_dt$Year)
  
  # Close the graphics device
  dev.off()
  
  
}

  



  # # plots denominator 
  # pdf((paste0(output_dir, "plots/", pop_prefix ,"_WOCBP_denominator.pdf")), width=8, height=4)
  # plot(FUmonths_df$studyFUmonths, FUmonths_df$Freq, ylab="Persons Observed per Month", xlab="Year and Month")
  # invisible(dev.off())
  # 
  # 




