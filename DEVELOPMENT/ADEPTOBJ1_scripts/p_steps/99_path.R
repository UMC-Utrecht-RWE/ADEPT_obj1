###################################################
# Create Folders and Set Paths 
###################################################

# Define root folder 
if (!exists("root_dir")) root_dir <- thisdir

# Assign names to paths 
paths <- list(
  D3_dir = file.path(root_dir, "D3_study_variables"),
  D4_dir = file.path(root_dir, "D4_analytic_datasets"),
  D5_dir = file.path(root_dir, "D5_results")
)

# Delete folders if they already exist <<< #
for (p in paths) {
  if (dir.exists(p)) {
    unlink(p, recursive = TRUE)
    message("Deleted existing folder: ", p)
  }
}

if(deap_flags$is_EFEMERIS | deap_flags$is_FIN_REG) {
  # Create list of all folders to be created
  subfolders <- list(
    
    file.path(paths$D3_dir, c(
      "concept_sets", "denominator", "source_population", "spells", "study_population", "tmp", "tx_episodes",
      "algorithm_input", "alternatives", "cov", "exposure", "indication" 
    )),
    
    file.path(paths$D4_dir, c(
      "1.2_discontinued", "1.2_altmeds", "1.2_switching", "1.2_polytherapy", 
      "1.3_pre-pregnancy_use", "1.3_pregnancy_initiation", "1.3_pregnancy_continuous", 
      "1.4_pregnancy_discontinuation", "1.4_pregnancy_switching", "1.4_pregnancy_polytherapy"
    )),
    
    file.path(paths$D5_dir, c(
      "1.3_pre-pregnancy_use", "1.3_pregnancy_initiation", "1.3_pregnancy_continuous", 
      "1.4_pregnancy_discontinuation", "1.4_pregnancy_switching", "1.4_pregnancy_polytherapy",
      "flowcharts", "baseline_tables", "plots"
      
    ))
  ) |> unlist()
  
} else {
  # Create list of all folders to be created
  subfolders <- list(
    
    file.path(paths$D3_dir, c(
      "concept_sets", "denominator", "source_population", "spells", "study_population", "tmp", "tx_episodes",
      "algorithm_input", "alternatives", "cov", "exposure", "indication" 
    )),
    
    file.path(paths$D4_dir, c(
      "1.1_incidence", "1.1_prevalence",
      "1.2_treatment_duration", "1.2_discontinued", "1.2_altmeds", "1.2_switching", "1.2_polytherapy", 
      "1.3_pre-pregnancy_use", "1.3_pregnancy_initiation", "1.3_pregnancy_continuous", 
      "1.4_pregnancy_discontinuation", "1.4_pregnancy_switching", "1.4_pregnancy_polytherapy"
    )),
    
    file.path(paths$D5_dir, c(
      "1.1_incidence", "1.1_prevalence",
      "1.2_treatment_duration", "1.2_discontinued", "1.2_altmeds", "1.2_switching", "1.2_polytherapy", 
      "1.3_pre-pregnancy_use", "1.3_pregnancy_initiation", "1.3_pregnancy_continuous", 
      "1.4_pregnancy_discontinuation", "1.4_pregnancy_switching", "1.4_pregnancy_polytherapy",
      "flowcharts", "baseline_tables", "plots"
      
    ))
  ) |> unlist()
}


# Create folders
for (f in subfolders) {
  
  if (!dir.exists(f)) {
    
    dir.create(f, recursive = TRUE)
    message("Created folder: ", f)
    
  } else {
    
    message("Folder already exists: ", f)
    
  }
}



