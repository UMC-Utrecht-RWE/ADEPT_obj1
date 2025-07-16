
# <<< Define root folder >>> #
if (!exists("root_dir")) {root_dir <- thisdir
}
# Assign named paths only to D3, D4, D5
paths <- list(
  D3_dir = file.path(root_dir, "D3_study_variables"),
  D4_dir = file.path(root_dir, "D4_analytic_datasets"),
  D5_dir = file.path(root_dir, "D5_results")
)

# >>> Delete D3, D4, D5 if they already exist <<< #
for (p in paths) {
  if (dir.exists(p)) {
    unlink(p, recursive = TRUE)
    message("Deleted existing folder: ", p)
  }
}

# List all subfolders to be created
subfolders <- list(
  file.path(paths$D3_dir, c(
    "spells", "source_population", "study_population", "concept_sets",
    "exposure", "cov", "indications", "tx_episodes", "denominator", "algorithm_input", "tmp", "alternatives"
  )),
  file.path(paths$D4_dir, c(
    "1.1_incidence", "1.1_prevalence",
    "1.2_treatment_duration", "1.2_discontinued", "1.2_altmeds", "1.2_switching", "1.2_polytherapy", 
    "1.3_pre-pregnancy_use_rate", "1.3_initiation_rate_during_pregnancy", "1.3_continuous_use_rate", 
    "1.4_discontinued_use_rate"
  )),
  file.path(paths$D5_dir, c(
    "1.1_incidence", "1.1_prevalence",
    "1.2_treatment_duration", "1.2_discontinued", "1.2_altmeds", "1.2_switching", "1.2_polytherapy", 
    "1.3_pre-pregnancy_use_rate", "1.3_initiation_rate_during_pregnancy", "1.3_continuous_use_rate", 
    "1.4_discontinued_use_rate",
    "flowcharts", "baseline_tables", "plots"
    
  ))
) |> unlist()

# Create all folders
# Create all folders if they don't already exist
for (f in subfolders) {
  if (!dir.exists(f)) {
    dir.create(f, recursive = TRUE)
    message("Created folder: ", f)
  } else {
    message("Folder already exists: ", f)
  }
}



