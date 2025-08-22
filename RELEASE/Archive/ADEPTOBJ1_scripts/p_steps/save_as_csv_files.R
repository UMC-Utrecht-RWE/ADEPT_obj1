# Load libraries
library(data.table)

# define path to your results folder - this is where all the DAPS folders will be. 
# the structure should e.g. 
#===  Main Folder -> DAP named folder -> D5_results
#===  e.g. Results/BIFAP/D5_results 
#===       Results/CPRD/D5_results

#=====================================================================================
#<<< USER INPUT >>> 
# Set Path to Main Folder Here 
path_to_main_folder <- "Set/Path/To/Main/Folder/Here"
# Set name of DAP folder you want to convert to csv
dap_name <- "CPRD"

#=====================================================================================

# set paths to input and output folders 
input_root  <- file.path(path_to_main_folder, dap_name, "D5_results")
output_root <- file.path(path_to_main_folder, dap_name, "D5_results_csv")

# find all .rds files recursively 
rds_files <- list.files(path = input_root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)

# loop through files
for (rds_path in rds_files) {
  
  # read in file 
  dt <- readRDS(rds_path)
  
  # get relative path from input_root
  relative_path <- substr(rds_path, nchar(input_root) + 2, nchar(rds_path))

  # create output path 
  output_path <- file.path(output_root, relative_path)
  
  # Extract folder and file name
  output_dir <- dirname(output_path)
  filename <- basename(output_path)
  
  # Prefix file name with dap_name and change extension to .csv
  base_filename <- tools::file_path_sans_ext(filename)
  new_filename <- paste0(dap_name, "_", base_filename, ".csv")
  
  # Make sure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Write csv
  fwrite(dt, file.path(output_dir, new_filename))
}





 
  
  
