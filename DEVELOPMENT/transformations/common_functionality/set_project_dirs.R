# The aim of this script is to create variables in R that store the paths of all
# directories and subdirectories in your project. The variables are named in a 
# way that reflects the directory structure of your project, making it easier 
#for you to reference these paths in your code.

# Get the path of the current working directory
project_path <- getwd()

# List all directories and subdirectories
all_dirs <- list.dirs(path = project_path, full.names = TRUE, recursive = TRUE)
all_dirs <- all_dirs[!all_dirs %in% project_path]

# Store each directory path in a variable with a meaningful name
for (dir_path in all_dirs) {
  # Remove the project path and the leading slash from the directory path
  dir_name <- sub(paste0("^", project_path, "/"), "", dir_path)

  # Replace slashes with underscores to create a valid variable name
  var_name <- gsub("/", "_", dir_name)

  # Assign the directory path to a variable with the meaningful name
  assign(var_name, dir_path)
}

rm(dir_path, dir_name, var_name)
