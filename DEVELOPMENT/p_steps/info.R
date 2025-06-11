# Define function
`%!in%` = Negate(`%in%`)

# Function to list files by pattern in a directory
get_files <- function(dir, pattern) if(length(files <- list.files(dir, pattern = pattern)) == 0) character(0) else files

# Detect which data tables are present in the directory
actual_tables <- list(
  MEDICINES            = get_files(path_CDM_dir, "^MEDICINES"),
  EVENTS               = get_files(path_CDM_dir, "^EVENTS"),
  PROCEDURES           = get_files(path_CDM_dir, "^PROCEDURES"),
  MEDICAL_OBSERVATIONS = get_files(path_CDM_dir, "^MEDICAL_OBSERVATIONS"),
  SURVEY_ID            = get_files(path_CDM_dir, "^SURVEY_ID"),
  SURVEY_OBSERVATIONS  = get_files(path_CDM_dir, "^SURVEY_OBSERVATIONS"),
  EUROCAT              = get_files(path_CDM_dir, "^EUROCAT"),
  VACCINES             = get_files(path_CDM_dir, "^VACCINES")
)

# Check for presence of diagnoses and pregnancy datasets

### pregnancies: if TRUE pregnancies can be retrieved
### diagnoses: if TRUE diagnoses can be retrieved

### diagnoses_pregnancy_med: if TRUE medicine exposure can be estimated
### diagnoses_pregnancy_vacc: if TRUE vaccine exposure can be estimated
### diagnoses_pregnancy_med_vacc: if TRUE both medicines and vaccine exposure can be estimated

### pregnancy_only_med: if TRUE only medicine exposure in pregnancy can be estimated
### pregnancy_only_vacc: if TRUE only vaccine exposure in pregnancy can be estimated
### pregnancy_only_med_vacc: if TRUE only medicine and vaccine exposure in pregnancy can be estimated

if(sum(length(actual_tables$EVENTS), 
       length(actual_tables$MEDICAL_OBSERVATIONS), 
       length(actual_tables$SURVEY_OBSERVATIONS))==0){
  
  #no diagnoses can be retrieved
  diagnoses<-FALSE
  diagnoses_pregnancy_med<-FALSE
  diagnoses_pregnancy_vacc<-FALSE
  diagnoses_pregnancy_med_vacc<-FALSE
  
  if(length(actual_tables$SURVEY_ID)>0){
    
    #pregnancies can be retrieved
    pregnancies<-TRUE
    
    if(length(actual_tables$MEDICINES)>0){
      
      if(length(actual_tables$VACCINES)>0){
        
        pregnancy_only_med      <- TRUE
        pregnancy_only_vacc     <- TRUE
        pregnancy_only_med_vacc <- TRUE
        
      } else {
        
        pregnancy_only_med      <-TRUE
        pregnancy_only_vacc     <-FALSE
        pregnancy_only_med_vacc <-FALSE
        
      }
      
    } else {
      
      if(length(actual_tables$VACCINES)>0){
        
        pregnancy_only_med      <- FALSE 
        pregnancy_only_vacc     <- TRUE
        pregnancy_only_med_vacc <- FALSE
        
      } else {
        
        pregnancy_only_med      <- FALSE
        pregnancy_only_vacc     <- FALSE
        pregnancy_only_med_vacc <- FALSE
        
      }
      
    }
    
  } else {
    
    pregnancies<-FALSE
    
  }
  
} else {
  
  #diagnoses can be retrieved
  diagnoses               <- TRUE
  pregnancies             <- TRUE
  
  pregnancy_only_med      <- FALSE
  pregnancy_only_vacc     <- FALSE
  pregnancy_only_med_vacc <- FALSE
  
  if(length(actual_tables$MEDICINES)>0){
    
    if(length(actual_tables$VACCINES)>0){
      
      diagnoses_pregnancy_med      <- TRUE 
      diagnoses_pregnancy_vacc     <- TRUE
      diagnoses_pregnancy_med_vacc <- TRUE
      
    } else {
      
      diagnoses_pregnancy_med      <- TRUE 
      diagnoses_pregnancy_vacc     <- FALSE
      diagnoses_pregnancy_med_vacc <- FALSE
      
    }
    
  } else {
    
    if(length(actual_tables$VACCINES)>0){
      
      diagnoses_pregnancy_med      <- FALSE 
      diagnoses_pregnancy_vacc     <- TRUE
      diagnoses_pregnancy_med_vacc <- FALSE 
      
    } else {
      
      diagnoses_pregnancy_med      <- FALSE 
      diagnoses_pregnancy_vacc     <- FALSE
      diagnoses_pregnancy_med_vacc <- FALSE 
      
    }
    
  }
  
}


# Check for presence of subpopulations


# Load Metadata file
METADATA <- fread(list.files(path_CDM_dir, pattern = "^METADATA", full.names = TRUE))

# Filter METADATA to exclude certain types of metadata
METADATA_subp <- METADATA[type_of_metadata %!in% c("presence_of_table", "presence_of_column", "list_of_values")]

# Check if any subpopulation metadata is present
subpopulations_present <- METADATA_subp[type_of_metadata == "subpopulations", .N] > 0

# If present, extract subpopulation names
if (subpopulations_present) {
  subpop_values <- METADATA_subp[type_of_metadata == "subpopulations", values]
  subpopulations_names <- unlist(str_split(subpop_values, pattern = " "))
}
