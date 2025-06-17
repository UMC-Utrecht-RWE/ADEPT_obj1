# ==========================
# Load Required Packages
# ==========================

# Function to check if each package is installed.
# If not, it installs the package and then loads it.
# Uses suppressPackageStartupMessages to keep the console clean.

load_package <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

# List of all required packages
required_packages <- c("data.table", "lubridate", "RcppAlgos", "stringr", "readxl", "readr", "fs")

# Load each package
invisible(lapply(required_packages, load_package))



# # Load Packages needed
# if(!require(rmarkdown)){install.packages("rmarkdown")}
# suppressPackageStartupMessages(library(rmarkdown))
# if(!require(viridis)){install.packages("viridis")}
# suppressPackageStartupMessages(library(viridis))
# if(!require(RColorBrewer)){install.packages("RColorBrewer")}
# suppressPackageStartupMessages(library(RColorBrewer))
# if(!require(survival)){install.packages("survival")}
# suppressPackageStartupMessages(library(survival))
# if(!require(stringr)){install.packages("stringr")}
# suppressPackageStartupMessages(library(stringr))
# if(!require(knitr)){install.packages("knitr")}
# suppressPackageStartupMessages(library(knitr))
# if(!require(DT)){install.packages("DT")}
# suppressPackageStartupMessages(library(DT))
# if(!require(plyr)){install.packages("plyr")}
# suppressPackageStartupMessages(library(plyr))
# if(!require(ggplot2)){install.packages("ggplot2")}
# suppressPackageStartupMessages(library(ggplot2))
# if(!require(plotly)){install.packages("plotly")}
# suppressPackageStartupMessages(library(plotly))
# if(!require(grid)){install.packages("grid")}
# suppressPackageStartupMessages(library(grid))
# if(!require(e1071)){install.packages("e1071")}
# suppressPackageStartupMessages(library(e1071))
# if(!require(rlist)){install.packages("rlist")}
# suppressPackageStartupMessages(library(rlist))
# if(!require(colorRamps)){install.packages("colorRamps")}
# suppressPackageStartupMessages(library(colorRamps))
# if(!require(zoo)){install.packages("zoo")}
# suppressPackageStartupMessages(library(zoo))
# if(!require(readxl)){install.packages("readxl")}
# suppressPackageStartupMessages(library(readxl))
# if(!require(writexl)){install.packages("writexl")}
# suppressPackageStartupMessages(library(writexl))
# if(!require(ff)){install.packages("ff")}
# suppressPackageStartupMessages(library(ff))
# if(!require(dplyr)){install.packages("dplyr")}
# suppressPackageStartupMessages(library(dplyr))
# if(!require(AdhereR)){install.packages("AdhereR")}
# suppressPackageStartupMessages(library(AdhereR))
# if(!require(stringi)){install.packages("stringi")}
# suppressPackageStartupMessages(library(stringi))
