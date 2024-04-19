# Load necessary libraries for feature extraction
library(MfeatExtractor)
library(mfe)

# Set the working directory to the specified path where the datasets reside
setwd("D:/Current_Paper_Experiments/NASLAF")

# Main function to orchestrate the extraction of meta-features
mainExtraction <- function(datafile, option) {
  # Ensure all packages are loaded
  devtools::load_all()
  
  # Retrieve dataset names from the specified directory and remove file extensions
  dataset_names <- gsub(x = list.files(path = "data/datasets/"), pattern = "\\.rda$", replacement = "")
  
  # Iterate over each dataset
  for (dataset in dataset_names) {
    # Forced assignment for the purpose of this function; should ideally come from input or be dynamic
    datafile <- dataset
    option <- "stat"  # For testing, the 'stat' option is used; this could be parameterized
    
    # Validate the datafile and option against known values
    assertChoice(x = datafile, choices = dataset_names, .var.name = "datafile")
    assertChoice(x = option, choices = c("mfe", "infotheo", "comp", "all", "stat"))
    
    # Logging the process start
    cat(" ---------------------------- \n")
    cat(" ** Meta-features extractor ** \n")
    cat(" ---------------------------- \n")
    cat(paste0(" - Datafile: \t", datafile, "\n"))
    cat(paste0(" - Features: \t", option, "\n"))
    cat(" ---------------------------- \n")
    
    # Execute the extraction process for RDA files
    load(paste0("data/datasets/", datafile, ".rda"))  # Assumes data is loaded into a variable named 'data'
    runExtraction(data = data, option = option)  # Adjusted to pass data directly
    
    # Logging the process end
    cat("\n - Finished!\n")
    cat(" ---------------------------- \n")
  }
}

# -------------------------------------------------------------------------------------------------
# Set options for script output visibility
options(echo = TRUE)

# Retrieve command line arguments
args = commandArgs(trailingOnly = TRUE)

# Function to parse command line arguments
parseArgs <- function(x) {
  lapply(strsplit(sub("^--", "", x), "="), function(arg) {
    list(name = arg[1], value = arg[2])
  })
}

# Convert parsed arguments into a data frame for easier access
argsDF <- do.call(rbind, lapply(parseArgs(args), function(arg) as.data.frame(as.list(arg), stringsAsFactors = FALSE)))
argsL <- as.list(setNames(as.character(argsDF$value), argsDF$name))

# Execute the main extraction function using parsed arguments
mainExtraction(datafile = argsL$datafile, option = argsL$option)

# -------------------------------------------------------------------------------------------------
