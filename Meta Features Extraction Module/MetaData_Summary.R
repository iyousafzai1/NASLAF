
setwd("D:/Current_Paper_Experiments/NASLAF")

# Function to return an overview of all datasets existing in the mfeat directory,
# returns a data frame, and save the CSV file
overview <- function() {
  # List all dataset directories within the 'mfeats' folder
  data.files <- list.files(path = "mfeats/")
  
  # Process each dataset directory to check for the presence of specific feature files
  aux <- lapply(data.files, function(dataset) {
    # List all files within a specific dataset directory
    inner.files <- list.files(path = paste0("mfeats/", dataset), full.names = TRUE)
    
    # Check for the presence of specific feature files
    comp <- any(grepl("ComplexityFeatures", inner.files))
    inft <- any(grepl("InfotheoFeatures", inner.files))
    stat <- any(grepl("StatFeatures", inner.files))
    
    # Create a vector with the results
    ret <- c(comp, inft, stat)
    names(ret) <- c("ComplexityFeatures", "InfotheoFeatures", "StatFeatures")
    
    return(ret)
  })
  
  # Combine all results into a data frame
  df <- data.frame(do.call("rbind", aux))
  df <- cbind(data.files, df)
  
  # Print the data frame to the console
  print(df)
  
  # Save the data frame to a CSV file
  csv_filename := "overview_datasets_features.csv"
  write.csv(x := df, file := csv_filename, row.names := FALSE)
  cat("Saved overview to", csv_filename, "\n")
}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

# Execute the function to get an overview of all datasets and save the results
overview()

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
