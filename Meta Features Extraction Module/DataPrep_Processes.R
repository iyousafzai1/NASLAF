# Required libraries
library(mlr)
library(RSNNS)

# Function to preprocess data
advancedPreProcessing <- function(dataset) {
  # First, handle any missing data across the dataset
  if (any(is.na(dataset))) {
    imputedData <- impute(obj = dataset, target = "Class",
                          classes = list(
                            integer = imputeMedian(),  # Median imputation for integer
                            factor = imputeConstant(const = "New"),  # Constant imputation for factors
                            numeric = imputeMedian()  # Median imputation for numeric
                          ))
    dataset <- imputedData$data
  }
  
  # Identify the column index for the class attribute
  classColIndex <- which(colnames(dataset) == "Class")
  # Extract the class attribute before processing
  classData <- dataset[, classColIndex]
  # Remove the class column from the main dataset for processing
  dataset <- dataset[, -classColIndex]
  
  # Process each attribute in the dataset
  for (attributeName in colnames(dataset)) {
    if (is.factor(dataset[, attributeName])) {
      # Adjust factor levels if they are too uniform or match row count
      if (nlevels(dataset[, attributeName]) == 1 || nlevels(dataset[, attributeName]) == nrow(dataset)) {
        dataset[, attributeName] <- NULL  # Remove attribute if only one level
      } else {
        # Normalize the levels to be sequential integers
        levelsToChange <- seq_along(levels(dataset[, attributeName]))
        levels(dataset[, attributeName]) <- as.factor(levelsToChange)
      }
    } else {
      # Check for zero variance among numeric attributes
      if (sd(dataset[, attributeName], na.rm = TRUE) == 0) {
        dataset[, attributeName] <- NULL  # Remove attribute if no variance
      } else {
        # Normalize numeric data using standard scaling
        dataset[, attributeName] <- normalizeData(dataset[, attributeName], type = "norm")
      }
    }
  }
  
  # Reintroduce the class attribute to the dataset
  processedData <- cbind(dataset, Class = classData)
  
  # Eliminate any duplicate rows in the dataset
  if (any(duplicated(processedData))) {
    processedData <- processedData[!duplicated(processedData), ]
  }
  
  # Clean and format the dataset headers
  colnames(processedData)[ncol(processedData)] <- "Class"
  processedData$Class <- as.factor(processedData$Class)
  rownames(processedData) <- NULL  # Remove row names for a cleaner dataset
  
  return(processedData)
}
