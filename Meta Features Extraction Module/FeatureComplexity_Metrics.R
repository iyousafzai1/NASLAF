# Load necessary libraries
library(dplyr)
library(ECoL)
library(RWeka)

# Set the primary working directory for the project
setwd("D:/Current_Paper_Experiments/NASLAF")

# Function to read and preprocess the dataset
read_and_preprocess <- function(file_path) {
  # Load the dataset from an RDA file
  load(file_path)
  # Assume the data is loaded into a variable named 'data'. If the name varies, this needs to be adjusted accordingly.
  
  # Preprocessing: Remove columns with only one unique value to reduce model bias
  preprocessed_data <- data %>% select_if(~ length(unique(.)) > 1)
  
  return(preprocessed_data)
}

# Function to calculate and return minority classes
get_minority_classes <- function(data) {
  # Calculate frequency of each class
  class_frequency <- as.data.frame(table(data$Class))
  
  # Identify minority classes with only one instance
  minority_classes <- which(class_frequency$Freq == 1)
  
  return(minority_classes)
}

# Function to calculate complexity features
calculate_complexity <- function(data) {
  # Calculate complexity features using the ECoL package
  complexity_features <- complexity(Class ~ ., data, type = "class")
  transformed_features <- as.data.frame(t(complexity_features))
  
  return(transformed_features)
}

# Function to handle directory creation and data saving
save_features <- function(features, dir_name, file_name) {
  # Create directory if it doesn't exist
  dir_path <- paste0("mfeats/", dir_name)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Save features in the specified path
  save_path <- paste0(dir_path, "/", file_name, ".RData")
  save(features, file = save_path)
  
  # Log saved file information
  message(paste("Features saved in:", save_path))
}

# Main function to process all datasets
process_all_datasets <- function() {
  # Retrieve dataset names without the file extension
  dataset_names <- gsub(".rda", "", list.files(path = "data/datasets/"))
  
  # Initialize a dataframe to compile all features
  compiled_features <- data.frame()
  
  # Process each dataset individually
  for (name in dataset_names) {
    message(paste("Processing dataset:", name))
    
    file_path <- paste0("data/datasets/", name, ".rda")
    data <- read_and_preprocess(file_path)
    
    # Skip processing if data has no valid features
    if (ncol(data) == 0) {
      message(paste("Skipping dataset due to lack of variability:", name))
      next
    }
    
    minority_classes <- get_minority_classes(data)
    data <- data[!data$Class %in% minority_classes, ]
    
    features <- calculate_complexity(data)
    
    save_features(features, name, "ComplexityFeatures")
    
    compiled_features <- rbind(compiled_features, features)
  }
  
  return(compiled_features)
}

# Execute the processing for all datasets and save the results
all_complexity_features <- process_all_datasets()
save(all_complexity_features, file = "AllComplexityFeatures.RData")
