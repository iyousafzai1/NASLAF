setwd("D:/Current_Paper_Experiments/NASLAF/data/datasets")

files <- list.files(pattern = "\\.rda$")

# Function to load, apply noise, and save the dataset
process_dataset <- function(file_name, noise_rate) {
  # Load the dataset
  envir <- new.env()
  load(file_name, envir)
  dataset_names <- ls(envir)
  
  # Loop through all datasets in the file
  for (dataset_name in dataset_names) {
    dataset <- get(dataset_name, envir)
    
    # Assuming 'y' is the last column and 'x' consists of all other columns
    x <- dataset[, -ncol(dataset), drop = FALSE]
    y <- dataset[, ncol(dataset), drop = FALSE]
    
    # Apply noise and save
    tryCatch({
      y_noisy <- neighborwise.default(x, y, rate = noise_rate)
      dataset[, ncol(dataset)] <- y_noisy
      save(dataset, file = paste0("noisy_", dataset_name, ".rda"))
    }, error = function(e) {
      cat("Error processing ", dataset_name, " in ", file_name, ": ", e$message, "\n")
    })
  }
}

# Apply to all files
sapply(files, process_dataset, noise_rate = 0.3)
