library(cluster)

# Define the distance function
dist <- function(x) {
  as.matrix(daisy(x, metric="gower", stand=TRUE))
}

# Define intra-cluster distance
intra <- function(x, y, d, i) {
  tmp <- rownames(x[y == y[i],])
  min(d[i, setdiff(tmp, i)])
}

# Define inter-cluster distance
inter <- function(x, y, d, i) {
  tmp <- rownames(x[y != y[i],])
  min(d[i, tmp])
}

# Calculate the noise index
n2 <- function(x, y) {
  d <- dist(x)
  aux <- sapply(1:nrow(x), function(i) {
    a <- intra(x, y, d, i)
    r <- inter(x, y, d, i)
    c(a, r)
  })
  colnames(aux) <- rownames(x)
  aux <- aux[1,]/aux[2,]
  return(aux)
}

# Neighborwise noise imputation
neighborwise.default <- function(x, y, rate, ...) {
  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }
  if(is.data.frame(y)) {
    y <- y[, 1]
  }
  y <- as.factor(y)
  if(min(table(y)) < 2) {
    stop("number of examples in the minority class must be >= 2")
  }
  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }
  if(rate < 0 | rate > 1) {
    stop("the noise rate must be higher than 0 and lower than 1")
  }
  rate <- trunc(nrow(x) * rate)
  noise <- n2(x, y)
  noise <- rev(order(noise))[1:rate]
  return(noise)  # Assuming you have a function `generate` to process `noise` and `y`
}

# Set the working directory
setwd("D:/Current_Paper_Experiments/NASLAF/data/datasets")

# List all RDA files in the directory
files <- list.files(pattern = "\\.rda$")

# Define a function to load, apply noise, and save the dataset
process_dataset <- function(file_name, noise_rate) {
  # Load the dataset
  envir <- new.env()
  load(file_name, envir)
  dataset_names <- ls(envir)
  
  for (dataset_name in dataset_names) {
    dataset <- get(dataset_name, envir)
    
    # Process dataset
    if (!is.data.frame(dataset)) {
      next  # Skip if not a data frame
    }
    x <- dataset[, -ncol(dataset), drop = FALSE]
    y <- dataset[, ncol(dataset), drop = FALSE]
    
    tryCatch({
      noise_indices <- neighborwise.default(x, y, rate = noise_rate)
      # Assuming 'generate' function to apply noise using the indices, modify `y` as needed
      # Update dataset with new `y`, save or further process as required
      save(dataset, file = paste0("noisy_", dataset_name, ".rda"))
    }, error = function(e) {
      cat("Error processing ", dataset_name, " in ", file_name, ": ", e$message, "\n")
    })
  }
}

# Apply to all files with a specified noise rate (e.g., 0.1 or 10% noise)
sapply(files, process_dataset, noise_rate = 0.1)
