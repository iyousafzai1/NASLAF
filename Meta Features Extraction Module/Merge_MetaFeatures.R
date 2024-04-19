# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

# Function to list all datasets in the 'mfeats' directory, bind features into a data frame,
# return it, and save it to disk.

joinMfeats <- function() {
  # Define the sets of features expected to be present in each dataset
  feat.sets <- c("StatFeatures", "InfotheoFeatures", "ComplexityFeatures")
  
  # List all dataset directories within the 'mfeats' folder
  data.files <- list.files(path = "mfeats/")
  
  # Process each dataset to extract and combine all relevant features
  aux <- lapply(data.files, function(dataset) {
    cat("Processing dataset:", dataset, "\n")
    
    # Load each feature set for the current dataset
    inner.aux <- lapply(feat.sets, function(feat.file) {
      feat.path <- paste0("mfeats/", dataset, "/", feat.file, ".RData")
      if (!file.exists(feat.path)) {
        cat("File not found:", feat.path, "\n")
        return(NULL)
      } else {
        cat("Loading feature set:", feat.path, "\n")
        loaded_data <- load(feat.path)
        loaded_features <- get(loaded_data)
        if (is.list(loaded_features)) {
          # Flatten the list of features and rename to include the element name as prefix
          feat.aux <- lapply(names(loaded_features), function(elem) {
            values <- loaded_features[[elem]]
            names(values) <- paste(elem, names(values), sep = ".")
            return(values)
          })
          return(Reduce(c, feat.aux))
        }
        return(loaded_features)
      }
    })
    
    # Combine all loaded features into a single data frame row per dataset
    meta.example <- data.frame(t(unlist(Reduce(c, inner.aux))))
    return(meta.example)
  })
  
  # Combine all datasets' meta-features into a single data frame
  meta.features.df <- do.call(plyr::rbind.fill, aux)
  meta.features.df <- cbind(data.files, meta.features.df)
  
  # Save the combined features to disk
  n <- nrow(meta.features.df)
  m <- ncol(meta.features.df) - 1
  
  csv_filename <- paste0("combined_meta_features_", n, "_datasets_", m, "_features.csv")
  rdata_filename <- paste0("combined_meta_features_", n, "_datasets_", m, "_features.RData")
  
  write.csv(meta.features.df, file = csv_filename, row.names = FALSE)
  save(meta.features.df, file = rdata_filename)
  
  cat("Saved combined features to CSV and RData files.\n")
}

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------

# Execute the function to join and save meta-features
joinMfeats()

# -------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------
