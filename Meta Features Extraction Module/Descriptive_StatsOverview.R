
getMfeFeatures <- function(data) {
  # Common summary statistics for mfe feature extraction
  commonSummary <- c("kurtosis", "max", "mean", "median", "min", "sd", "skewness", "var", "hist")
  
  cat("   - Extracting mfe general features \n")
  # General features extraction with error handling
  general <- tryCatch({
    unlist(mfe::general(formula = as.formula("Class ~ ."), data = data, features = "all", summary = commonSummary))
  }, error = function(err) {
    cat("    * Error encountered in general features: returning empty vector ... \n")
    print(err)
    return(numeric(0))
  })
  
  cat("   - Extracting mfe statistical features \n")
  # Statistical features extraction with error handling
  statistical <- tryCatch({
    unlist(mfe::statistical(formula = as.formula("Class ~ ."), data = data, features = "all", summary = commonSummary))
  }, error = function(err) {
    cat("    * Error encountered in statistical features: returning empty vector ... \n")
    print(err)
    return(numeric(0))
  })
  
  cat("   - Extracting mfe information-theoretic features \n")
  # Information-theoretic features extraction with error handling
  infotheo <- tryCatch({
    unlist(mfe::infotheo(formula = as.formula("Class ~ ."), data = data, features = "all", summary = commonSummary))
  }, error = function(err) {
    cat("    * Error encountered in information-theoretic features: returning empty vector ... \n")
    print(err)
    return(numeric(0))
  })
  
  # Discriminant features section is commented out due to stability issues (segmentation faults)
  # cat("   - Extracting mfe discriminant features \n")
  # discriminant <- tryCatch({
  #     unlist(mfe::discriminant(formula = as.formula("Class ~ ."), data = data,
  #     features = mfe::ls.discriminant()[-8], summary = commonSummary))
  # }, error = function(err) {
  #     cat("    * Error encountered in discriminant features: returning empty vector ... \n")
  #     print(err)
  #     return(numeric(0))
  # })
  
  # Combine all feature sets into a single object
  featureSets <- list(
    general = general,
    statistical = statistical,
    infotheo = infotheo
    # discriminant = discriminant  # Uncomment if stability issues are resolved
  )
  
  return(featureSets)
}
