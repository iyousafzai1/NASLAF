
library(dplyr)
library(purrr)
library(readr)
library(BayesFactor)
library(tidyr)
library(ggplot2)
library(lubridate)

# Define a function for detailed logging
log_message <- function(message) {
  cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", message, "\n"))
}

log_message("Starting the script...")

# Load the metadata from an RDS file
metadata_path <- "D:/Current_Paper_Experiments/NASLAF/metadata.rda"
if (!file.exists(metadata_path)) {
  stop("Metadata file does not exist: ", metadata_path)
}

metadata <- readRDS(metadata_path)
log_message("Metadata loaded successfully.")

# Check if necessary columns are present
required_columns <- c("Dataset", "Filter_Name", "RF_Trees", "Filter_Params", "F1_Score", "Workflow_ID")
missing_columns <- setdiff(required_columns, names(metadata))
if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse=", "))
}

log_message("Metadata structure validated successfully.")

# Define the ROPE interval
rope_bounds <- c(-1, 1)  # Define as needed

# Placeholder for the Bayes Sign Test with ROPE
bayes_sign_test_with_rope <- function(difference) {
  if (difference > 1) {
    return("Win")
  } else if (difference < -1) {
    return("Lose")
  } else {
    return("Draw")
  }
}

# Process each dataset
process_each_dataset <- function(data) {
  oracle_f1 <- max(data$F1_Score, na.rm = TRUE)
  data <- data %>%
    mutate(
      Oracle_F1 = oracle_f1,
      Percentage_Difference = (F1_Score - Oracle_F1) / Oracle_F1 * 100,
      Result = map_chr(Percentage_Difference, bayes_sign_test_with_rope)
    )
  return(data)
}

log_message("Processing each dataset individually...")

# Apply the function to each dataset
results <- metadata %>%
  group_by(Dataset) %>%
  group_split() %>%
  map(process_each_dataset)

# Combine results into one dataframe
final_results <- bind_rows(results)

# Output results
output_path <- "final_results.csv"
write_csv(final_results, output_path)
log_message(paste("Analysis results saved to", output_path))

# Create and save a summary visualization
log_message("Generating summary visualization...")
ggplot(final_results, aes(x = Dataset, fill = Result)) +
  geom_bar(position = "fill") +
  labs(title = "Summary of Results by Dataset",
       x = "Dataset",
       y = "Proportion",
       fill = "Result") +
  theme_minimal() ->
  summary_plot

ggsave("summary_results_plot.png", plot = summary_plot)
log_message("Summary plot saved as 'summary_results_plot.png'.")

# Final log entry
log_message("Script completed successfully.")
