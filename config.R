#' Configuration settings for iperf data analysis
#'
#' This file contains configuration settings for the iperf data analysis project.
#' Edit these settings as needed before running the analysis scripts.

# Configuration for iPerf Data Analysis

# Define paths
CONFIG <- list(
  input_dir = here::here("Input"),
  output_dir = here::here("output"),
  report_dir = here::here("output", "reports")
)

# Create directories if they don't exist
for(dir in CONFIG) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# Function to get input files with better error handling
process_input_files <- function() {
  input_dir <- CONFIG$input_dir
  message("Looking for iperf files in: ", input_dir)
  
  # Check if directory exists
  if(!dir.exists(input_dir)) {
    warning("Input directory does not exist: ", input_dir)
    dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
    return(character(0))
  }
  
  # Look for both text and CSV files (no JSON)
  files <- list.files(input_dir, 
                     pattern = "\\.(txt|log|csv)$", 
                     full.names = TRUE,
                     recursive = TRUE)
  
  if(length(files) == 0) {
    message("No iperf files found in: ", input_dir)
  } else {
    message("Found ", length(files), " iperf files")
  }
  
  return(files)
}

# Whether to copy files to the project structure or just reference them
COPY_FILES_TO_PROJECT <- TRUE

# Default directories within the project
RAW_DATA_DIR <- here::here("data", "raw")
PROCESSED_DATA_DIR <- here::here("data", "processed")
OUTPUT_FIGURES_DIR <- here::here("output", "figures")
OUTPUT_REPORTS_DIR <- here::here("output", "reports")

# Analysis settings
PLOT_WIDTH <- 10
PLOT_HEIGHT <- 6
MAX_TESTS_IN_COMPARISON <- 5  # Limit number of tests in comparison plots for readability