#' Configuration settings for iperf data analysis
#'
#' This file contains configuration settings for the iperf data analysis project.
#' Edit these settings as needed before running the analysis scripts.

# Configuration for iperf data analysis

# Directory where input iperf JSON files are located
# Raw string syntax
IPERF_INPUT_DIR <- r"(G:\My Drive\Projects\Git Home\iperf_data_analysis\iperf_data_analysis\input)"

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