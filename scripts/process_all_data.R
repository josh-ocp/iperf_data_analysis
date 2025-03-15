# Enhanced iperf3 Data Analysis Script

# Check and install required packages
required_packages <- c("tidyverse", "jsonlite", "fs", "patchwork", "scales", "zoo", "lubridate")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse=", "), "\n")
  install.packages(missing_packages)
}

# Load required libraries
library(tidyverse)
library(here)
library(jsonlite)
library(fs)      # For file operations
library(patchwork) # For combining plots
library(scales)  # For nice formatting
library(zoo)     # For rolling averages
library(lubridate) # For date handling

# Load configuration and utility functions
source(here("config.R"))
source(here("R", "parse_iperf.R"))
source(here("R", "analyze_iperf.R"))
source(here("R", "visualize_iperf.R"))

# Create directory structure if it doesn't exist
dir.create(here("data", "raw", "tcp"), recursive = TRUE, showWarnings = FALSE)
# dir.create(here("data", "raw", "udp"), recursive = TRUE, showWarnings = FALSE) # No longer needed
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "reports"), recursive = TRUE, showWarnings = FALSE)

# Function to determine if an iperf file is TCP or UDP
identify_protocol <- function(file_path) {
  tryCatch({
    data <- fromJSON(file_path)
    if (data$start$test_start$protocol == "UDP") {
      return("UDP")
    } else {
      return("TCP")
    }
  }, error = function(e) {
    warning(paste("Could not parse file:", file_path, "-", e$message))
    return(NA)
  })
}

# Process files from the external input directory (simplify)
process_input_files <- function() {
  cat("Looking for iperf TCP files in:", IPERF_INPUT_DIR, "\n")
  
  # Check if input directory exists
  if (!dir.exists(IPERF_INPUT_DIR)) {
    stop("Input directory does not exist. Please create it or update the config.R file.")
  }
  
  # Get all JSON files in the input directory
  input_files <- list.files(IPERF_INPUT_DIR, pattern = "\\.json$", full.names = TRUE)
  
  if (length(input_files) == 0) {
    cat("No JSON files found in the input directory.\n")
    return(character(0))
  }
  
  cat("Found", length(input_files), "JSON files.\n")
  
  # Validate that files are TCP
  tcp_files <- character(0)
  for (file in input_files) {
    # Try to confirm it's TCP
    protocol <- identify_protocol(file)
    if (!is.na(protocol) && protocol == "TCP") {
      tcp_files <- c(tcp_files, file)
    } else {
      warning(paste("Skipping non-TCP file:", file))
    }
  }
  
  cat("- Valid TCP files:", length(tcp_files), "\n")
  
  # Copy files to project structure if configured to do so
  if (COPY_FILES_TO_PROJECT) {
    cat("Copying files to project structure...\n")
    
    # Copy TCP files
    for (file in tcp_files) {
      dest <- here("data", "raw", "tcp", basename(file))
      file.copy(file, dest, overwrite = TRUE)
    }
    
    # Use the local copies for processing
    tcp_files <- list.files(here("data", "raw", "tcp"), pattern = "\\.json$", full.names = TRUE)
  }
  
  return(tcp_files)
}

# Main processing function for TCP data
process_tcp_data <- function(tcp_files) {
  if (length(tcp_files) == 0) {
    cat("No TCP files to process.\n")
    return(NULL)
  }
  
  cat("Processing TCP data...\n")
  
  # Parse the raw JSON files
  tcp_data <- process_iperf_files(tcp_files)
  if (is.null(tcp_data) || nrow(tcp_data) == 0) {
    cat("No valid TCP data found in files.\n")
    return(NULL)
  }
  
  # Save the raw processed data
  saveRDS(tcp_data, here("data", "processed", "tcp_data.rds"))
  
  # Basic analysis
  cat("Generating basic TCP analysis...\n")
  
  # Generate summary statistics
  tcp_summary <- summarize_iperf_data(tcp_data)
  write_csv(tcp_summary, here("output", "reports", "tcp_summary.csv"))
  
  # Export detailed data as CSV
  write_csv(tcp_data, here("output", "reports", "tcp_detailed.csv"))
  
  # Generate basic visualizations
  cat("Creating basic TCP visualizations...\n")
  for (file in unique(tcp_data$file)) {
    single_test <- tcp_data %>% filter(file == !!file)
    p <- plot_throughput(single_test, paste("TCP Throughput:", file))
    ggsave(here("output", "figures", paste0("tcp_", gsub("\\.json$", "", file), ".png")), 
           p, width = 10, height = 8)
  }
  
  # Create comparison plot if we have multiple tests
  if (length(unique(tcp_data$file)) > 1) {
    cat("Creating TCP comparison visualizations...\n")
    tcp_comparison <- plot_test_comparison(tcp_data)
    ggsave(here("output", "figures", "tcp_comparison.png"), tcp_comparison, width = 10, height = 12)
  }
  
  # Enhanced analysis
  cat("Performing enhanced TCP analysis...\n")
  
  # Stability analysis - if we have enough data points
  if (nrow(tcp_data) >= 5) {
    tcp_stability <- analyze_stability(tcp_data)
    saveRDS(tcp_stability, here("data", "processed", "tcp_stability.rds"))
    write_csv(tcp_stability, here("output", "reports", "tcp_stability.csv"))
  }
  
  # Segment analysis - if we have enough data for at least 2 segments
  if (nrow(tcp_data) >= 10) {
    segment_size <- min(10, max(tcp_data$interval_end) / 5) # Ensure at least 5 segments
    tcp_segments <- analyze_segments(tcp_data, segment_size)
    saveRDS(tcp_segments, here("data", "processed", "tcp_segments.rds"))
    write_csv(tcp_segments, here("output", "reports", "tcp_segments.csv"))
    
    # Create segment visualization
    p_heatmap <- plot_performance_heatmap(tcp_data, segment_size)
    ggsave(here("output", "figures", "tcp_heatmap.png"), p_heatmap, width = 10, height = 6)
  }
  
  # Anomaly detection
  if (nrow(tcp_data) >= 5) {
    tcp_anomalies <- detect_anomalies(tcp_data)
    saveRDS(tcp_anomalies, here("data", "processed", "tcp_anomalies.rds"))
    write_csv(tcp_anomalies, here("output", "reports", "tcp_anomalies.csv"))
    
    # Create visualization of anomalies
    p_anomalies <- tcp_anomalies %>%
      ggplot(aes(x = interval_start, y = bitrate_mbps, color = anomaly_type)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_manual(values = c("normal" = "darkgreen", 
                                   "high_throughput" = "blue", 
                                   "low_throughput" = "red")) +
      labs(title = "TCP Throughput Anomalies", 
           subtitle = paste("Threshold:", 2, "standard deviations"),
           x = "Time (seconds)", y = "Throughput (Mbps)") +
      theme_minimal()
    
    ggsave(here("output", "figures", "tcp_anomalies.png"), p_anomalies, width = 10, height = 6)
  }
  
  return(tcp_data)
}

# Generate aggregated report (simplified for TCP only)
create_summary_report <- function(tcp_data = NULL) {
  cat("Generating summary report...\n")
  
  # Create report content
  report <- c("# iperf TCP Data Analysis Summary Report", 
              paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
              "",
              "## Analysis Overview",
              "")
  
  if (!is.null(tcp_data)) {
    tcp_files <- unique(tcp_data$file)
    
    # Get user counts if available
    user_text <- ""
    if ("user_name" %in% names(tcp_data)) {
      unique_users <- unique(tcp_data$user_name)
      user_text <- paste0("* Number of users: ", length(unique_users), "\n* Users: ", 
                         paste(unique_users, collapse=", "))
    }
    
    report <- c(report,
                "### TCP Tests",
                paste0("* Number of TCP tests: ", length(tcp_files)),
                paste0("* Files analyzed: ", paste(tcp_files, collapse=", ")),
                user_text,
                paste0("* Total data points: ", nrow(tcp_data)),
                paste0("* Average throughput: ", round(mean(tcp_data$bitrate_mbps), 2), " Mbps"),
                paste0("* Max throughput: ", round(max(tcp_data$bitrate_mbps), 2), " Mbps"),
                "")
  }
  
  report <- c(report,
              "## Output Files",
              "",
              "### Data Files",
              "* TCP raw data: `data/processed/tcp_data.rds`",
              "* TCP detailed report: `output/reports/tcp_detailed.csv`",
              "* TCP summary stats: `output/reports/tcp_summary.csv`",
              "",
              "### Visualizations",
              "* Individual test throughput: `output/figures/tcp_*.png`",
              "* Performance heatmaps: `output/figures/tcp_heatmap.png`",
              "* Anomaly detection: `output/figures/tcp_anomalies.png`",
              "* User comparisons: `output/figures/user_comparison.png`",
              "")
  
  # Write report to file
  writeLines(report, here("output", "reports", "analysis_summary.md"))
  cat("Summary report saved to:", here("output", "reports", "analysis_summary.md"), "\n")
}

# MAIN EXECUTION FLOW (simplified)
cat("\n========== iperf TCP Data Analysis ==========\n")

# Process input files
tcp_files <- process_input_files()

# Process TCP data
tcp_data <- process_tcp_data(tcp_files)

# Generate summary report with just TCP data
create_summary_report(tcp_data, NULL)  # Passing NULL for UDP data

# Create user comparisons if we have multiple users
if(!is.null(tcp_data) && nrow(tcp_data) > 0 && "user_name" %in% names(tcp_data)) {
  unique_users <- unique(tcp_data$user_name)
  if(length(unique_users) > 1) {
    cat("Creating user comparison visualizations...\n")
    
    # User comparison
    user_comparison <- plot_user_comparison(tcp_data)
    ggsave(here("output", "figures", "user_comparison.png"), 
           user_comparison, width = 12, height = 15)
    
    # Also create a CSV report summarizing by user and ISP
    user_summary <- tcp_data %>%
      group_by(user_name, isp) %>%
      summarize(
        avg_throughput = mean(bitrate_mbps),
        median_throughput = median(bitrate_mbps),
        min_throughput = min(bitrate_mbps),
        max_throughput = max(bitrate_mbps),
        test_count = n_distinct(file),
        data_points = n(),
        test_date = first(test_date),
        .groups = "drop"
      )
    
    write_csv(user_summary, here("output", "reports", "user_summary.csv"))
  }
}

cat("\nAnalysis complete! All results saved to the output directory.\n")
cat("Check the summary report at:", here("output", "reports", "analysis_summary.md"), "\n")
cat("=============================================\n\n")