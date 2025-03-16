# Enhanced iperf3 Data Analysis Script for TCP Tests

# Source centralized dependency management
source(here::here("install_dependencies.R"))

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
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "reports"), recursive = TRUE, showWarnings = FALSE)

# Process files from the external input directory
process_input_files <- function() {
  cat("Looking for iperf files in:", IPERF_INPUT_DIR, "\n")
  
  # Check if input directory exists
  if (!dir.exists(IPERF_INPUT_DIR)) {
    stop("Input directory does not exist. Please create it or update the config.R file.")
  }
  
  # Get all CSV files in the input directory
  input_files <- list.files(IPERF_INPUT_DIR, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(input_files) == 0) {
    cat("No CSV files found in the input directory.\n")
    return(character(0))
  }
  
  cat("Found", length(input_files), "iperf CSV files.\n")
  
  # Copy files to project structure if configured to do so
  if (COPY_FILES_TO_PROJECT) {
    cat("Copying files to project structure...\n")
    
    for (file in input_files) {
      dest <- here("data", "raw", "tcp", basename(file))
      file.copy(file, dest, overwrite = TRUE)
    }
    
    # Use the local copies for processing
    input_files <- list.files(here("data", "raw", "tcp"), pattern = "\\.csv$", full.names = TRUE)
  }
  
  return(input_files)
}

# Main processing function for TCP data
process_tcp_data <- function(tcp_files) {
  if (length(tcp_files) == 0) {
    cat("No iperf files to process.\n")
    return(NULL)
  }
  
  cat("Processing iperf data...\n")
  
  # Parse the raw CSV files
  tcp_data <- process_iperf_files(tcp_files)
  if (is.null(tcp_data) || nrow(tcp_data) == 0) {
    cat("No valid data found in files.\n")
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
    
    # Use human-readable datetime if available for better titles
    if("datetime_readable" %in% names(single_test)) {
      test_date <- first(single_test$datetime_readable)
      title <- paste("TCP Throughput:", file, "-", test_date)
    } else {
      title <- paste("TCP Throughput:", file)
    }
    
    p <- plot_throughput(single_test, title)
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
    segment_size <- min(10, max(tcp_data$interval_end, na.rm = TRUE) / 5) # Ensure at least 5 segments
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
  
  # Create the enhanced visualization
  if(nrow(tcp_data) > 0) {
    # Add human-readable datetime to title if available
    if("datetime_readable" %in% names(tcp_data) && length(unique(tcp_data$file)) == 1) {
      test_date <- first(tcp_data$datetime_readable)
      title <- paste("TCP Test:", unique(tcp_data$file), "-", test_date)
    } else {
      title <- paste("TCP Test:", paste(unique(tcp_data$file), collapse=", "))
    }
    
    p_detailed <- plot_throughput_detailed(tcp_data, title)
    ggsave(here("output", "figures", "tcp_detailed_analysis.png"), 
           p_detailed, width = 15, height = 10, dpi = 300)
    
    # For larger user-friendly viewing
    ggsave(here("output", "figures", "tcp_detailed_analysis_large.png"), 
           p_detailed, width = 20, height = 15, dpi = 150)
  }
  
  # Create additional visualizations and reports
  if (!is.null(tcp_data) && nrow(tcp_data) > 0) {
    cat("Generating enhanced visualizations...\n")
    
    # Create dashboard and save it
    dashboard <- create_performance_dashboard(tcp_data)
    ggsave(here("output", "figures", "network_performance_dashboard.png"), 
           dashboard, width = 14, height = 10)
    
    # Generate stability analysis
    stability_metrics <- calculate_stability_metrics(tcp_data)
    write_csv(stability_metrics, here("output", "reports", "stability_metrics.csv"))
    
    # Generate time-of-day analysis
    tod_summary <- group_by_time_of_day(tcp_data)
    write_csv(tod_summary, here("output", "reports", "time_of_day_summary.csv"))
    
    # Check for anomalies
    anomalies <- detect_anomalies(tcp_data)
    if (nrow(anomalies) > 0) {
      write_csv(anomalies, here("output", "reports", "performance_anomalies.csv"))
      cat("Note: Detected", nrow(anomalies), "anomalous test(s).\n")
    }
    
    cat("Enhanced visualizations and reports created successfully.\n")
  }
  
  # Time-based analysis - use datetime field safely
  if ("test_datetime" %in% names(tcp_data) && 
      !all(is.na(tcp_data$test_datetime)) && 
      length(unique(tcp_data$test_datetime[!is.na(tcp_data$test_datetime)])) >= 2) {
    
    cat("Generating time-based comparisons...\n")
    
    # Time series comparison
    p_time_series <- plot_time_series_comparison(tcp_data, color_by = "file", max_series = 8)
    ggsave(here("output", "figures", "time_series_comparison.png"), 
           p_time_series, width = 12, height = 10)
    
    # Performance trends over time
    p_trends <- plot_performance_trends(tcp_data, time_unit = "day")
    ggsave(here("output", "figures", "performance_trends.png"), 
           p_trends, width = 12, height = 8)
    
    # Time patterns heatmap
    p_patterns <- plot_time_pattern_heatmap(tcp_data)
    ggsave(here("output", "figures", "time_pattern_heatmap.png"), 
           p_patterns, width = 10, height = 8)
    
    # Generate time period comparisons
    time_periods <- compare_time_periods(tcp_data, period_var = "hour", include_users = TRUE)
    write_csv(time_periods, here("output", "reports", "time_period_comparison.csv"))
    
    # Time trends analysis
    time_trends <- analyze_time_trends(tcp_data, time_unit = "day")
    write_csv(time_trends, here("output", "reports", "time_trends.csv"))
    
    cat("Time-based analysis complete!\n")
  }
  
  # If we have data from multiple users
  if(!is.null(tcp_data) && "user_name" %in% names(tcp_data) && length(unique(tcp_data$user_name)) > 1) {
    cat("Generating multi-user time comparisons...\n")
    
    # User time series comparison
    p_user_time <- plot_time_series_comparison(tcp_data, color_by = "user_name")
    ggsave(here("output", "figures", "user_time_series.png"), 
           p_user_time, width = 12, height = 10)
    
    # User trends
    p_user_trends <- plot_performance_trends(tcp_data, facet_by = "user_name")
    ggsave(here("output", "figures", "user_performance_trends.png"), 
           p_user_trends, width = 14, height = 10)
    
    # User time patterns
    p_user_patterns <- plot_time_pattern_heatmap(tcp_data, facet_by = "user_name")
    ggsave(here("output", "figures", "user_time_patterns.png"), 
           p_user_patterns, width = 14, height = 10)
  }
  
  return(tcp_data)
}

# Generate summary report for TCP tests with enhanced metadata
create_summary_report <- function(tcp_data = NULL) {
  cat("Generating summary report...\n")
  
  # Create report content
  report <- c("# iperf3 TCP Data Analysis Summary Report", 
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
    
    # Include ISP information if available
    isp_text <- ""
    if ("isp" %in% names(tcp_data)) {
      unique_isps <- unique(tcp_data$isp)
      isp_text <- paste0("* ISPs tested: ", paste(unique_isps, collapse=", "), "\n")
    }
    
    # Include date range if available
    date_range_text <- ""
    if ("datetime_readable" %in% names(tcp_data) && !all(is.na(tcp_data$datetime_readable))) {
      date_values <- na.omit(tcp_data$datetime_readable)
      if (length(date_values) > 0) {
        date_range_text <- paste0("* Test period: ", min(date_values), " to ", max(date_values), "\n")
      }
    } else if ("test_datetime" %in% names(tcp_data) && !all(is.na(tcp_data$test_datetime))) {
      date_values <- na.omit(tcp_data$test_datetime)
      if (length(date_values) > 0) {
        date_range_text <- paste0("* Test period: ", 
                                format(min(date_values), "%Y-%m-%d %H:%M:%S"), " to ", 
                                format(max(date_values), "%Y-%m-%d %H:%M:%S"), "\n")
      }
    }
    
    report <- c(report,
                "### TCP Tests",
                paste0("* Number of TCP tests: ", length(tcp_files)),
                paste0("* Files analyzed: ", paste(tcp_files, collapse=", ")),
                user_text,
                isp_text,
                date_range_text,
                paste0("* Total data points: ", nrow(tcp_data)),
                paste0("* Average throughput: ", round(mean(tcp_data$bitrate_mbps, na.rm = TRUE), 2), " Mbps"),
                paste0("* Max throughput: ", round(max(tcp_data$bitrate_mbps, na.rm = TRUE), 2), " Mbps"),
                "")
    
    # Add general information to report
    report <- c(report,
                paste0("* Total files analyzed: ", length(tcp_files)),
                paste0("* Data points: ", nrow(tcp_data)),
                user_text,
                isp_text,
                "", 
                "## Test Summary",
                paste0("* Average throughput: ", round(mean(tcp_data$bitrate_mbps, na.rm = TRUE), 2), " Mbps"),
                paste0("* Median throughput: ", round(median(tcp_data$bitrate_mbps, na.rm = TRUE), 2), " Mbps"),
                paste0("* Standard deviation: ", round(sd(tcp_data$bitrate_mbps, na.rm = TRUE), 2), " Mbps"),
                paste0("* CV (stability): ", round(sd(tcp_data$bitrate_mbps, na.rm = TRUE) / mean(tcp_data$bitrate_mbps, na.rm = TRUE) * 100, 2), "%"),
                "")
  }
  
  # Enhanced reporting of output files
  report <- c(report,
              "## Output Files",
              "",
              "### Data Files",
              "* TCP raw data: `data/processed/tcp_data.rds`",
              "* TCP detailed report: `output/reports/tcp_detailed.csv`",
              "* TCP summary stats: `output/reports/tcp_summary.csv`",
              ifelse(file.exists(here("output", "reports", "time_period_comparison.csv")), 
                    "* Time period analysis: `output/reports/time_period_comparison.csv`", ""),
              ifelse(file.exists(here("output", "reports", "stability_metrics.csv")),
                    "* Stability metrics: `output/reports/stability_metrics.csv`", ""),
              ifelse(file.exists(here("output", "reports", "time_of_day_summary.csv")),
                    "* Time-of-day summary: `output/reports/time_of_day_summary.csv`", ""),
              ifelse(file.exists(here("output", "reports", "user_summary.csv")),
                    "* User summary: `output/reports/user_summary.csv`", ""),
              "",
              "### Visualizations",
              "* Individual test throughput: `output/figures/tcp_*.png`",
              "* Network dashboard: `output/figures/network_performance_dashboard.png`",
              ifelse(file.exists(here("output", "figures", "tcp_heatmap.png")),
                    "* Performance heatmaps: `output/figures/tcp_heatmap.png`", ""),
              ifelse(file.exists(here("output", "figures", "tcp_anomalies.png")),
                    "* Anomaly detection: `output/figures/tcp_anomalies.png`", ""),
              ifelse(file.exists(here("output", "figures", "user_comparison.png")),
                    "* User comparisons: `output/figures/user_comparison.png`", ""),
              ifelse(file.exists(here("output", "figures", "time_series_comparison.png")),
                    "* Time series: `output/figures/time_series_comparison.png`", ""),
              ifelse(file.exists(here("output", "figures", "performance_trends.png")),
                    "* Performance trends: `output/figures/performance_trends.png`", ""),
              "")
  
  # Remove empty strings
  report <- report[report != ""]
  
  # Write report to file
  writeLines(report, here("output", "reports", "analysis_summary.md"))
  cat("Summary report saved to:", here("output", "reports", "analysis_summary.md"), "\n")
}

# MAIN EXECUTION FLOW
cat("\n========== iperf TCP Network Analysis ==========\n")

# Process input files
tcp_files <- process_input_files()

# Process TCP data
tcp_data <- process_tcp_data(tcp_files)

# Generate summary report
create_summary_report(tcp_data)

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
        avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
        median_throughput = median(bitrate_mbps, na.rm = TRUE),
        min_throughput = min(bitrate_mbps, na.rm = TRUE),
        max_throughput = max(bitrate_mbps, na.rm = TRUE),
        test_count = n_distinct(file),
        data_points = n(),
        # Use date_formatted if available
        test_date = if("date_formatted" %in% names(.)) first(date_formatted) else as.character(first(test_datetime)),
        .groups = "drop"
      )
    
    write_csv(user_summary, here("output", "reports", "user_summary.csv"))
  }
}

cat("\nAnalysis complete! All results saved to the output directory.\n")
cat("Check the summary report at:", here("output", "reports", "analysis_summary.md"), "\n")
cat("=============================================\n\n")

# Find and process TCP test files
process_all_tcp_data <- function(file_pattern = "\\.(txt|csv)$") {
  cat("Looking for TCP test files...\n")
  
  # Get file paths using the file pattern
  files <- process_input_files()
  
  # Filter by pattern if specified
  if (!is.null(file_pattern)) {
    files <- files[grep(file_pattern, files)]
  }
  
  if (length(files) == 0) {
    cat("No TCP test files found matching pattern.\n")
    return(NULL)
  }
  
  cat("Found", length(files), "TCP test files.\n")
  
  # Process the TCP files
  tcp_data <- process_tcp_data(files)
  
  # ...existing code...
}

# ...existing code...