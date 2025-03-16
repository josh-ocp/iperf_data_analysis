library(tidyverse)
library(lubridate)
library(zoo)  # For rolling averages

#' Calculate summary statistics for iperf3 test data
#'
#' @param data Tibble of processed TCP iperf3 data
#' @return Tibble with summary statistics
summarize_iperf_data <- function(data) {
  data %>%
    group_by(file, user_name, isp) %>%
    summarize(
      # Basic statistics
      avg_bitrate_mbps = mean(bitrate_mbps),
      min_bitrate_mbps = min(bitrate_mbps),
      max_bitrate_mbps = max(bitrate_mbps),
      sd_bitrate_mbps = sd(bitrate_mbps),
      test_duration_sec = max(interval_end),
      
      # Additional statistics
      median_bitrate_mbps = median(bitrate_mbps),
      pct_95_bitrate_mbps = quantile(bitrate_mbps, 0.95),
      stability_cv = sd(bitrate_mbps) / mean(bitrate_mbps) * 100, # Coefficient of variation (%)
      
      # Count of intervals
      interval_count = n(),
      
      .groups = "drop"
    )
}

#' Segment analysis - break down performance by time periods
#'
#' @param data Tibble of processed TCP iperf3 data
#' @param segment_size Size of each segment in seconds (default: 10)
#' @return Tibble with segment statistics
analyze_segments <- function(data, segment_size = 10) {
  # Add segment identifier based on interval_start
  data_with_segments <- data %>%
    mutate(segment = floor(interval_start / segment_size))
  
  # Analyze each segment
  segment_analysis <- data_with_segments %>%
    group_by(file, user_name, isp, segment) %>%
    summarize(
      start_time = min(interval_start),
      end_time = max(interval_end),
      avg_bitrate_mbps = mean(bitrate_mbps),
      stability_cv = sd(bitrate_mbps) / mean(bitrate_mbps) * 100,
      interval_count = n(),
      .groups = "drop"
    )
  
  return(segment_analysis)
}

#' Analyze throughput stability
#'
#' @param data Tibble of processed TCP iperf3 data
#' @param window_size Size of rolling window for calculations (default: 5)
#' @return Tibble with stability metrics
analyze_stability <- function(data, window_size = 5) {
  # Sort by interval_start to ensure correct time sequence
  data_sorted <- data %>%
    arrange(interval_start)
  
  # Calculate rolling statistics if we have enough data points
  if(nrow(data_sorted) >= window_size) {
    stability_metrics <- data_sorted %>%
      mutate(
        # Rolling average and standard deviation
        rolling_avg_mbps = rollmean(bitrate_mbps, window_size, fill = NA, align = "right"),
        rolling_sd_mbps = rollapply(bitrate_mbps, window_size, sd, fill = NA, align = "right"),
        
        # Calculate trend (is throughput increasing or decreasing?)
        delta_mbps = c(NA, diff(bitrate_mbps)),
        
        # Calculate stability coefficient (rolling CV)
        rolling_cv = rolling_sd_mbps / rolling_avg_mbps * 100
      )
    
    return(stability_metrics)
  } else {
    warning("Not enough data points for stability analysis. Need at least ", window_size, " intervals.")
    return(data_sorted %>% mutate(
      rolling_avg_mbps = NA,
      rolling_sd_mbps = NA,
      delta_mbps = NA,
      rolling_cv = NA
    ))
  }
}

#' Identify anomalies in the throughput data
#'
#' @param data Tibble of processed TCP iperf3 data
#' @param threshold Number of standard deviations to consider as anomaly (default: 2)
#' @return Tibble with anomaly indicators
detect_anomalies <- function(data, threshold = 2) {
  # Calculate overall statistics
  overall_mean <- mean(data$bitrate_mbps)
  overall_sd <- sd(data$bitrate_mbps)
  
  # Mark anomalies
  data_with_anomalies <- data %>%
    mutate(
      # Calculate z-score
      z_score = abs(bitrate_mbps - overall_mean) / overall_sd,
      
      # Flag anomalies
      is_anomaly = z_score > threshold,
      
      # Classify anomaly type
      anomaly_type = case_when(
        !is_anomaly ~ "normal",
        bitrate_mbps > overall_mean ~ "high_throughput",
        TRUE ~ "low_throughput"
      )
    )
  
  return(data_with_anomalies)
}

#' Compare performance between different tests
#'
#' @param data Tibble of processed TCP iperf3 data
#' @param group_var Variable to group by (e.g., "network", "time_of_day")
#' @return Analysis results
compare_tests <- function(data, group_var) {
  if(!group_var %in% names(data)) {
    stop(paste("Variable", group_var, "not found in data"))
  }
  
  comparison <- data %>%
    group_by(!!sym(group_var)) %>%
    summarize(
      avg_bitrate_mbps = mean(bitrate_mbps),
      sd_bitrate_mbps = sd(bitrate_mbps),
      cv_percent = sd_bitrate_mbps / avg_bitrate_mbps * 100,
      min_bitrate_mbps = min(bitrate_mbps),
      max_bitrate_mbps = max(bitrate_mbps),
      .groups = "drop"
    )
  
  return(comparison)
}

# Line 147 was empty, and line 148 had a stray '/' character
# Removing the problematic character

# Add a proper comment or closing statement here if needed
# If this was intended to be a comment, change it to R's comment style using #

#' Group and summarize iperf tests by date
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @return Dataframe with daily summary statistics
group_by_date <- function(tcp_data) {
  # First check if test_datetime exists
  if(!"test_datetime" %in% names(tcp_data)) {
    # Try to create test_datetime from other fields if available
    if("file" %in% names(tcp_data)) {
      # Extract date from filename following the pattern in the example files
      # Pattern: root_Ziply_Fiber__ash_iperf_test_20250316_075748.txt
      tcp_data <- tcp_data %>%
        mutate(
          extracted_date = str_extract(file, "\\d{8}_\\d{6}"),
          test_datetime = if_else(
            !is.na(extracted_date),
            as.POSIXct(
              paste0(
                substr(extracted_date, 1, 4), "-",
                substr(extracted_date, 5, 6), "-",
                substr(extracted_date, 7, 8), " ",
                substr(extracted_date, 10, 11), ":",
                substr(extracted_date, 12, 13), ":",
                substr(extracted_date, 14, 15)
              ), 
              format = "%Y-%m-%d %H:%M:%S"
            ),
            Sys.time() # Use current time as fallback
          )
        )
    } else {
      # If no usable information, add a dummy date
      warning("No datetime information available in data, using current date")
      tcp_data$test_datetime <- Sys.time()
    }
  }
  
  tcp_data %>%
    mutate(
      test_date = as.Date(test_datetime)
    ) %>%
    group_by(test_date) %>%
    summarize(
      avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
      median_throughput = median(bitrate_mbps, na.rm = TRUE),
      max_throughput = max(bitrate_mbps, na.rm = TRUE),
      min_throughput = min(bitrate_mbps, na.rm = TRUE),
      stability = sd(bitrate_mbps, na.rm = TRUE),
      stability_pct = sd(bitrate_mbps, na.rm = TRUE) / mean(bitrate_mbps, na.rm = TRUE) * 100,
      test_count = length(unique(file)),
      .groups = "drop"
    )
}

#' Group and summarize iperf tests by time of day
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param hour_bins Number of bins for grouping hours (default: 4, resulting in 6-hour periods)
#' @return Dataframe with time-of-day summary statistics
group_by_time_of_day <- function(tcp_data, hour_bins = 4) {
  tcp_data %>%
    mutate(
      hour = hour(test_datetime),
      time_period = case_when(
        hour >= 0 & hour < 6 ~ "Night (12AM-6AM)",
        hour >= 6 & hour < 12 ~ "Morning (6AM-12PM)",
        hour >= 12 & hour < 18 ~ "Afternoon (12PM-6PM)",
        hour >= 18 & hour < 24 ~ "Evening (6PM-12AM)"
      )
    ) %>%
    group_by(time_period) %>%
    summarize(
      avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
      median_throughput = median(bitrate_mbps, na.rm = TRUE),
      max_throughput = max(bitrate_mbps, na.rm = TRUE),
      min_throughput = min(bitrate_mbps, na.rm = TRUE),
      stability = sd(bitrate_mbps, na.rm = TRUE),
      test_count = length(unique(file)),
      .groups = "drop"
    ) %>%
    # Sort by time of day
    mutate(time_period = factor(time_period, 
                              levels = c("Morning (6AM-12PM)", 
                                         "Afternoon (12PM-6PM)",
                                         "Evening (6PM-12AM)", 
                                         "Night (12AM-6AM)"))) %>%
    arrange(time_period)
}

#' Detect anomalies in iperf tests
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param threshold_z Standard deviations from mean to consider an anomaly
#' @return Dataframe with identified anomalies
detect_anomalies <- function(tcp_data, threshold_z = 2) {
  # Calculate overall stats
  overall_mean <- mean(tcp_data$bitrate_mbps, na.rm = TRUE)
  overall_sd <- sd(tcp_data$bitrate_mbps, na.rm = TRUE)
  
  # Identify tests with anomalous performance
  tcp_data %>%
    group_by(file) %>%
    summarize(
      avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
      max_throughput = max(bitrate_mbps, na.rm = TRUE),
      min_throughput = min(bitrate_mbps, na.rm = TRUE),
      z_score = (avg_throughput - overall_mean) / overall_sd,
      is_anomaly = abs(z_score) > threshold_z,
      direction = if_else(z_score > 0, "above average", "below average"),
      test_datetime = first(test_datetime),
      .groups = "drop"
    ) %>%
    filter(is_anomaly) %>%
    arrange(desc(abs(z_score)))
}

#' Calculate stability metrics for tests
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @return Dataframe with stability metrics for each test
calculate_stability_metrics <- function(tcp_data) {
  tcp_data %>%
    group_by(file) %>%
    summarize(
      avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
      median_throughput = median(bitrate_mbps, na.rm = TRUE),
      throughput_sd = sd(bitrate_mbps, na.rm = TRUE),
      cv = throughput_sd / avg_throughput * 100, # Coefficient of variation (%)
      range = max(bitrate_mbps, na.rm = TRUE) - min(bitrate_mbps, na.rm = TRUE),
      range_pct = range / avg_throughput * 100,
      test_datetime = first(test_datetime),
      user_name = first(user_name),
      isp = first(isp),
      .groups = "drop"
    ) %>%
    arrange(desc(cv)) # Sort by most variable first
}

#' Analyze performance trends over time across multiple tests
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param time_unit Unit for grouping time ('day', 'week', 'month', 'hour', etc.)
#' @return Dataframe with time trends analysis
analyze_time_trends <- function(tcp_data, time_unit = "day") {
  # Ensure we have datetime information
  if(is.null(tcp_data$test_datetime) || all(is.na(tcp_data$test_datetime))) {
    stop("Test datetime information is missing")
  }
  
  # Format time based on the requested unit
  tcp_data <- tcp_data %>%
    mutate(
      time_group = case_when(
        time_unit == "hour" ~ floor_date(test_datetime, "hour"),
        time_unit == "day" ~ as.Date(test_datetime),
        time_unit == "week" ~ floor_date(test_datetime, "week"),
        time_unit == "month" ~ floor_date(test_datetime, "month"),
        TRUE ~ as.Date(test_datetime)  # Default to day
      )
    )
  
  # Group and analyze by time period
  time_trends <- tcp_data %>%
    group_by(time_group) %>%
    summarize(
      test_count = n_distinct(file),
      user_count = n_distinct(user_name),
      avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
      median_throughput = median(bitrate_mbps, na.rm = TRUE),
      p95_throughput = quantile(bitrate_mbps, 0.95, na.rm = TRUE),
      stability = sd(bitrate_mbps, na.rm = TRUE),
      stability_pct = stability / avg_throughput * 100,
      data_points = n(),
      .groups = "drop"
    ) %>%
    arrange(time_group)
  
  # Calculate trend indicators
  if(nrow(time_trends) > 1) {
    time_trends <- time_trends %>%
      mutate(
        throughput_change = c(NA, diff(avg_throughput)),
        throughput_change_pct = throughput_change / lag(avg_throughput) * 100,
        trend = case_when(
          is.na(throughput_change) ~ "initial",
          throughput_change > 0 ~ "improving",
          throughput_change < 0 ~ "degrading",
          TRUE ~ "stable"
        )
      )
  }
  
  return(time_trends)
}

#' Compare performance between different time periods and users
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param period_var Variable to use for period grouping ('hour', 'day_of_week', 'month', etc.)
#' @param include_users Whether to break down by user (default: TRUE)
#' @return Dataframe comparing performance across time periods
compare_time_periods <- function(tcp_data, period_var = "hour", include_users = TRUE) {
  # Create period groupings
  tcp_data <- tcp_data %>%
    mutate(
      hour = hour(test_datetime),
      day_of_week = wday(test_datetime, label = TRUE),
      month = month(test_datetime, label = TRUE),
      quarter = quarter(test_datetime)
    )
  
  # Validate the requested period variable exists
  if(!period_var %in% c("hour", "day_of_week", "month", "quarter")) {
    stop("Invalid period_var. Use 'hour', 'day_of_week', 'month', or 'quarter'")
  }
  
  # Group data
  if(include_users && "user_name" %in% names(tcp_data)) {
    comparison <- tcp_data %>%
      group_by(user_name, isp, !!sym(period_var)) %>%
      summarize(
        avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
        median_throughput = median(bitrate_mbps, na.rm = TRUE),
        stability = sd(bitrate_mbps, na.rm = TRUE) / mean(bitrate_mbps, na.rm = TRUE) * 100,
        test_count = n_distinct(file),
        data_points = n(),
        .groups = "drop"
      )
  } else {
    comparison <- tcp_data %>%
      group_by(!!sym(period_var)) %>%
      summarize(
        avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
        median_throughput = median(bitrate_mbps, na.rm = TRUE),
        stability = sd(bitrate_mbps, na.rm = TRUE) / mean(bitrate_mbps, na.rm = TRUE) * 100,
        test_count = n_distinct(file),
        data_points = n(),
        .groups = "drop"
      )
  }
  
  return(comparison)
}