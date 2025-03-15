library(tidyverse)
library(lubridate)
library(zoo)  # For rolling averages

#' Calculate summary statistics for iperf3 test data
#'
#' @param data Tibble of processed iperf3 data
#' @return Tibble with summary statistics
summarize_iperf_data <- function(data) {
  result <- data %>%
    group_by(protocol, file, user_name, isp) %>%
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
  
  # Add UDP-specific metrics if available
  if("lost_percent" %in% names(data)) {
    udp_summary <- data %>%
      filter(protocol == "UDP") %>%
      group_by(protocol, file, user_name, isp) %>%
      summarize(
        # Basic UDP stats
        avg_jitter_ms = mean(jitter_ms),
        max_jitter_ms = max(jitter_ms),
        median_jitter_ms = median(jitter_ms),
        sd_jitter_ms = sd(jitter_ms),
        
        # Packet stats
        total_packets = sum(packets_total),
        lost_packets = sum(packets_lost),
        overall_loss_percent = lost_packets / total_packets * 100,
        
        # Max packet loss interval
        max_loss_percent = max(lost_percent),
        
        # Correlation between jitter and loss
        jitter_loss_correlation = cor(jitter_ms, lost_percent, use = "complete.obs"),
        
        .groups = "drop"
      )
    
    result <- result %>%
      left_join(udp_summary, by = c("protocol", "file", "user_name", "isp"))
  }
  
  return(result)
}

#' Segment analysis - break down performance by time periods
#'
#' @param data Tibble of processed iperf3 data
#' @param segment_size Size of each segment in seconds (default: 10)
#' @return Tibble with segment statistics
analyze_segments <- function(data, segment_size = 10) {
  # Add segment identifier based on interval_start
  data_with_segments <- data %>%
    mutate(segment = floor(interval_start / segment_size))
  
  # Analyze each segment
  segment_analysis <- data_with_segments %>%
    group_by(protocol, file, user_name, isp, segment) %>%
    summarize(
      start_time = min(interval_start),
      end_time = max(interval_end),
      avg_bitrate_mbps = mean(bitrate_mbps),
      stability_cv = sd(bitrate_mbps) / mean(bitrate_mbps) * 100,
      interval_count = n(),
      .groups = "drop"
    )
  
  # Add UDP-specific segment analysis if available
  if("lost_percent" %in% names(data)) {
    udp_segments <- data_with_segments %>%
      filter(protocol == "UDP") %>%
      group_by(protocol, file, user_name, isp, segment) %>%
      summarize(
        avg_jitter_ms = mean(jitter_ms),
        avg_loss_percent = mean(lost_percent),
        .groups = "drop"
      )
    
    segment_analysis <- segment_analysis %>%
      left_join(udp_segments, by = c("protocol", "file", "user_name", "isp", "segment"))
  }
  
  return(segment_analysis)
}

#' Analyze throughput stability
#'
#' @param data Tibble of processed iperf3 data
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
    
    # Add UDP-specific rolling metrics if available
    if("jitter_ms" %in% names(data_sorted)) {
      stability_metrics <- stability_metrics %>%
        mutate(
          rolling_avg_jitter = rollmean(jitter_ms, window_size, fill = NA, align = "right"),
          rolling_avg_loss = rollmean(lost_percent, window_size, fill = NA, align = "right")
        )
    }
    
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
#' @param data Tibble of processed iperf3 data
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
#' @param data Tibble of processed iperf3 data
#' @param group_var Variable to group by (e.g., "network", "time_of_day")
#' @return Analysis results
compare_tests <- function(data, group_var) {
  if(!group_var %in% names(data)) {
    stop(paste("Variable", group_var, "not found in data"))
  }
  
  comparison <- data %>%
    group_by(protocol, !!sym(group_var)) %>%
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