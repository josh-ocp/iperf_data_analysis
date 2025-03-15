library(tidyverse)
library(lubridate)

#' Calculate summary statistics for iperf3 test data
#'
#' @param data Tibble of processed iperf3 data
#' @return Tibble with summary statistics
summarize_iperf_data <- function(data) {
  result <- data %>%
    group_by(protocol, file) %>%
    summarize(
      avg_bitrate_mbps = mean(bitrate_mbps),
      min_bitrate_mbps = min(bitrate_mbps),
      max_bitrate_mbps = max(bitrate_mbps),
      sd_bitrate_mbps = sd(bitrate_mbps),
      test_duration_sec = max(interval_end),
      .groups = "drop"
    )
  
  # Add UDP-specific metrics if available
  if("lost_percent" %in% names(data)) {
    udp_summary <- data %>%
      filter(protocol == "UDP") %>%
      group_by(protocol, file) %>%
      summarize(
        avg_jitter_ms = mean(jitter_ms),
        max_jitter_ms = max(jitter_ms),
        total_packets = sum(packets_total),
        lost_packets = sum(packets_lost),
        overall_loss_percent = lost_packets / total_packets * 100,
        .groups = "drop"
      )
    
    result <- result %>%
      left_join(udp_summary, by = c("protocol", "file"))
  }
  
  return(result)
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
  
  # Implement comparison logic here
  # ...
  
  return(results)
}