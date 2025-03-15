library(jsonlite)
library(tidyverse)

#' Process a single iperf3 JSON file
#'
#' @param file_path Path to the iperf3 JSON file
#' @return A tibble with parsed iperf3 data
process_iperf_json <- function(file_path) {
  # Read JSON file
  iperf_data <- fromJSON(file_path)
  
  # Extract test info
  test_start_timestamp <- iperf_data$start$timestamp$time
  test_protocol <- iperf_data$start$test_start$protocol
  is_udp <- test_protocol == "UDP"
  test_duration <- iperf_data$start$test_start$duration
  
  # Process interval data
  intervals_df <- as.data.frame(iperf_data$intervals)
  
  # Handle nested structure from JSON
  if("streams" %in% names(intervals_df)) {
    # Extract data from each interval
    result <- map_df(1:nrow(intervals_df), function(i) {
      interval <- intervals_df$streams[[i]]
      
      # Basic metrics for both TCP and UDP
      data <- tibble(
        interval_start = intervals_df$streams[[i]][[1]]$start,
        interval_end = intervals_df$streams[[i]][[1]]$end,
        transfer_bytes = intervals_df$streams[[i]][[1]]$bytes,
        bitrate_mbps = intervals_df$streams[[i]][[1]]$bits_per_second / 1000000
      )
      
      # Add UDP-specific metrics
      if(is_udp) {
        data$jitter_ms <- intervals_df$streams[[i]][[1]]$jitter_ms
        data$packets_lost <- intervals_df$streams[[i]][[1]]$lost_packets
        data$packets_total <- intervals_df$streams[[i]][[1]]$packets
        data$lost_percent <- data$packets_lost / data$packets_total * 100
      }
      
      return(data)
    })
    
    # Add test metadata
    result$protocol <- test_protocol
    result$timestamp <- test_start_timestamp
    result$file <- basename(file_path)
    
    return(result)
  } else {
    warning(paste("Unexpected JSON structure in file:", file_path))
    return(NULL)
  }
}

#' Process multiple iperf3 JSON files
#'
#' @param file_paths Vector of file paths
#' @return Combined tibble with data from all files
process_iperf_files <- function(file_paths) {
  combined_data <- map_df(file_paths, process_iperf_json)
  return(combined_data)
}