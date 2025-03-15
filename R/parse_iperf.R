library(jsonlite)
library(tidyverse)

# Add this function to your parse_iperf.R file

#' Extract user metadata from filename
#'
#' @param filename The iperf filename (name_isp_iperf_tcp_test_date_time.json format)
#' @return List containing user name, ISP, and test date/time
extract_user_metadata <- function(filename) {
  # Remove file path if present
  filename <- basename(filename)
  
  # Split by underscore
  parts <- strsplit(filename, "_")[[1]]
  
  # Default values
  result <- list(
    user_name = "unknown",
    isp = "unknown",
    test_date = NA,
    test_time = NA
  )
  
  # Extract information if format matches
  if(length(parts) >= 2) {
    result$user_name <- parts[1]
    result$isp <- parts[2]
    
    # Extract date and time if available
    if(length(parts) >= 6) {
      # Expected format: name_isp_iperf_tcp_test_date_time.json
      result$test_date <- parts[5]
      
      # Extract time from the last part (remove .json extension)
      time_part <- parts[6]
      result$test_time <- sub("\\.json$", "", time_part)
      
      # Create a datetime object
      datetime_str <- paste(result$test_date, result$test_time)
      
      # Try parsing with explicit format instead of automatic detection
      tryCatch({
        result$test_datetime <- ymd_hms(
          paste0(
            substr(result$test_date, 1, 4), "-",
            substr(result$test_date, 5, 6), "-",
            substr(result$test_date, 7, 8), " ",
            substr(result$test_time, 1, 2), ":",
            substr(result$test_time, 3, 4), ":",
            ifelse(nchar(result$test_time) >= 6, substr(result$test_time, 5, 6), "00")
          ),
          quiet = TRUE
        )
      }, error = function(e) {
        warning("Could not parse date/time: ", e$message)
      })
    }
  }
  
  return(result)
}

#' Process a single iperf3 JSON file
#' 
#' @param file_path Path to the iperf3 JSON file
#' @return A tibble with parsed iperf3 data
process_iperf_json <- function(file_path) {
  # Print file being processed for debugging
  cat("Processing file:", file_path, "\n")
  
  # Extract user metadata from filename
  user_meta <- extract_user_metadata(file_path)
  
  # Read JSON file with error handling
  tryCatch({
    # Parse the JSON
    json_text <- readLines(file_path, warn = FALSE)
    json_text <- paste(json_text, collapse = "")
    
    # Additional debugging to see what's in the file
    cat("File size:", nchar(json_text), "characters\n")
    
    # Parse the JSON
    iperf_data <- fromJSON(json_text)
    
    # Extract test info
    test_start_timestamp <- iperf_data$start$timestamp$time
    test_protocol <- iperf_data$start$test_start$protocol
    is_udp <- identical(toupper(test_protocol), "UDP")
    
    cat("Protocol detected:", test_protocol, "\n")
    cat("Number of intervals:", length(iperf_data$intervals), "\n")
    
    # Create empty result dataframe with required columns
    result <- tibble(
      interval_start = numeric(),
      interval_end = numeric(),
      transfer_bytes = numeric(),
      bitrate_mbps = numeric(),
      protocol = character(),
      timestamp = character(),
      file = character(),
      user_name = character(),
      isp = character()
    )
    
    # Add UDP-specific empty columns if needed
    if(is_udp) {
      result$packets_total <- numeric()
      result$packets_lost <- numeric()
      result$lost_percent <- numeric()
      result$jitter_ms <- numeric()
    }
    
    # EXTRACT INDIVIDUAL INTERVAL DATA - CRITICAL FIX
    # -------------------------------------------
    cat("\nExtracting individual interval data...\n")
    
    # Get a deeper look at the intervals structure
    cat("Full intervals structure:\n")
    str(iperf_data$intervals, max.level = 4)
    
    # Process intervals from iperf data
    if(length(iperf_data$intervals) > 0) {
      interval_count <- 0
      
      for(i in 1:length(iperf_data$intervals)) {
        interval <- iperf_data$intervals[[i]]
        cat("\nProcessing interval", i, "of", length(iperf_data$intervals), "\n")
        
        # Approach 1: Try direct extraction if interval is a list with sum element
        if(is.list(interval) && "sum" %in% names(interval)) {
          cat("Found 'sum' element in interval", i, "\n")
          sum_data <- interval$sum
          
          # Debug sum data
          cat("Sum data structure:\n")
          str(sum_data, max.level = 2)
          
          # Check if sum has essential fields
          if(is.list(sum_data) && 
             "start" %in% names(sum_data) && 
             "end" %in% names(sum_data) && 
             "bits_per_second" %in% names(sum_data)) {
            
            cat("Creating row from sum data\n")
            
            data_row <- tibble(
              interval_start = sum_data$start,
              interval_end = sum_data$end,
              bitrate_mbps = sum_data$bits_per_second / 1000000,
              protocol = test_protocol,
              timestamp = test_start_timestamp,
              file = basename(file_path),
              user_name = user_meta$user_name,
              isp = user_meta$isp
            )
            
            # Add bytes if available
            if("bytes" %in% names(sum_data)) {
              data_row$transfer_bytes <- sum_data$bytes
            }
            
            # Add UDP-specific metrics
            if(is_udp) {
              if("packets" %in% names(sum_data)) data_row$packets_total <- sum_data$packets
              if("lost_packets" %in% names(sum_data)) data_row$packets_lost <- sum_data$lost_packets
              if("jitter_ms" %in% names(sum_data)) data_row$jitter_ms <- sum_data$jitter_ms
              
              if("packets" %in% names(sum_data) && "lost_packets" %in% names(sum_data) && sum_data$packets > 0) {
                data_row$lost_percent <- (sum_data$lost_packets / sum_data$packets) * 100
              } else {
                data_row$lost_percent <- 0
              }
            }
            
            result <- bind_rows(result, data_row)
            interval_count <- interval_count + 1
          }
        } 
        
        # Approach 2: Try to access streams if available
        if(is.list(interval) && "streams" %in% names(interval)) {
          streams <- interval$streams
          cat("Found 'streams' element with", length(streams), "stream(s)\n")
          
          for(j in 1:length(streams)) {
            stream <- streams[[j]]
            
            # Debug stream structure
            cat("Stream", j, "structure:\n")
            str(stream, max.level = 2)
            
            if(is.list(stream) && 
               "start" %in% names(stream) && 
               "end" %in% names(stream) && 
               "bits_per_second" %in% names(stream)) {
              
              cat("Creating row from stream data\n")
              
              data_row <- tibble(
                interval_start = stream$start,
                interval_end = stream$end,
                bitrate_mbps = stream$bits_per_second / 1000000,
                protocol = test_protocol,
                timestamp = test_start_timestamp,
                file = basename(file_path),
                user_name = user_meta$user_name,
                isp = user_meta$isp
              )
              
              # Add bytes if available
              if("bytes" %in% names(stream)) {
                data_row$transfer_bytes <- stream$bytes
              }
              
              # Add UDP-specific metrics
              if(is_udp) {
                if("packets" %in% names(stream)) data_row$packets_total <- stream$packets
                if("lost_packets" %in% names(stream)) data_row$packets_lost <- stream$lost_packets
                if("jitter_ms" %in% names(stream)) data_row$jitter_ms <- stream$jitter_ms
                
                if("packets" %in% names(stream) && "lost_packets" %in% names(stream) && stream$packets > 0) {
                  data_row$lost_percent <- (stream$lost_packets / stream$packets) * 100
                } else {
                  data_row$lost_percent <- 0
                }
              }
              
              result <- bind_rows(result, data_row)
              interval_count <- interval_count + 1
            }
          }
        }
      }
      
      cat("Processed", interval_count, "intervals\n")
    } else {
      cat("No intervals found in JSON data\n")
    }
    
    # Add summary data if we didn't get any interval data
    if(nrow(result) == 0 && "end" %in% names(iperf_data) && 
       "sum_sent" %in% names(iperf_data$end) && is.list(iperf_data$end$sum_sent)) {
      
      cat("No interval data found, falling back to summary data\n")
      
      # Get summary data
      summary_data <- iperf_data$end$sum_sent
      
      # Create a row from summary data
      data_row <- tibble(
        interval_start = 0,
        interval_end = summary_data$seconds,
        bitrate_mbps = summary_data$bits_per_second / 1000000,
        protocol = test_protocol,
        timestamp = test_start_timestamp,
        file = basename(file_path),
        user_name = user_meta$user_name,
        isp = user_meta$isp
      )
      
      # Add bytes if available
      if("bytes" %in% names(summary_data)) {
        data_row$transfer_bytes <- summary_data$bytes
      }
      
      # Add UDP-specific metrics if available
      if(is_udp) {
        if("packets" %in% names(summary_data)) data_row$packets_total <- summary_data$packets
        if("lost_packets" %in% names(summary_data)) data_row$packets_lost <- summary_data$lost_packets
        if("jitter_ms" %in% names(summary_data)) data_row$jitter_ms <- summary_data$jitter_ms
        
        if("packets" %in% names(summary_data) && "lost_packets" %in% names(summary_data) && summary_data$packets > 0) {
          data_row$lost_percent <- (summary_data$lost_packets / summary_data$packets) * 100
        } else {
          data_row$lost_percent <- 0
        }
      }
      
      result <- bind_rows(result, data_row)
    }
    
    # If we got data, return it
    if(nrow(result) > 0) {
      cat("Successfully extracted", nrow(result), "data rows\n")
      
      # Fill in any NA transfer_bytes
      if(any(is.na(result$transfer_bytes))) {
        result$transfer_bytes[is.na(result$transfer_bytes)] <- 0
      }
      
      return(result)
    } else {
      warning(paste("Could not extract interval data from file:", file_path))
      return(NULL)
    }
  }, error = function(e) {
    warning(paste("Error processing file:", file_path, "-", e$message))
    print(e)  # Print the full error for debugging
    return(NULL)
  })
}

#' Process multiple iperf3 JSON files
#'
#' @param file_paths Vector of file paths
#' @return Combined tibble with data from all files
process_iperf_files <- function(file_paths) {
  # Check if we have any files
  if(length(file_paths) == 0) {
    warning("No files to process")
    return(tibble())
  }
  
  # Process each file
  results_list <- list()
  for(i in 1:length(file_paths)) {
    results_list[[i]] <- process_iperf_json(file_paths[i])
  }
  
  # Remove NULL results and combine
  results_list <- results_list[!sapply(results_list, is.null)]
  
  if(length(results_list) > 0) {
    # Check if all results have the same columns
    column_sets <- lapply(results_list, names)
    if(length(unique(column_sets)) > 1) {
      cat("Warning: Not all result data frames have the same columns. Adjusting...\n")
      # Get all column names
      all_cols <- unique(unlist(column_sets))
      # Make sure all results have all columns
      for(i in 1:length(results_list)) {
        missing_cols <- setdiff(all_cols, names(results_list[[i]]))
        for(col in missing_cols) {
          results_list[[i]][[col]] <- NA
        }
      }
    }
    
    combined_data <- bind_rows(results_list)
    return(combined_data)
  } else {
    warning("None of the files could be processed successfully")
    return(tibble())
  }
}