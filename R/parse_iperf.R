library(tidyverse)
library(lubridate)

#' Extract user metadata from filename with enhanced flexibility
#'
#' @param filename The iperf filename (supports various formats)
#' @return List containing user name, ISP, and test date/time
extract_user_metadata <- function(filename) {
  # Remove file path if present
  filename <- basename(filename)
  # Remove .json extension
  filename <- sub("\\.json$", "", filename)
  
  # Split by underscore
  parts <- strsplit(filename, "_")[[1]]
  
  # Default values
  result <- list(
    user_name = "unknown",
    isp = "unknown",
    test_date = NA,
    test_time = NA,
    date_formatted = NA,    # Human-readable date
    time_formatted = NA,    # Human-readable time
    test_datetime = NA
  )
  
  # Always use first part as username if available
  if(length(parts) >= 1) {
    result$user_name <- parts[1]
  }
  
  # Try to identify date and time - look for 8-digit number (date) followed by 6-digit number (time)
  date_time_indices <- NULL
  for(i in 1:(length(parts)-1)) {
    if(grepl("^\\d{8}$", parts[i]) && grepl("^\\d{6}$", parts[i+1])) {
      result$test_date <- parts[i]
      result$test_time <- parts[i+1]
      date_time_indices <- c(i, i+1)
      break
    }
  }
  
  # If date/time found, everything between username and date is ISP
  if(!is.null(date_time_indices) && date_time_indices[1] > 2) {
    # Get all parts between username and date as ISP
    isp_parts <- parts[2:(date_time_indices[1]-1)]
    
    # If "iperf" is in these parts, only use parts before "iperf"
    iperf_idx <- match("iperf", isp_parts)
    if(!is.na(iperf_idx) && iperf_idx > 1) {
      isp_parts <- isp_parts[1:(iperf_idx-1)]
    }
    
    result$isp <- paste(isp_parts, collapse = " ")
  } else if(length(parts) >= 2) {
    # If date/time not found or immediately after username, use second part as ISP
    result$isp <- parts[2]
  }
  
  # Clean up ISP (handle multiple underscores)
  result$isp <- gsub("_", " ", result$isp)
  result$isp <- trimws(result$isp)
  
  # Create formatted date and time if date and time were found
  if(!is.na(result$test_date) && !is.na(result$test_time)) {
    # Format date as YYYY-MM-DD
    result$date_formatted <- paste0(
      substr(result$test_date, 1, 4), "-",  # Year
      substr(result$test_date, 5, 6), "-",  # Month
      substr(result$test_date, 7, 8)        # Day
    )
    
    # Format time as HH:MM:SS
    result$time_formatted <- paste0(
      substr(result$test_time, 1, 2), ":",  # Hour
      substr(result$test_time, 3, 4), ":",  # Minute
      substr(result$test_time, 5, 6)        # Second
    )
    
    # Create datetime object
    tryCatch({
      result$test_datetime <- ymd_hms(
        paste(result$date_formatted, result$time_formatted),
        quiet = TRUE
      )
      
      # Add a more human-friendly datetime string
      result$datetime_readable <- format(result$test_datetime, "%b %d, %Y at %I:%M %p")
    }, error = function(e) {
      warning("Could not parse date/time: ", e$message)
    })
  }
  
  return(result)
}

#' Enhanced raw iPerf3 output parsing with summary information
#'
#' @param file_path Path to the raw iPerf3 output file
#' @return A data frame with structured iPerf3 data
#' @import dplyr
preprocess_raw_iperf <- function(file_path) {
  # Read the file content
  raw_lines <- readLines(file_path)
  
  # Identify data lines (those with interval pattern like "0.00-1.00")
  data_pattern <- "^\\s*\\[\\s*\\d+\\]\\s+(\\d+\\.\\d+)-(\\d+\\.\\d+)\\s+sec\\s+(\\d+\\.\\d+\\s+\\w+|\\d+\\s+\\w+)\\s+(\\d+\\.\\d+\\s+\\w+/sec|\\d+\\.\\d+\\s+\\w+/sec)"
  data_lines <- raw_lines[grep(data_pattern, raw_lines)]
  
  if (length(data_lines) == 0) {
    return(NULL) # No valid data found
  }
  
  # Extract connection info
  host_line <- grep("Connecting to host", raw_lines, value = TRUE)
  host <- if (length(host_line) > 0) {
    sub(".*host\\s+([^,]+).*", "\\1", host_line[1])
  } else {
    NA_character_
  }
  
  # Extract client/server info
  conn_line <- grep("local .* port .* connected to", raw_lines, value = TRUE)
  local_ip <- if (length(conn_line) > 0) {
    sub(".*local\\s+(\\S+)\\s+port.*", "\\1", conn_line[1])
  } else {
    NA_character_
  }
  remote_ip <- if (length(conn_line) > 0) {
    sub(".*connected to\\s+(\\S+)\\s+port.*", "\\1", conn_line[1])
  } else {
    NA_character_
  }
  
  # Extract protocol information
  protocol <- if (any(grepl("UDP", raw_lines))) "udp" else "tcp"
  
  # Extract summary info for total transfer and bitrate
  summary_pattern <- ".*\\s+(\\d+(\\.\\d+)?\\s+\\w+)\\s+(\\d+(\\.\\d+)?\\s+\\w+/sec)"
  summary_sender_line <- grep("sender", raw_lines[grep(summary_pattern, raw_lines)], value = TRUE)
  summary_receiver_line <- grep("receiver", raw_lines[grep(summary_pattern, raw_lines)], value = TRUE)
  
  # Extract sender summary
  total_sent_bytes <- NA
  total_sent_bitrate <- NA
  if (length(summary_sender_line) > 0) {
    transfer_match <- regexpr("\\d+(\\.\\d+)?\\s+\\w+", summary_sender_line)
    if (transfer_match > 0) {
      transfer_str <- substr(summary_sender_line, transfer_match, 
                          transfer_match + attr(transfer_match, "match.length") - 1)
      transfer_val <- as.numeric(sub("\\s+\\w+$", "", transfer_str))
      transfer_unit <- sub("^\\d+(\\.\\d+)?\\s+", "", transfer_str)
      
      # Convert to bytes
      if (grepl("KB|KBytes", transfer_unit)) {
        total_sent_bytes <- transfer_val * 1024
      } else if (grepl("MB|MBytes", transfer_unit)) {
        total_sent_bytes <- transfer_val * 1024 * 1024
      } else if (grepl("GB|GBytes", transfer_unit)) {
        total_sent_bytes <- transfer_val * 1024 * 1024 * 1024
      }
    }
    
    # Extract bitrate
    bitrate_match <- regexpr("\\d+(\\.\\d+)?\\s+\\w+/sec", summary_sender_line)
    if (bitrate_match > 0) {
      bitrate_str <- substr(summary_sender_line, bitrate_match, 
                         bitrate_match + attr(bitrate_match, "match.length") - 1)
      bitrate_val <- as.numeric(sub("\\s+\\w+/sec$", "", bitrate_str))
      bitrate_unit <- sub("^\\d+(\\.\\d+)?\\s+", "", bitrate_str)
      
      # Convert to Mbps
      if (grepl("Kbits/sec|KBytes/sec", bitrate_unit)) {
        total_sent_bitrate <- bitrate_val / 1024
      } else if (grepl("Mbits/sec|MBytes/sec", bitrate_unit)) {
        total_sent_bitrate <- bitrate_val
      } else if (grepl("Gbits/sec|GBytes/sec", bitrate_unit)) {
        total_sent_bitrate <- bitrate_val * 1024
      }
    }
  }
  
  # Parse the data lines into a structured format
  parsed_data <- data.frame(
    interval_id = seq_along(data_lines),
    stream_id = as.integer(gsub("\\D", "", regmatches(data_lines, regexpr("^\\s*\\[\\s*\\d+\\]", data_lines)))),
    stringsAsFactors = FALSE
  )
  
  # Extract interval start and end times
  time_matches <- regmatches(data_lines, regexpr("\\d+\\.\\d+-\\d+\\.\\d+", data_lines))
  time_parts <- strsplit(time_matches, "-")
  parsed_data$interval_start <- as.numeric(sapply(time_parts, `[`, 1))
  parsed_data$interval_end <- as.numeric(sapply(time_parts, `[`, 2))
  parsed_data$interval_length <- parsed_data$interval_end - parsed_data$interval_start
  
  # Extract transfer amount
  transfer_matches <- regmatches(data_lines, regexpr("\\d+(\\.\\d+)?\\s+\\w+(?=\\s+\\d+(\\.\\d+)?\\s+\\w+/sec)", data_lines))
  parsed_data$transfer_val <- as.numeric(sub("\\s+\\w+$", "", transfer_matches))
  parsed_data$transfer_unit <- sub("^\\d+(\\.\\d+)?\\s+", "", transfer_matches)
  
  # Calculate bytes based on transfer unit
  parsed_data$bytes <- parsed_data$transfer_val
  # Convert to bytes based on unit
  bytes_idx <- grepl("Bytes", parsed_data$transfer_unit)
  if (any(bytes_idx)) {
    kb_idx <- grepl("KBytes", parsed_data$transfer_unit)
    mb_idx <- grepl("MBytes", parsed_data$transfer_unit)
    gb_idx <- grepl("GBytes", parsed_data$transfer_unit)
    
    parsed_data$bytes[kb_idx] <- parsed_data$bytes[kb_idx] * 1024
    parsed_data$bytes[mb_idx] <- parsed_data$bytes[mb_idx] * 1024^2
    parsed_data$bytes[gb_idx] <- parsed_data$bytes[gb_idx] * 1024^3
  }
  
  # Extract bitrate
  bitrate_matches <- regmatches(data_lines, regexpr("\\d+(\\.\\d+)?\\s+\\w+/sec", data_lines))
  parsed_data$bitrate_val <- as.numeric(sub("\\s+\\w+/sec$", "", bitrate_matches))
  parsed_data$bitrate_unit <- sub("^\\d+(\\.\\d+)?\\s+", "", bitrate_matches)
  
  # Convert to Mbps
  parsed_data$bitrate_mbps <- parsed_data$bitrate_val
  kbps_idx <- grepl("Kbits/sec", parsed_data$bitrate_unit)
  gbps_idx <- grepl("Gbits/sec", parsed_data$bitrate_unit)
  
  parsed_data$bitrate_mbps[kbps_idx] <- parsed_data$bitrate_mbps[kbps_idx] / 1024
  parsed_data$bitrate_mbps[gbps_idx] <- parsed_data$bitrate_mbps[gbps_idx] * 1024
  
  # Add connection metadata
  parsed_data$host <- host
  parsed_data$local_ip <- local_ip
  parsed_data$remote_ip <- remote_ip
  parsed_data$protocol <- protocol
  parsed_data$total_sent_bytes <- total_sent_bytes
  parsed_data$total_sent_bitrate <- total_sent_bitrate
  
  # Remove temporary columns used for conversion
  parsed_data <- parsed_data %>%
    select(-transfer_val, -transfer_unit, -bitrate_val, -bitrate_unit)
  
  return(parsed_data)
}

#' Parse iPerf3 JSON output files
#'
#' @param file_path Path to the iPerf3 JSON output file
#' @return A data frame with structured iPerf3 data
#' @import jsonlite
parse_iperf_json <- function(file_path) {
  tryCatch({
    # Read the JSON file
    json_data <- jsonlite::fromJSON(file_path)
    
    # Check if the file contains valid iperf3 JSON output
    if(!("intervals" %in% names(json_data))) {
      warning("Not a valid iPerf3 JSON file: ", file_path)
      return(NULL)
    }
    
    # Extract interval data
    intervals <- json_data$intervals
    
    # Handle difference in JSON structure between iperf3 versions
    if("streams" %in% names(intervals)) {
      # Newer iperf3 versions have nested stream data
      interval_data <- lapply(intervals$streams, function(stream_data) {
        stream_id <- stream_data$socket
        data.frame(
          interval_id = seq_len(nrow(stream_data)),
          stream_id = stream_id,
          interval_start = stream_data$start,
          interval_end = stream_data$end,
          interval_length = stream_data$seconds,
          bytes = stream_data$bytes,
          bitrate_mbps = stream_data$bits_per_second / 1e6,
          retransmits = if("retransmits" %in% names(stream_data)) stream_data$retransmits else NA_integer_,
          stringsAsFactors = FALSE
        )
      })
      parsed_data <- do.call(rbind, interval_data)
    } else {
      # Direct structure
      parsed_data <- data.frame(
        interval_id = seq_len(length(intervals)),
        stream_id = 1,
        interval_start = sapply(intervals, function(x) x$sum$start),
        interval_end = sapply(intervals, function(x) x$sum$end),
        interval_length = sapply(intervals, function(x) x$sum$seconds),
        bytes = sapply(intervals, function(x) x$sum$bytes),
        bitrate_mbps = sapply(intervals, function(x) x$sum$bits_per_second) / 1e6,
        retransmits = if("retransmits" %in% names(intervals[[1]]$sum)) 
                         sapply(intervals, function(x) x$sum$retransmits) 
                      else rep(NA_integer_, length(intervals)),
        stringsAsFactors = FALSE
      )
    }
    
    # Add connection metadata
    parsed_data$host <- json_data$start$connecting_to$host
    parsed_data$local_ip <- json_data$start$connected[[1]]$local_host
    parsed_data$remote_ip <- json_data$start$connected[[1]]$remote_host
    
    # Add test summary data
    parsed_data$total_sent_bytes <- json_data$end$sum_sent$bytes
    parsed_data$total_received_bytes <- json_data$end$sum_received$bytes
    parsed_data$total_sent_bitrate <- json_data$end$sum_sent$bits_per_second / 1e6
    parsed_data$total_received_bitrate <- json_data$end$sum_received$bits_per_second / 1e6
    
    # Add protocol info
    parsed_data$protocol <- tolower(json_data$start$test_start$protocol)
    
    return(parsed_data)
  }, error = function(e) {
    warning("Error parsing JSON file: ", file_path, " - ", e$message)
    return(NULL)
  })
}

#' Process iperf CSV files into a tidy dataframe with robust error handling
#'
#' @param file_paths Vector of file paths to iperf CSV files
#' @return Combined dataframe of processed iperf data
process_iperf_files <- function(file_paths) {
  # Initialize an empty list to store data from each file
  all_data <- list()
  
  for (file_path in file_paths) {
    tryCatch({
      cat("Processing file:", basename(file_path), "\n")
      
      # Check if this is a raw iPerf3 output file or a structured CSV
      first_lines <- readLines(file_path, n = 5)
      is_raw_iperf <- any(grepl("Connecting to host|local .* port .* connected to", first_lines))
      
      if (is_raw_iperf) {
        cat("Detected raw iPerf3 output file, preprocessing...\n")
        csv_data <- preprocess_raw_iperf(file_path)
        if (is.null(csv_data)) {
          warning("Could not parse raw iPerf3 data from file: ", file_path)
          next
        }
      } else {
        # Original CSV reading logic
        csv_data <- read_csv(file_path, show_col_types = FALSE)
        
        # Check if CSV is valid
        if(nrow(csv_data) == 0) {
          warning("File appears to be empty: ", file_path)
          next
        }
      }
      
      # Extract user metadata from filename
      metadata <- extract_user_metadata(file_path)
      
      # Process the data whether it came from raw or structured input
      # Continue with existing processing logic
      
      # Expected columns for iperf CSV
      required_columns <- c("interval_start", "bitrate_mbps")
      if(!all(required_columns %in% colnames(csv_data))) {
        # Check if we have a different column layout that can be mapped
        if("interval" %in% colnames(csv_data) && "bits_per_second" %in% colnames(csv_data)) {
          csv_data <- csv_data %>%
            rename(interval_start = interval) %>%
            mutate(bitrate_mbps = bits_per_second / 1e6)
        } else {
          warning("Missing required columns in file ", file_path)
          next
        }
      }
      
      # Process and format the data
      formatted_data <- csv_data %>%
        # Add row IDs for interval and stream if not present
        mutate(
          interval_id = if("interval_id" %in% colnames(csv_data)) interval_id else row_number(),
          stream_id = if("stream_id" %in% colnames(csv_data)) stream_id else 1,
          socket = if("socket" %in% colnames(csv_data)) socket else NA_integer_
        ) %>%
        # Ensure we have all the standard columns
        mutate(
          interval_end = if("interval_end" %in% colnames(csv_data)) interval_end else interval_start + 1,
          interval_length = if("interval_length" %in% colnames(csv_data)) interval_length else 1,
          bytes = if("bytes" %in% colnames(csv_data)) bytes else NA_integer_,
          retransmits = if("retransmits" %in% colnames(csv_data)) retransmits else NA_integer_
        )
      
      # Add metadata to the data
      formatted_data <- formatted_data %>% 
        mutate(
          file = basename(file_path),
          user_name = metadata$user_name,
          isp = metadata$isp,
          test_datetime = metadata$test_datetime,
          date_formatted = metadata$date_formatted,
          time_formatted = metadata$time_formatted,
          datetime_readable = metadata$datetime_readable
        )
      
      # Add to the list of all data
      all_data[[length(all_data) + 1]] <- formatted_data
      
      # Log success
      cat("Successfully processed", nrow(formatted_data), "data rows from", file_path, "\n")
      
    }, error = function(e) {
      warning("Error processing file: ", file_path, " - ", e$message)
    })
  }
  
  # Combine all data frames
  if(length(all_data) > 0) {
    combined_data <- bind_rows(all_data)
    return(combined_data)
  } else {
    warning("No valid data was processed.")
    return(NULL)
  }
}

#' Calculate additional derived metrics for iperf data
#'
#' @param data Dataframe of processed iperf data
#' @return Dataframe with additional metrics
calculate_derived_metrics <- function(data) {
  if(is.null(data) || nrow(data) == 0) {
    warning("No data to calculate derived metrics.")
    return(NULL)
  }
  
  data %>%
    group_by(file) %>%
    mutate(
      # Calculate percentage of max capacity
      max_bitrate = max(bitrate_mbps, na.rm = TRUE),
      percent_of_max = bitrate_mbps / max_bitrate * 100,
      
      # Calculate change from previous interval
      delta = c(NA, diff(bitrate_mbps)),
      percent_change = delta / lag(bitrate_mbps, default = first(bitrate_mbps)) * 100,
      
      # Calculate relative time since start
      relative_time = interval_start - first(interval_start)
    ) %>%
    ungroup()
}