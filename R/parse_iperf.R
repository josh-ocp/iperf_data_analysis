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
  transfer_pattern <- "(\\d+(\\.\\d+)?\\s+\\w+)\\s+\\d+(\\.\\d+)?\\s+\\w+/sec"
  transfer_matches <- regmatches(data_lines, regexpr(transfer_pattern, data_lines))
  # Extract just the transfer amount part (first capture group) from the matches
  transfer_parts <- gsub("\\s+\\d+(\\.\\d+)?\\s+\\w+/sec$", "", transfer_matches)
  parsed_data$transfer_val <- as.numeric(sub("\\s+\\w+$", "", transfer_parts))
  parsed_data$transfer_unit <- sub("^\\d+(\\.\\d+)?\\s+", "", transfer_parts)
  
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

#' Enhanced raw iPerf3 output parsing with summary information
#'
#' @param file_path Path to raw iperf output file
#' @return Dataframe with parsed iperf data
parse_raw_iperf_output <- function(file_path) {
  tryCatch({
    # Read the file
    content <- readLines(file_path)
    
    # Find data lines - they typically start with "[ID]" header followed by data rows
    header_idx <- grep("^\\[ ID\\]", content)
    if(length(header_idx) == 0) {
      warning("Could not find data header in file: ", basename(file_path))
      return(NULL)
    }
    
    # Get data lines (skip the header)
    data_lines <- content[(header_idx[1] + 1):length(content)]
    
    # Extract only normal interval lines (not summary lines)
    # Normal interval pattern: [ ID] start-end sec transfer bitrate
    data_lines <- grep("^\\s*\\[\\s*\\d+\\]\\s+\\d+\\.\\d+-\\d+\\.\\d+", data_lines, value = TRUE)
    
    if(length(data_lines) == 0) {
      warning("No valid data lines found in file: ", basename(file_path))
      return(NULL)
    }
    
    # Extract connection ID, interval, transfer and bitrate
    # Using regex to parse each line
    
    # Example line: [ 29]   0.00-1.01   sec  8.50 MBytes  70.9 Mbits/sec
    
    # Extract total bitrate from summary line if available
    total_sent_bitrate <- NA
    total_lines <- grep("^\\[ ID\\]\\s+Interval\\s+Transfer\\s+Bitrate", content, value = TRUE)
    if(length(total_lines) > 0) {
      sender_line <- grep("sender$", content, value = TRUE)
      if(length(sender_line) > 0) {
        bitrate_match <- regexpr("\\d+(\\.\\d+)?\\s+[KMG]bits/sec\\s+sender$", sender_line)
        if(bitrate_match > 0) {
          bitrate_str <- regmatches(sender_line, bitrate_match)
          bitrate_parts <- strsplit(bitrate_str, "\\s+")[[1]]
          bitrate_val <- as.numeric(bitrate_parts[1])
          bitrate_unit <- bitrate_parts[2]
          
          if (grepl("Kbits/sec|KBytes/sec", bitrate_unit)) {
            total_sent_bitrate <- bitrate_val / 1024
          } else if (grepl("Mbits/sec|MBytes/sec", bitrate_unit)) {
            total_sent_bitrate <- bitrate_val
          } else if (grepl("Gbits/sec|GBytes/sec", bitrate_unit)) {
            total_sent_bitrate <- bitrate_val * 1024
          }
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
    parsed_data$interval_start <- as.numeric(sapply(time_parts, function(x) x[1]))
    parsed_data$interval_end <- as.numeric(sapply(time_parts, function(x) x[2]))
    
    # Extract transfer amount
    transfer_matches <- regmatches(data_lines, regexpr("\\d+(\\.\\d+)?\\s+[KMG]Bytes", data_lines))
    parsed_data$transfer_bytes <- NA
    for(i in seq_along(transfer_matches)) {
      if(length(transfer_matches[i]) > 0) {
        transfer_parts <- strsplit(transfer_matches[i], "\\s+")[[1]]
        transfer_val <- as.numeric(transfer_parts[1])
        transfer_unit <- transfer_parts[2]
        
        if(grepl("KBytes", transfer_unit)) {
          parsed_data$transfer_bytes[i] <- transfer_val * 1024
        } else if(grepl("MBytes", transfer_unit)) {
          parsed_data$transfer_bytes[i] <- transfer_val * 1024^2
        } else if(grepl("GBytes", transfer_unit)) {
          parsed_data$transfer_bytes[i] <- transfer_val * 1024^3
        } else {
          parsed_data$transfer_bytes[i] <- transfer_val
        }
      }
    }
    
    # Extract bitrate
    bitrate_matches <- regmatches(data_lines, regexpr("\\d+(\\.\\d+)?\\s+[KMG]bits/sec", data_lines))
    parsed_data$bitrate_mbps <- NA
    for(i in seq_along(bitrate_matches)) {
      if(length(bitrate_matches[i]) > 0) {
        bitrate_parts <- strsplit(bitrate_matches[i], "\\s+")[[1]]
        bitrate_val <- as.numeric(bitrate_parts[1])
        bitrate_unit <- bitrate_parts[2]
        
        if(grepl("Kbits/sec", bitrate_unit)) {
          parsed_data$bitrate_mbps[i] <- bitrate_val / 1000
        } else if(grepl("Mbits/sec", bitrate_unit)) {
          parsed_data$bitrate_mbps[i] <- bitrate_val
        } else if(grepl("Gbits/sec", bitrate_unit)) {
          parsed_data$bitrate_mbps[i] <- bitrate_val * 1000
        }
      }
    }
    
    # Add file information
    df <- parsed_data
    df$file <- basename(file_path)
    
    # Extract metadata from filename
    file_metadata <- extract_user_metadata(file_path)
    df$user_name <- file_metadata$user_name
    df$isp <- file_metadata$isp
    
    # Add test datetime
    if (!is.na(file_metadata$test_datetime)) {
      df$test_datetime <- file_metadata$test_datetime
      df$date_formatted <- file_metadata$date_formatted
      df$time_formatted <- file_metadata$time_formatted
      df$datetime_readable <- format(df$test_datetime, "%b %d, %Y at %I:%M %p")
    } else {
      # Try to extract date from filename directly
      date_str <- NA
      time_str <- NA
      
      # Look for patterns like 20250316_075748 in filename
      parts <- strsplit(basename(file_path), "_")[[1]]
      for(i in seq_along(parts)) {
        if(nchar(parts[i]) == 8 && grepl("^\\d{8}$", parts[i])) {
          date_str <- parts[i]
          if(i < length(parts) && nchar(parts[i+1]) == 6 && grepl("^\\d{6}$", parts[i+1])) {
            time_str <- parts[i+1]
          }
          break
        }
      }
      
      if(!is.na(date_str) && !is.na(time_str)) {
        df$test_datetime <- as.POSIXct(
          paste0(
            substr(date_str, 1, 4), "-", 
            substr(date_str, 5, 6), "-", 
            substr(date_str, 7, 8), " ", 
            substr(time_str, 1, 2), ":", 
            substr(time_str, 3, 4), ":", 
            substr(time_str, 5, 6)
          ), 
          format="%Y-%m-%d %H:%M:%S"
        )
        df$date_formatted <- format(df$test_datetime, "%Y-%m-%d")
        df$time_formatted <- format(df$test_datetime, "%H:%M:%S")
        df$datetime_readable <- format(df$test_datetime, "%Y-%m-%d %H:%M:%S")
      } else {
        # Fallback to file modification time
        file_info <- file.info(file_path)
        df$test_datetime <- file_info$mtime
        df$datetime_readable <- format(df$test_datetime, "%Y-%m-%d %H:%M:%S")
      }
    }
    
    return(df)
  }, error = function(e) {
    warning("Error parsing raw iperf file: ", basename(file_path), " - ", e$message)
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

# Functions for parsing iPerf data files

# Main function to process TCP data from iPerf files
process_tcp_data <- function(files) {
  if(length(files) == 0) {
    message("No files to process.")
    return(NULL)
  }
  
  # Initialize empty data frame
  all_data <- data.frame()
  
  for(file in files) {
    message("Processing file: ", basename(file))
    
    tryCatch({
      # Check file extension and content
      if(grepl("\\.txt$", file)) {
        # Text files are assumed to be raw iperf output files
        message("Processing iperf text output: ", basename(file))
        file_data <- parse_raw_iperf_output(file)
      } else if(grepl("\\.csv$", file)) {
        # For backward compatibility, check if this is actually a CSV or raw iperf output
        first_lines <- readLines(file, n = 10)
        is_raw_iperf <- any(grepl("^\\s*\\[\\s*\\d+\\]\\s+\\d+\\.\\d+-\\d+\\.\\d+\\s+sec", first_lines))
        
        if(is_raw_iperf) {
          message("Detected raw iperf output file with .csv extension, parsing as text")
          file_data <- parse_raw_iperf_output(file)
        } else {
          # Standard CSV processing for true CSV files
          message("Processing CSV file: ", basename(file))
          file_data <- process_csv_file(file)
        }
      } else {
        warning("Unknown file type: ", file)
        next
      }
      
      if(!is.null(file_data) && nrow(file_data) > 0) {
        # Add filename for reference if not already present
        if(!"file" %in% names(file_data)) {
          file_data$file <- basename(file)
        }
        all_data <- rbind(all_data, file_data)
      }
    }, error = function(e) {
      warning("Error processing file ", basename(file), ": ", e$message)
    })
  }
  
  # If we have data, add datetime field for filtering
  if(nrow(all_data) > 0) {
    if("timestamp" %in% names(all_data)) {
      all_data$test_datetime <- as.POSIXct(all_data$timestamp, origin="1970-01-01")
    }
    
    # If no bitrate_mbps field exists but bits_per_second does, convert it
    if(!"bitrate_mbps" %in% names(all_data) && "bits_per_second" %in% names(all_data)) {
      all_data$bitrate_mbps <- all_data$bits_per_second / 1e6
    }
  }
  
  return(all_data)
}

# Process CSV file
process_csv_file <- function(file) {
  # Try to read the CSV file - adjust parameters based on your CSV format
  tryCatch({
    data <- read.csv(file, stringsAsFactors = FALSE)
    
    # Check if this appears to be an iPerf CSV file
    required_columns <- c("interval_start", "interval_end", "bitrate_mbps")
    missing_columns <- required_columns[!required_columns %in% names(data)]
    
    if(length(missing_columns) > 0) {
      # Try to adapt to different column names
      if("bits_per_second" %in% names(data)) {
        data$bitrate_mbps <- data$bits_per_second / 1e6
      }
      
      if("start" %in% names(data) && !("interval_start" %in% names(data))) {
        data$interval_start <- data$start
      }
      
      if("end" %in% names(data) && !("interval_end" %in% names(data))) {
        data$interval_end <- data$end
      }
      
      # Check again after adaptations
      missing_columns <- required_columns[!required_columns %in% names(data)]
      if(length(missing_columns) > 0) {
        warning("CSV file missing required columns: ", paste(missing_columns, collapse=", "))
      }
    }
    
    # Try to extract test datetime from filename or file modification time
    if(!("timestamp" %in% names(data))) {
      file_info <- file.info(file)
      data$timestamp <- as.numeric(file_info$mtime)
    }
    
    # Try to extract user and ISP information from filename
    filename <- basename(file)
    
    # Extract user name if pattern like "user_name-" exists
    user_match <- regexpr("^[^-]+-", filename)
    if(user_match > 0) {
      user_length <- attr(user_match, "match.length") - 1  # Remove the trailing hyphen
      data$user_name <- substr(filename, 1, user_length)
    }
    
    # Extract ISP if pattern like "-isp-" exists
    isp_match <- regexpr("-[^-]+-", filename)
    if(isp_match > 0) {
      isp_start <- isp_match + 1  # Skip the initial hyphen
      isp_length <- attr(isp_match, "match.length") - 2  # Remove both hyphens
      data$isp <- substr(filename, isp_start, isp_start + isp_length - 1)
    }
    
    return(data)
  }, error = function(e) {
    warning("Error reading CSV file: ", e$message)
    return(NULL)
  })
}

# Add this function to your existing file

analyze_csv_structure <- function(file_path) {
  # Read the header to determine column structure
  header <- read.csv(file_path, nrows = 1)
  column_names <- names(header)
  
  # Output analysis to console
  message("Analyzing CSV structure for ", basename(file_path))
  message("Found columns: ", paste(column_names, collapse = ", "))
  
  # Sample first few rows
  data_sample <- read.csv(file_path, nrows = 5)
  message("Sample data (first row):")
  print(data_sample[1,])
  
  return(list(
    columns = column_names,
    has_required = all(c("interval_start", "interval_end", "bitrate_mbps") %in% column_names),
    sample = data_sample
  ))
}

# You can also enhance your process_tcp_data function to be more robust with different CSV formats
# This is just a suggestion - integrate it with your existing function
process_csv_with_fallback <- function(file_path) {
  structure <- analyze_csv_structure(file_path)
  
  if(structure$has_required) {
    # Process with standard approach
    df <- read.csv(file_path)
  } else {
    # Fallback approach based on available columns
    df <- read.csv(file_path)
    
    # Map to required column structure based on what's available
    if("timestamp" %in% structure$columns) {
      df$interval_start <- df$timestamp
      df$interval_end <- df$timestamp + 1
    }
    
    if("bits_per_second" %in% structure$columns) {
      df$bitrate_mbps <- df$bits_per_second / 1000000
    } else if("KBytes_per_sec" %in% structure$columns) {
      df$bitrate_mbps <- df$KBytes_per_sec * 8 / 1000
    }
    
    # Extract metadata from filename
    file_name <- basename(file_path)
    parts <- strsplit(file_name, "_")[[1]]
    
    # Add metadata columns
    df$file <- file_name
    
    if(length(parts) >= 1) df$user_name <- parts[1]
    if(length(parts) >= 3) df$isp <- paste(parts[2:3], collapse = " ")
    if(length(parts) >= 5) df$host <- parts[5]
  }
  
  return(df)
}

#' Parse raw iperf output files with .csv extension
#'
#' @param file_path Path to a raw iperf output file with .csv extension
#' @return A dataframe with structured iperf data
#' @export
parse_raw_iperf_output <- function(file_path) {
  tryCatch({
    # Read the raw iperf output file
    raw_lines <- readLines(file_path)
    
    # Extract interval data lines with pattern like "[  5]  4.00-5.00   sec  12.0 MBytes  101 Mbits/sec"
    data_pattern <- "^\\s*\\[\\s*\\d+\\]\\s+(\\d+\\.\\d+)-(\\d+\\.\\d+)\\s+sec\\s+(\\d+(\\.\\d+)?\\s+\\w+)\\s+(\\d+(\\.\\d+)?\\s+\\w+/sec)"
    data_lines <- raw_lines[grep(data_pattern, raw_lines)]
    
    if(length(data_lines) == 0) {
      warning("No interval data found in file: ", basename(file_path))
      return(NULL)
    }
    
    # Extract connection info if available
    connection_line <- grep("local .* port .* connected to", raw_lines, value = TRUE)
    host <- NA
    local_ip <- NA
    remote_ip <- NA
    protocol <- "tcp" # Default to TCP
    
    if(length(connection_line) > 0) {
      host_match <- regexpr("connected to .* port", connection_line)
      if(host_match > 0) {
        host_str <- substr(connection_line, host_match + 13, host_match + attr(host_match, "match.length") - 6)
        host <- host_str
      }
      
      local_ip_match <- regexpr("local .* port", connection_line)
      if(local_ip_match > 0) {
        local_ip_str <- substr(connection_line, local_ip_match + 6, local_ip_match + attr(local_ip_match, "match.length") - 6)
        local_ip <- local_ip_str
      }
      
      remote_ip_match <- regexpr("connected to .*", connection_line)
      if(remote_ip_match > 0) {
        remote_ip_str <- substr(connection_line, remote_ip_match + 13, nchar(connection_line))
        remote_ip <- strsplit(remote_ip_str, " ")[[1]][1]
      }
    }
    
    # Extract UDP-specific metrics if present
    is_udp <- any(grepl("datagrams received", raw_lines))
    if(is_udp) {
      protocol <- "udp"
    }
    
    # Extract stream IDs
    stream_ids <- as.integer(gsub("\\D", "", regmatches(data_lines, regexpr("^\\s*\\[\\s*\\d+\\]", data_lines))))
    
    # Parse interval data
    interval_starts <- as.numeric(sub("^\\s*\\[\\s*\\d+\\]\\s+(\\d+\\.\\d+)-(\\d+\\.\\d+).*$", "\\1", data_lines))
    interval_ends <- as.numeric(sub("^\\s*\\[\\s*\\d+\\]\\s+(\\d+\\.\\d+)-(\\d+\\.\\d+).*$", "\\2", data_lines))
    
    # Extract transfer amounts
    transfer_pattern <- "(\\d+(\\.\\d+)?\\s+\\w+)\\s+\\d+(\\.\\d+)?\\s+\\w+/sec"
    transfer_matches <- regmatches(data_lines, regexpr(transfer_pattern, data_lines))
    # Extract just the transfer amount part (first capture group) from the matches
    transfer_parts <- gsub("\\s+\\d+(\\.\\d+)?\\s+\\w+/sec$", "", transfer_matches)
    transfer_values <- as.numeric(sub("\\s+\\w+$", "", transfer_parts))
    transfer_units <- sub("^\\d+(\\.\\d+)?\\s+", "", transfer_parts)
    
    # Calculate bytes based on transfer unit
    bytes <- transfer_values
    kb_idx <- grepl("KBytes", transfer_units)
    mb_idx <- grepl("MBytes", transfer_units)
    gb_idx <- grepl("GBytes", transfer_units)
    
    bytes[kb_idx] <- bytes[kb_idx] * 1024
    bytes[mb_idx] <- bytes[mb_idx] * 1024^2
    bytes[gb_idx] <- bytes[gb_idx] * 1024^3
    
    # Extract bitrates
    bitrate_pattern <- "\\d+(\\.\\d+)?\\s+\\w+/sec"
    bitrate_matches <- regmatches(data_lines, regexpr(bitrate_pattern, data_lines))
    bitrate_values <- as.numeric(sub("\\s+\\w+/sec$", "", bitrate_matches))
    bitrate_units <- sub("^\\d+(\\.\\d+)?\\s+", "", bitrate_matches)
    
    # Convert to Mbps
    bitrate_mbps <- bitrate_values
    kbps_idx <- grepl("Kbits/sec", bitrate_units)
    gbps_idx <- grepl("Gbits/sec", bitrate_units)
    
    bitrate_mbps[kbps_idx] <- bitrate_mbps[kbps_idx] / 1024
    bitrate_mbps[gbps_idx] <- bitrate_mbps[gbps_idx] * 1024
    
    # Extract UDP metrics if present
    jitter_ms <- rep(NA, length(data_lines))
    lost_packets <- rep(NA, length(data_lines))
    total_packets <- rep(NA, length(data_lines))
    lost_percent <- rep(NA, length(data_lines))
    
    if(is_udp) {
      # Pattern like "/0.000 ms, 0/923 (0%)"
      udp_pattern <- "(\\d+(\\.\\d+)?)\\s+ms,\\s+(\\d+)/(\\d+)\\s+\\((\\d+(\\.\\d+)?)%\\)"
      udp_matches <- gregexpr(udp_pattern, data_lines)
      
      for(i in 1:length(data_lines)) {
        if(udp_matches[[i]][1] > 0) {
          match_text <- regmatches(data_lines[i], udp_matches[[i]])
          if(length(match_text) > 0) {
            parts <- strsplit(match_text, "\\s+ms,\\s+|/|\\s+\\(|%\\)")[[1]]
            jitter_ms[i] <- as.numeric(parts[1])
            lost_packets[i] <- as.numeric(parts[2])
            total_packets[i] <- as.numeric(parts[3])
            lost_percent[i] <- as.numeric(parts[4])
          }
        }
      }
    }
    
    # Extract summary data
    summary_pattern <- ".*\\s+(\\d+(\\.\\d+)?\\s+\\w+)\\s+(\\d+(\\.\\d+)?\\s+\\w+/sec)"
    summary_sender_line <- grep("sender", raw_lines[grep(summary_pattern, raw_lines)], value = TRUE)
    summary_receiver_line <- grep("receiver", raw_lines[grep(summary_pattern, raw_lines)], value = TRUE)
    
    total_sent_bytes <- NA
    total_sent_bitrate <- NA
    
    if(length(summary_sender_line) > 0) {
      transfer_match <- regexpr("\\d+(\\.\\d+)?\\s+\\w+", summary_sender_line)
      if(transfer_match > 0) {
        transfer_str <- substr(summary_sender_line, transfer_match, 
                            transfer_match + attr(transfer_match, "match.length") - 1)
        transfer_val <- as.numeric(sub("\\s+\\w+$", "", transfer_str))
        transfer_unit <- sub("^\\d+(\\.\\d+)?\\s+", "", transfer_str)
        
        # Convert to bytes
        if(grepl("KB|KBytes", transfer_unit)) {
          total_sent_bytes <- transfer_val * 1024
        } else if(grepl("MB|MBytes", transfer_unit)) {
          total_sent_bytes <- transfer_val * 1024 * 1024
        } else if(grepl("GB|GBytes", transfer_unit)) {
          total_sent_bytes <- transfer_val * 1024 * 1024 * 1024
        }
      }
      
      # Extract bitrate
      bitrate_match <- regexpr("\\d+(\\.\\d+)?\\s+\\w+/sec", summary_sender_line)
      if(bitrate_match > 0) {
        bitrate_str <- substr(summary_sender_line, bitrate_match, 
                           bitrate_match + attr(bitrate_match, "match.length") - 1)
        bitrate_val <- as.numeric(sub("\\s+\\w+/sec$", "", bitrate_str))
        bitrate_unit <- sub("^\\d+(\\.\\d+)?\\s+", "", bitrate_str)
        
        # Convert to Mbps
        if(grepl("Kbits/sec", bitrate_unit)) {
          total_sent_bitrate <- bitrate_val / 1024
        } else if(grepl("Mbits/sec", bitrate_unit)) {
          total_sent_bitrate <- bitrate_val
        } else if(grepl("Gbits/sec", bitrate_unit)) {
          total_sent_bitrate <- bitrate_val * 1024
        }
      }
    }
    
    # Create data frame with parsed data
    df <- data.frame(
      interval_id = seq_along(data_lines),
      stream_id = stream_ids,
      interval_start = interval_starts,
      interval_end = interval_ends,
      interval_length = interval_ends - interval_starts,
      bytes = bytes,
      bitrate_mbps = bitrate_mbps,
      stringsAsFactors = FALSE
    )
    
    # Add UDP metrics if present
    if(is_udp) {
      df$jitter_ms <- jitter_ms
      df$lost_packets <- lost_packets
      df$total_packets <- total_packets
      df$lost_percent <- lost_percent
    }
    
    # Add file metadata
    df$file <- basename(file_path)
    
    # Add connection metadata
    df$host <- host
    df$local_ip <- local_ip
    df$remote_ip <- remote_ip
    df$protocol <- protocol
    df$total_sent_bytes <- total_sent_bytes
    df$total_sent_bitrate <- total_sent_bitrate
    
    # Extract user and ISP from filename
    file_name <- basename(file_path)
    parts <- strsplit(file_name, "_")[[1]]
    
    if(length(parts) >= 1) df$user_name <- parts[1]
    if(length(parts) >= 3) df$isp <- paste(parts[2:3], collapse = " ")
    
    # Parse test date from filename
    date_str <- NA
    time_str <- NA
    
    # Look for date pattern (like "20250316") and time pattern (like "064526")
    for(i in 1:length(parts)) {
      if(nchar(parts[i]) == 8 && grepl("^\\d{8}$", parts[i])) {
        date_str <- parts[i]
        if(i < length(parts) && nchar(parts[i+1]) == 6 && grepl("^\\d{6}$", parts[i+1])) {
          time_str <- parts[i+1]
        }
        break
      }
    }
    
    if(!is.na(date_str) && !is.na(time_str)) {
      df$test_datetime <- as.POSIXct(
        paste0(
          substr(date_str, 1, 4), "-", 
          substr(date_str, 5, 6), "-", 
          substr(date_str, 7, 8), " ", 
          substr(time_str, 1, 2), ":", 
          substr(time_str, 3, 4), ":", 
          substr(time_str, 5, 6)
        ), 
        format="%Y-%m-%d %H:%M:%S"
      )
      df$date_formatted <- format(df$test_datetime, "%Y-%m-%d")
      df$time_formatted <- format(df$test_datetime, "%H:%M:%S")
      df$datetime_readable <- format(df$test_datetime, "%Y-%m-%d %H:%M:%S")
    }
    
    return(df)
  }, error = function(e) {
    warning("Error parsing raw iperf file: ", basename(file_path), " - ", e$message)
    return(NULL)
  })
}

# Enhance process_tcp_data to check for raw iperf files
process_tcp_data <- function(files) {
  if(length(files) == 0) {
    message("No files to process.")
    return(NULL)
  }
  
  # Initialize empty data frame
  all_data <- data.frame()
  
  for(file in files) {
    message("Processing file: ", basename(file))
    
    tryCatch({
      # Check if this is a raw iperf output file
      if(grepl("\\.csv$", file) || grepl("\\.txt$", file)) {
        first_lines <- readLines(file, n = 10)
        is_raw_iperf <- any(grepl("^\\s*\\[\\s*\\d+\\]\\s+\\d+\\.\\d+-\\d+\\.\\d+\\s+sec", first_lines))
        
        if(is_raw_iperf) {
          message("Detected raw iperf output file, using specialized parser")
          file_data <- parse_raw_iperf_output(file)
        } else {
          # Standard CSV processing
          file_data <- process_csv_file(file)
        }
      } else {
        warning("Unknown file type: ", file)
        next
      }
      
      if(!is.null(file_data) && nrow(file_data) > 0) {
        # Add filename for reference
        file_data$file <- basename(file)
        all_data <- rbind(all_data, file_data)
      }
    }, error = function(e) {
      warning("Error processing file ", basename(file), ": ", e$message)
    })
  }
  
  # If we have data, add datetime field for filtering
  if(nrow(all_data) > 0) {
    if("timestamp" %in% names(all_data)) {
      all_data$test_datetime <- as.POSIXct(all_data$timestamp, origin="1970-01-01")
    }
    
    # If no bitrate_mbps field exists but bits_per_second does, convert it
    if(!"bitrate_mbps" %in% names(all_data) && "bits_per_second" %in% names(all_data)) {
      all_data$bitrate_mbps <- all_data$bits_per_second / 1e6
    }
  }
  
  return(all_data)
}