library(tidyverse)
library(jsonlite)
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

#' Process iperf JSON files into a tidy dataframe with robust error handling
#'
#' @param files Vector of file paths to iperf JSON files
#' @return Combined dataframe of processed iperf data
process_iperf_files <- function(files) {
  # Initialize an empty list to store data from each file
  all_data <- list()
  
  for(file_path in files) {
    tryCatch({
      # Read the JSON file line by line and combine
      json_lines <- readLines(file_path, warn = FALSE)
      json_text <- paste(json_lines, collapse = "")
      
      # Check if JSON is valid
      if(nchar(json_text) < 10) {
        warning("File appears to be empty or too small: ", file_path)
        next
      }
      
      # Try to parse the JSON with error handling
      iperf_data <- tryCatch({
        fromJSON(json_text, simplifyVector = TRUE, simplifyDataFrame = TRUE, simplifyMatrix = TRUE)
      }, error = function(e) {
        warning("JSON parse error in file ", file_path, ": ", e$message)
        return(NULL)
      })
      
      if(is.null(iperf_data)) {
        next
      }
      
      # Extract user metadata from filename
      metadata <- extract_user_metadata(file_path)
      
      # Process intervals data with enhanced error handling
      if(is.null(iperf_data$intervals) || length(iperf_data$intervals) == 0) {
        warning("No interval data found in ", file_path, ", falling back to summary data")
        
        # Try to extract summary data if intervals are not available
        if(!is.null(iperf_data$end) && !is.null(iperf_data$end$sum_sent)) {
          sum_data <- iperf_data$end$sum_sent
          
          # Create a single row with summary data
          data_row <- tibble(
            interval_id = 1,
            stream_id = 1,
            socket = NA_integer_,
            interval_start = 0,
            interval_end = if(!is.null(sum_data$seconds)) sum_data$seconds else NA_real_,
            interval_length = if(!is.null(sum_data$seconds)) sum_data$seconds else NA_real_,
            bytes = if(!is.null(sum_data$bytes)) sum_data$bytes else NA_integer_,
            bits_per_second = if(!is.null(sum_data$bits_per_second)) sum_data$bits_per_second else NA_real_,
            bitrate_mbps = if(!is.null(sum_data$bits_per_second)) sum_data$bits_per_second / 1e6 else NA_real_,
            retransmits = NA_integer_
          )
          
          # Add metadata
          data_row$file <- basename(file_path)
          data_row$user_name <- metadata$user_name
          data_row$isp <- metadata$isp
          data_row$test_datetime <- metadata$test_datetime
          data_row$date_formatted <- metadata$date_formatted
          data_row$time_formatted <- metadata$time_formatted
          data_row$datetime_readable <- metadata$datetime_readable
          
          # Add to the list
          all_data[[length(all_data) + 1]] <- data_row
          cat("Successfully extracted 1 data row from summary data\n")
          next
        } else {
          warning("Could not extract any usable data from ", file_path)
          next
        }
      }
      
      # Initialize a dataframe to store flattened data
      interval_data <- data.frame()
      
      # Process each interval with error checking
      for(i in seq_along(iperf_data$intervals)) {
        # Safety check
        if(is.null(iperf_data$intervals[[i]])) {
          next
        }
        
        # Get the streams data for this interval
        streams <- iperf_data$intervals[[i]]$streams
        
        # Get the sum for this interval
        sum_data <- iperf_data$intervals[[i]]$sum
        
        # If no streams, try using sum data
        if(is.null(streams) || length(streams) == 0) {
          if(!is.null(sum_data)) {
            # Create a row for the sum with safe field access
            row_data <- data.frame(
              interval_id = i,
              stream_id = 0,  # 0 indicates sum data
              socket = NA_integer_,
              interval_start = if(!is.null(sum_data$start)) sum_data$start else NA_real_,
              interval_end = if(!is.null(sum_data$end)) sum_data$end else NA_real_,
              interval_length = if(!is.null(sum_data$seconds)) sum_data$seconds else NA_real_,
              bytes = if(!is.null(sum_data$bytes)) sum_data$bytes else NA_integer_,
              bits_per_second = if(!is.null(sum_data$bits_per_second)) sum_data$bits_per_second else NA_real_,
              bitrate_mbps = if(!is.null(sum_data$bits_per_second)) sum_data$bits_per_second / 1e6 else NA_real_,
              retransmits = if(!is.null(sum_data$retransmits)) sum_data$retransmits else NA_integer_
            )
            
            # Add to the dataframe
            interval_data <- rbind(interval_data, row_data)
          }
          next
        }
        
        # For each stream in this interval
        for(j in seq_along(streams)) {
          stream <- streams[[j]]
          
          # Skip if stream is NULL or missing required data
          if(is.null(stream) || is.null(stream$bits_per_second)) {
            next
          }
          
          # Create a row for this stream with safe field access
          row_data <- data.frame(
            interval_id = i,
            stream_id = j,
            socket = if(!is.null(stream$socket)) stream$socket else NA_integer_,
            interval_start = if(!is.null(stream$start)) stream$start else NA_real_,
            interval_end = if(!is.null(stream$end)) stream$end else NA_real_,
            interval_length = if(!is.null(stream$seconds)) stream$seconds else NA_real_,
            bytes = if(!is.null(stream$bytes)) stream$bytes else NA_integer_,
            bits_per_second = stream$bits_per_second,
            bitrate_mbps = stream$bits_per_second / 1e6,  # Convert to Mbps
            retransmits = if(!is.null(stream$retransmits)) stream$retransmits else NA_integer_
          )
          
          # Add to the dataframe
          interval_data <- rbind(interval_data, row_data)
        }
      }
      
      # Add metadata to each row
      interval_data$file <- basename(file_path)
      interval_data$user_name <- metadata$user_name
      interval_data$isp <- metadata$isp
      interval_data$test_datetime <- metadata$test_datetime
      interval_data$date_formatted <- metadata$date_formatted
      interval_data$time_formatted <- metadata$time_formatted
      interval_data$datetime_readable <- metadata$datetime_readable
      
      # Add to the list of all data
      all_data[[length(all_data) + 1]] <- interval_data
      
      # Log success
      cat("Successfully processed", nrow(interval_data), "data rows from", file_path, "\n")
      
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