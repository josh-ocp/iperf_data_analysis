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
      
      # Read the CSV file
      csv_data <- read_csv(file_path, show_col_types = FALSE)
      
      # Check if CSV is valid
      if(nrow(csv_data) == 0) {
        warning("File appears to be empty: ", file_path)
        next
      }
      
      # Extract user metadata from filename
      metadata <- extract_user_metadata(file_path)
      
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