library(tidyverse)
library(patchwork)  # For combining plots
library(scales)     # For nice formatting
library(lubridate)

#' Plot throughput over time for a single test
#'
#' @param data Processed iperf data
#' @param title Plot title
#' @return ggplot object
plot_throughput <- function(data, title = "Network Throughput") {
  # Sort data by time
  data <- data %>% arrange(interval_start)
  
  # If we have test_date and test_time, add them to the title
  if("test_date" %in% names(data) && !is.na(data$test_date[1]) && 
     "test_time" %in% names(data) && !is.na(data$test_time[1])) {
    
    # Format date nicely
    test_date <- data$test_date[1]
    formatted_date <- paste0(
      substr(test_date, 1, 4), "-",
      substr(test_date, 5, 6), "-",
      substr(test_date, 7, 8)
    )
    
    # Format time nicely
    test_time <- data$test_time[1]
    formatted_time <- paste0(
      substr(test_time, 1, 2), ":",
      substr(test_time, 3, 4)
    )
    
    title <- paste0(title, " (", formatted_date, " ", formatted_time, ")")
  }
  
  # Create the basic throughput plot
  p <- data %>%
    ggplot(aes(x = interval_start, y = bitrate_mbps)) +
    geom_line(color = "steelblue", linewidth = 0.7) +
    geom_point(alpha = 0.5, size = 1.5, color = "steelblue") +
    # Add a smoothed trend line
    geom_smooth(method = "loess", span = 0.3, se = FALSE, 
                color = "darkred", linetype = "dashed", linewidth = 1) +
    # Add the average as a horizontal line
    geom_hline(yintercept = mean(data$bitrate_mbps), 
               linetype = "dotted", color = "darkgreen", linewidth = 0.8) +
    # Annotate average
    annotate("text", x = min(data$interval_start), 
             y = mean(data$bitrate_mbps) * 1.05,
             label = paste("Avg:", round(mean(data$bitrate_mbps), 2), "Mbps"),
             hjust = 0, color = "darkgreen") +
    # Better labels
    labs(
      title = title,
      subtitle = paste0(nrow(data), " intervals, ", 
                       round(max(data$interval_end) - min(data$interval_start), 1), 
                       " seconds total"),
      x = "Time (seconds)",
      y = "Throughput (Mbps)",
      caption = format(Sys.time(), "Generated: %Y-%m-%d %H:%M:%S")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Special handling for single data point
  if(nrow(data) == 1) {
    # For a single data point, create a bar chart instead of a line
    p <- data %>%
      ggplot(aes(x = "Single Point", y = bitrate_mbps)) +
      geom_col(fill = "steelblue", width = 0.5) +
      geom_text(aes(label = round(bitrate_mbps, 1)), vjust = -0.5) +
      labs(
        title = title,
        subtitle = "Single data point (summary data only)",
        x = "",
        y = "Throughput (Mbps)",
        caption = format(Sys.time(), "Generated: %Y-%m-%d %H:%M:%S")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    return(p)
  }
  
  # If we have enough data points, add distribution histogram
  if(nrow(data) >= 10) {
    # Create histogram/density plot
    p_hist <- data %>%
      ggplot(aes(x = bitrate_mbps)) +
      geom_histogram(fill = "steelblue", alpha = 0.7, bins = min(30, nrow(data)/3)) +
      geom_density(aes(y = after_stat(count) * 0.8), color = "darkred", linewidth = 1) +
      labs(
        title = "Throughput Distribution",
        x = "Throughput (Mbps)",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    # Combine plots using patchwork
    combined <- p / p_hist + plot_layout(heights = c(3, 1))
    return(combined)
  }
  
  return(p)
}

#' Plot UDP specific metrics
#'
#' @param data UDP test data
#' @return ggplot object
plot_udp_metrics <- function(data) {
  # Check if this is UDP data
  if(!"jitter_ms" %in% names(data) || !"lost_percent" %in% names(data)) {
    stop("Data doesn't appear to contain UDP metrics")
  }
  
  # Sort data by time
  data <- data %>% arrange(interval_start)
  
  # Create throughput plot
  p1 <- data %>%
    ggplot(aes(x = interval_start, y = bitrate_mbps)) +
    geom_line(color = "steelblue", linewidth = 0.7) +
    geom_point(alpha = 0.5, size = 1.5, color = "steelblue") +
    geom_smooth(method = "loess", span = 0.3, se = FALSE, 
                color = "darkred", linetype = "dashed", linewidth = 1) +
    labs(
      title = "UDP Throughput",
      x = "Time (seconds)",
      y = "Throughput (Mbps)"
    ) +
    theme_minimal()
  
  # Create jitter plot
  p2 <- data %>%
    ggplot(aes(x = interval_start, y = jitter_ms)) +
    geom_line(color = "darkgreen", linewidth = 0.7) +
    geom_point(alpha = 0.5, size = 1.5, color = "darkgreen") +
    labs(
      title = "Jitter",
      x = "Time (seconds)",
      y = "Jitter (ms)"
    ) +
    theme_minimal()
  
  # Create packet loss plot
  p3 <- data %>%
    ggplot(aes(x = interval_start, y = lost_percent)) +
    geom_line(color = "firebrick", linewidth = 0.7) +
    geom_point(alpha = 0.5, size = 1.5, color = "firebrick") +
    labs(
      title = "Packet Loss",
      x = "Time (seconds)",
      y = "Loss (%)"
    ) +
    theme_minimal()
  
  # Create correlation plot between jitter and loss if we have enough data
  p4 <- data %>%
    ggplot(aes(x = jitter_ms, y = lost_percent)) +
    geom_point(alpha = 0.6, color = "purple", size = 2) +
    geom_smooth(method = "lm", color = "black", linetype = "dashed", linewidth = 1) +
    annotate("text", x = min(data$jitter_ms), y = max(data$lost_percent) * 0.9,
             label = paste("Correlation:", round(cor(data$jitter_ms, data$lost_percent), 3)),
             hjust = 0) +
    labs(
      title = "Jitter vs. Packet Loss",
      x = "Jitter (ms)",
      y = "Loss (%)"
    ) +
    theme_minimal()
  
  # Combine all plots
  combined_plot <- (p1 + p2) / (p3 + p4) + 
    plot_annotation(
      title = "UDP Performance Metrics",
      subtitle = paste("Test:", unique(data$file)),
      caption = format(Sys.time(), "Generated: %Y-%m-%d %H:%M:%S"),
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
  
  return(combined_plot)
}

#' Create a heatmap of network metrics over time
#'
#' @param data UDP test data
#' @param segment_size Size of time segments in seconds
#' @return ggplot object
plot_performance_heatmap <- function(data, segment_size = 10) {
  # Add segment identifier based on interval_start
  data_with_segments <- data %>%
    mutate(segment = floor(interval_start / segment_size))
  
  # Analyze each segment
  segment_analysis <- data_with_segments %>%
    group_by(segment) %>%
    summarize(
      start_time = min(interval_start),
      avg_bitrate_mbps = mean(bitrate_mbps),
      stability_cv = sd(bitrate_mbps) / mean(bitrate_mbps) * 100,
      .groups = "drop"
    )
  
  # Add UDP-specific segment analysis if available
  if("lost_percent" %in% names(data)) {
    udp_segments <- data_with_segments %>%
      group_by(segment) %>%
      summarize(
        avg_jitter_ms = mean(jitter_ms),
        avg_loss_percent = mean(lost_percent),
        .groups = "drop"
      )
    
    segment_analysis <- segment_analysis %>%
      left_join(udp_segments, by = "segment")
  }
  
  # Create long format data for heatmap
  heatmap_data <- segment_analysis %>%
    pivot_longer(
      cols = c(avg_bitrate_mbps, stability_cv, avg_jitter_ms, avg_loss_percent),
      names_to = "metric", 
      values_to = "value"
    ) %>%
    # Format metric names for display
    mutate(metric = case_when(
      metric == "avg_bitrate_mbps" ~ "Throughput (Mbps)",
      metric == "stability_cv" ~ "Stability (CV %)",
      metric == "avg_jitter_ms" ~ "Jitter (ms)",
      metric == "avg_loss_percent" ~ "Packet Loss (%)",
      TRUE ~ metric
    ))
  
  # Create heatmap
  heatmap <- heatmap_data %>%
    ggplot(aes(x = start_time, y = metric, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c() +
    labs(
      title = "Performance Metrics Over Time",
      subtitle = paste("Segment size:", segment_size, "seconds"),
      x = "Time (seconds)",
      y = "Metric",
      fill = "Value"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank()
    )
  
  return(heatmap)
}

#' Compare multiple test files
#'
#' @param data Combined data from multiple iperf tests
#' @return ggplot object
plot_test_comparison <- function(data) {
  # Ensure data has multiple files
  if(length(unique(data$file)) <= 1) {
    stop("Need multiple test files for comparison")
  }
  
  # Plot throughput comparison
  p1 <- data %>%
    ggplot(aes(x = interval_start, y = bitrate_mbps, color = file)) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Throughput Comparison",
      x = "Time (seconds)",
      y = "Throughput (Mbps)",
      color = "Test File"
    ) +
    theme_minimal()
  
  # Create boxplot comparison
  p2 <- data %>%
    ggplot(aes(x = file, y = bitrate_mbps, fill = file)) +
    geom_boxplot() +
    stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
    labs(
      title = "Throughput Distribution by Test",
      x = "Test File",
      y = "Throughput (Mbps)",
      fill = "Test File"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # UDP-specific comparison if available
  if("lost_percent" %in% names(data)) {
    p3 <- data %>%
      ggplot(aes(x = file, y = lost_percent, fill = file)) +
      geom_boxplot() +
      labs(
        title = "Packet Loss by Test",
        x = "Test File",
        y = "Loss (%)",
        fill = "Test File"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    combined <- (p1 / p2 / p3) + 
      plot_annotation(title = "Test Comparison", theme = theme(plot.title = element_text(face = "bold")))
  } else {
    combined <- (p1 / p2) + 
      plot_annotation(title = "Test Comparison", theme = theme(plot.title = element_text(face = "bold")))
  }
  
  return(combined)
}

#' Compare performance across different users (TCP only)
#'
#' @param data Combined data from multiple users
#' @return ggplot object
plot_user_comparison <- function(data) {
  # Check if we have user information
  if(!"user_name" %in% names(data) || !"isp" %in% names(data)) {
    stop("Data doesn't contain user information")
  }
  
  # Create a user+ISP label for clarity
  data <- data %>%
    mutate(user_label = paste0(user_name, " (", isp, ")"))
  
  # Bar chart of average throughput by user
  p1 <- data %>%
    group_by(user_label) %>%
    summarize(
      avg_bitrate = mean(bitrate_mbps),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = reorder(user_label, avg_bitrate), y = avg_bitrate, fill = user_label)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(avg_bitrate, 1)), hjust = -0.1) + 
    coord_flip() +
    labs(
      title = "Average Throughput by User",
      x = "",
      y = "Average Throughput (Mbps)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Boxplot of throughput distribution by user
  p2 <- data %>%
    ggplot(aes(x = reorder(user_label, bitrate_mbps, FUN = median), y = bitrate_mbps, fill = user_label)) +
    geom_boxplot() +
    coord_flip() +
    labs(
      title = "Throughput Distribution by User",
      x = "",
      y = "Throughput (Mbps)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Combine plots
  combined <- p1 / p2 + 
    plot_layout(heights = c(1, 1.5)) + 
    plot_annotation(
      title = "User Connection Comparison", 
      subtitle = paste("Based on", length(unique(data$file)), "tests from", length(unique(data$user_label)), "users"),
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
  
  return(combined)
}

#' Plot detailed throughput over time with statistics
#'
#' @param data Processed iperf data with interval details
#' @param title Plot title
#' @return ggplot object
plot_throughput_detailed <- function(data, title = "Network Throughput") {
  # Sort data by time
  data <- data %>% arrange(interval_start)
  
  # Calculate key statistics
  avg_throughput <- mean(data$bitrate_mbps)
  median_throughput <- median(data$bitrate_mbps)
  max_throughput <- max(data$bitrate_mbps)
  min_throughput <- min(data$bitrate_mbps)
  
  # Handle std_dev safely (can be NA with single data point)
  if(nrow(data) > 1) {
    std_dev <- sd(data$bitrate_mbps)
    cv_percent <- (std_dev / avg_throughput) * 100 # coefficient of variation
  } else {
    std_dev <- 0
    cv_percent <- 0
  }
  
  # Create the detailed time series plot
  p <- data %>%
    ggplot(aes(x = interval_start, y = bitrate_mbps)) +
    # Add the throughput line
    geom_line(color = "steelblue", linewidth = 0.7) +
    geom_point(alpha = 0.3, size = 1, color = "steelblue") +
    # Add reference lines
    geom_hline(yintercept = avg_throughput, 
               linetype = "dashed", color = "darkred", linewidth = 0.8) +
    # Add labels for reference lines
    annotate("text", x = min(data$interval_start), 
             y = avg_throughput * 1.05,
             label = paste("Mean:", round(avg_throughput, 1), "Mbps"),
             hjust = 0, color = "darkred", size = 3) +
    # Better labels
    labs(
      title = title,
      subtitle = paste0(
        nrow(data), " intervals, ", 
        round(max(data$interval_end) - min(data$interval_start), 1), " seconds total\n",
        "Range: ", round(min_throughput, 1), " - ", round(max_throughput, 1), " Mbps"
      ),
      x = "Time (seconds)",
      y = "Throughput (Mbps)",
      caption = format(Sys.time(), "Generated: %Y-%m-%d %H:%M:%S")
    ) +
    # Improve appearance
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Add standard deviation information if we have multiple data points
  if(nrow(data) > 1) {
    # Add shaded area for standard deviation range
    p <- p + geom_ribbon(aes(ymin = avg_throughput - std_dev, 
                             ymax = avg_throughput + std_dev),
                         fill = "lightblue", alpha = 0.3)
    
    # Update subtitle to include std dev
    p <- p + labs(subtitle = paste0(
      nrow(data), " intervals, ", 
      round(max(data$interval_end) - min(data$interval_start), 1), " seconds total\n",
      "Range: ", round(min_throughput, 1), " - ", round(max_throughput, 1), " Mbps, ",
      "Std Dev: ", round(std_dev, 1), " Mbps (CV: ", round(cv_percent, 1), "%)"
    ))
    
    # Add median line if we have multiple data points
    p <- p + geom_hline(yintercept = median_throughput, 
                        linetype = "dotted", color = "darkgreen", linewidth = 0.8) +
            annotate("text", x = min(data$interval_start), 
                    y = median_throughput * 0.95,
                    label = paste("Median:", round(median_throughput, 1), "Mbps"),
                    hjust = 0, color = "darkgreen", size = 3)
    
    # Add trend line if we have enough points
    if(nrow(data) >= 3) {
      p <- p + geom_smooth(method = "loess", span = 0.2, se = FALSE, 
                          color = "purple", linetype = "solid", linewidth = 1)
    }
  }
  
  # Create histogram of throughput distribution if we have enough data points
  if(nrow(data) > 1) {
    # Calculate a safe binwidth
    safe_binwidth <- diff(range(data$bitrate_mbps)) / max(1, min(30, nrow(data)))
    
    p_hist <- data %>%
      ggplot(aes(x = bitrate_mbps)) +
      geom_histogram(binwidth = safe_binwidth, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = avg_throughput, color = "darkred", 
                linetype = "dashed", linewidth = 0.8) +
      geom_vline(xintercept = median_throughput, color = "darkgreen", 
                linetype = "dotted", linewidth = 0.8) +
      labs(
        title = "Throughput Distribution",
        x = "Throughput (Mbps)",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
    
    # Combine with main plot
    combined <- p / p_hist + plot_layout(heights = c(3, 1))
    return(combined)
  }
  
  # For single data point, just return the main plot
  return(p)
}

#' Plot summary of throughput by date
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @return A ggplot object
plot_daily_summary <- function(tcp_data) {
  # Check for test_datetime field first
  if(!"test_datetime" %in% names(tcp_data)) {
    # Create a simple placeholder plot if date info not available
    return(
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Date information not available in data") +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()
        )
    )
  }
  
  # Regular processing with date information
  daily_data <- group_by_date(tcp_data)
  
  ggplot(daily_data, aes(x = test_date)) +
    geom_line(aes(y = avg_throughput), size = 1, color = "steelblue") +
    geom_ribbon(aes(ymin = min_throughput, ymax = max_throughput),
                alpha = 0.2, fill = "steelblue") +
    labs(
      title = "Daily Network Performance Summary",
      subtitle = paste("Based on", length(unique(tcp_data$file)), "iperf tests"),
      x = "Date",
      y = "Throughput (Mbps)",
      caption = "Ribbon shows min-max range, line shows average"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

#' Plot throughput by time of day
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @return A ggplot object
plot_time_of_day <- function(tcp_data) {
  # Try to create time of day data, with error handling
  tryCatch({
    # Check if test_datetime is available
    if(!"test_datetime" %in% names(tcp_data)) {
      # Create a helpful error message plot
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No test_datetime column found in data. Try refreshing the data or check parsing.") +
        theme_void())
    }
    
    # Check if all datetime values are NA
    if(all(is.na(tcp_data$test_datetime))) {
      # Try to construct datetime from individual components if available
      if(all(c("test_date", "test_time") %in% names(tcp_data))) {
        message("Converting test_date and test_time to test_datetime")
        tcp_data <- tcp_data %>%
          mutate(test_datetime = case_when(
            !is.na(test_date) & !is.na(test_time) ~ {
              # Parse date (assumed to be in YYYYMMDD format)
              year <- as.integer(substr(test_date, 1, 4))
              month <- as.integer(substr(test_date, 5, 6))
              day <- as.integer(substr(test_date, 7, 8))
              
              # Parse time (assumed to be in HHMMSS format)
              hour <- as.integer(substr(test_time, 1, 2))
              minute <- as.integer(substr(test_time, 3, 4))
              second <- if(nchar(test_time) >= 6) as.integer(substr(test_time, 5, 6)) else 0
              
              # Create datetime
              ymd_hms(paste(year, month, day, hour, minute, second, sep="-"))
            },
            TRUE ~ NA_POSIXct_
          ))
      } else {
        # Create a helpful error message plot
        return(ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "All test_datetime values are NA and could not reconstruct from test_date/test_time") +
          theme_void())
      }
    }
    
    # Debug info about test_datetime
    valid_dates <- sum(!is.na(tcp_data$test_datetime))
    total_rows <- nrow(tcp_data)
    message("test_datetime: ", valid_dates, "/", total_rows, " valid values (", 
            round(valid_dates/total_rows*100, 1), "%)")
    
    # Now call the original implementation
    tod_data <- group_by_time_of_day(tcp_data)
    
    ggplot(tod_data, aes(x = time_period, y = avg_throughput)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_errorbar(aes(ymin = pmax(0, avg_throughput - stability), 
                        ymax = avg_throughput + stability),
                    width = 0.2) +
      labs(
        title = "Network Performance by Time of Day",
        subtitle = paste0("Average throughput with standard deviation\n",
                         "Based on ", valid_dates, " valid datetime values"),
        x = "Time of Day",
        y = "Throughput (Mbps)",
        caption = paste0("Based on ", length(unique(tcp_data$file)), " tests")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }, error = function(e) {
    message("Error in plot_time_of_day: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste0("Could not create Time of Day plot:\n", e$message)) +
      theme_void()
  })
}

#' Create a heatmap of throughput over time
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @return A ggplot object
plot_throughput_heatmap <- function(tcp_data) {
  # Ensure we have datetime
  if(!"hour" %in% colnames(tcp_data)) {
    tcp_data <- tcp_data %>%
      mutate(
        hour = hour(test_datetime),
        day = day(test_datetime)
      )
  }
  
  # Aggregate by hour and day
  heatmap_data <- tcp_data %>%
    group_by(day, hour) %>%
    summarize(
      avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(heatmap_data, aes(x = hour, y = day, fill = avg_throughput)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Mbps", option = "plasma") +
    scale_x_continuous(breaks = seq(0, 23, 3),
                      labels = paste0(seq(0, 23, 3), ":00")) +
    labs(
      title = "Throughput Heatmap by Hour and Day",
      x = "Hour of Day",
      y = "Day of Month",
      caption = "Color indicates average throughput"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "right"
    )
}

#' Create a boxplot comparison of tests
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param max_tests Maximum number of tests to include (to avoid clutter)
#' @return A ggplot object
plot_test_comparison_boxplot <- function(tcp_data, max_tests = 8) {
  # If we have too many tests, select the most recent ones
  if(length(unique(tcp_data$file)) > max_tests) {
    # Check if we have datetime information to select most recent
    if("test_datetime" %in% names(tcp_data) && !all(is.na(tcp_data$test_datetime))) {
      test_dates <- tcp_data %>%
        group_by(file) %>%
        summarize(test_datetime = first(test_datetime), .groups = "drop") %>%
        arrange(desc(test_datetime)) %>%
        slice_head(n = max_tests) %>%
        pull(file)
    } else {
      # If no datetime info, just take the first max_tests
      test_dates <- unique(tcp_data$file)[1:max_tests]
    }
    
    tcp_data <- tcp_data %>%
      filter(file %in% test_dates)
  }
  
  # Create test labels - use datetime if available, otherwise use file names
  # Add a test_label column with default value (filename without path)
  tcp_data$test_label <- basename(as.character(tcp_data$file))
  
  # Update labels if we have datetime
  if("test_datetime" %in% names(tcp_data)) {
    # For rows where test_datetime is not NA, use formatted datetime
    valid_datetime <- !is.na(tcp_data$test_datetime)
    if(any(valid_datetime)) {
      tcp_data$test_label[valid_datetime] <- format(tcp_data$test_datetime[valid_datetime], "%m-%d %H:%M")
    }
  } else if(all(c("test_date", "test_time") %in% names(tcp_data))) {
    # If we have date and time columns, try those instead
    valid_date_time <- !is.na(tcp_data$test_date) & !is.na(tcp_data$test_time)
    if(any(valid_date_time)) {
      tcp_data$test_label[valid_date_time] <- paste(
        tcp_data$test_date[valid_date_time], 
        tcp_data$test_time[valid_date_time]
      )
    }
  }
  
  # Create the boxplot
  ggplot(tcp_data, aes(x = test_label, y = bitrate_mbps)) +
    geom_boxplot(aes(fill = test_label), alpha = 0.7) +
    scale_fill_viridis_d() +
    labs(
      title = "Comparison of Recent iperf Tests",
      subtitle = "Distribution of throughput measurements",
      x = "Test Date/Time",
      y = "Throughput (Mbps)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

#' Create a comprehensive dashboard of test results
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @return A combined ggplot object using patchwork
create_performance_dashboard <- function(tcp_data) {
  # Check if we have test_datetime before creating time-based plots
  has_datetime <- "test_datetime" %in% names(tcp_data) && 
                   !all(is.na(tcp_data$test_datetime))
  
  # Create individual plots - fallback to basic plots if needed
  p1 <- tryCatch({
    if(has_datetime) {
      plot_daily_summary(tcp_data)
    } else {
      # Basic throughput plot if no datetime information
      plot_throughput(tcp_data, "Network Throughput Summary")
    }
  }, error = function(e) {
    message("Error in plot_daily_summary: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
      theme_void()
  })
  
  p2 <- tryCatch({
    if(has_datetime) {
      plot_time_of_day(tcp_data)
    } else {
      # Histogram of throughput values
      tcp_data %>%
        ggplot(aes(x = bitrate_mbps)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        labs(title = "Throughput Distribution", x = "Throughput (Mbps)", y = "Count") +
        theme_minimal()
    }
  }, error = function(e) {
    message("Error in second plot: ", e$message)
    ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Error generating plot") + theme_void()
  })
  
  p3 <- tryCatch({
    plot_test_comparison_boxplot(tcp_data)
  }, error = function(e) {
    message("Error in plot_test_comparison_boxplot: ", e$message)
    ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Error generating plot") + theme_void()
  })
  
  # Create a dashboard layout
  (p1 + p2) / p3 + 
    plot_layout(heights = c(2, 1)) +
    plot_annotation(
      title = "Network Performance Dashboard",
      subtitle = paste("Analysis of", length(unique(tcp_data$file)), "iPerf Test(s)"),
      caption = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      theme = theme(plot.title = element_text(face = "bold", size = 16))
    )
}

#' Add to visualize_iperf.R

#' Plot time series comparison of multiple tests
#'
#' @param tcp_data Combined dataframe of all TCP test data 
#' @param color_by Variable to use for coloring (default: "file")
#' @param max_series Maximum number of series to show (default: 10)
#' @param smooth Whether to add smoothed trend lines (default: TRUE)
#' @return A ggplot object
plot_time_series_comparison <- function(tcp_data, color_by = "file", max_series = 10, smooth = TRUE) {
  # Ensure we have proper time information
  if(!"test_datetime" %in% names(tcp_data) || all(is.na(tcp_data$test_datetime))) {
    stop("Test datetime information is needed for time series comparison")
  }
  
  # For each file, standardize the time to start at 0
  tcp_data <- tcp_data %>%
    group_by(file) %>%
    mutate(
      relative_time = interval_start - first(interval_start),
      actual_time = test_datetime + seconds(interval_start)
    ) %>%
    ungroup()
  
  # Get the variable to color by
  if(!color_by %in% names(tcp_data)) {
    warning("Color variable not found, using file name")
    color_by <- "file"
  }
  
  # Limit number of series to avoid overplotting
  unique_values <- unique(tcp_data[[color_by]])
  if(length(unique_values) > max_series) {
    # If we have too many, take the most recent ones based on datetime
    if(color_by == "file") {
      # For files, get the most recent ones
      newest_files <- tcp_data %>%
        group_by(file) %>%
        summarize(last_time = max(test_datetime), .groups = "drop") %>%
        arrange(desc(last_time)) %>%
        slice_head(n = max_series) %>%
        pull(file)
      
      tcp_data <- tcp_data %>%
        filter(file %in% newest_files)
    } else {
      # For other variables, just take the first max_series values
      keep_values <- unique_values[1:max_series]
      tcp_data <- tcp_data %>%
        filter(!!sym(color_by) %in% keep_values)
    }
    
    message("Limited to ", max_series, " series to avoid overplotting")
  }
  
  # Plot the time series - Two options: by relative time or actual time
  
  # Option 1: Using relative time (aligned to start at 0)
  p1 <- tcp_data %>%
    ggplot(aes(x = relative_time, y = bitrate_mbps, color = !!sym(color_by), group = interaction(file, !!sym(color_by)))) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Network Performance Comparison Over Test Duration",
      x = "Time from start (seconds)",
      y = "Throughput (Mbps)",
      color = str_to_title(gsub("_", " ", color_by))
    ) +
    theme_minimal()
  
  # Option 2: Using actual datetime
  p2 <- tcp_data %>%
    ggplot(aes(x = actual_time, y = bitrate_mbps, color = !!sym(color_by), group = interaction(file, !!sym(color_by)))) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Network Performance Over Time",
      x = "Date/Time",
      y = "Throughput (Mbps)",
      color = str_to_title(gsub("_", " ", color_by))
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add smoothing if requested
  if(smooth) {
    p1 <- p1 + geom_smooth(method = "loess", se = FALSE, span = 0.3, linewidth = 1.2)
    p2 <- p2 + geom_smooth(method = "loess", se = FALSE, span = 0.3, linewidth = 1.2)
  }
  
  # Combine the plots
  combined <- p1 / p2 + plot_annotation(
    title = "Network Performance Time Series Analysis",
    subtitle = paste("Comparing", length(unique(tcp_data[[color_by]])), "different", gsub("_", " ", color_by), "values"),
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )
  
  return(combined)
}

#' Plot network performance trends over time
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param time_unit Unit for time grouping ('day', 'week', 'month', etc.)
#' @param facet_by Optional variable to facet by (e.g., "user_name") 
#' @return A ggplot object
plot_performance_trends <- function(tcp_data, time_unit = "day", facet_by = NULL) {
  # Try to analyze trends with error handling
  tryCatch({
    # Check if test_datetime is available
    if(!"test_datetime" %in% names(tcp_data)) {
      # Create a helpful error message plot
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No test_datetime column found in data. Try refreshing the data or check parsing.") +
        theme_void())
    }
    
    # Check if all datetime values are NA
    if(all(is.na(tcp_data$test_datetime))) {
      # Try to construct datetime from individual components if available
      if(all(c("test_date", "test_time") %in% names(tcp_data))) {
        message("Converting test_date and test_time to test_datetime")
        tcp_data <- tcp_data %>%
          mutate(test_datetime = case_when(
            !is.na(test_date) & !is.na(test_time) ~ {
              # Parse date (assumed to be in YYYYMMDD format)
              year <- as.integer(substr(test_date, 1, 4))
              month <- as.integer(substr(test_date, 5, 6))
              day <- as.integer(substr(test_date, 7, 8))
              
              # Parse time (assumed to be in HHMMSS format)
              hour <- as.integer(substr(test_time, 1, 2))
              minute <- as.integer(substr(test_time, 3, 4))
              second <- if(nchar(test_time) >= 6) as.integer(substr(test_time, 5, 6)) else 0
              
              # Create datetime
              ymd_hms(paste(year, month, day, hour, minute, second, sep="-"))
            },
            TRUE ~ NA_POSIXct_
          ))
      } else {
        # Create a helpful error message plot
        return(ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "All test_datetime values are NA and could not reconstruct from test_date/test_time") +
          theme_void())
      }
    }
    
    # Debug info about test_datetime
    valid_dates <- sum(!is.na(tcp_data$test_datetime))
    total_rows <- nrow(tcp_data)
    message("test_datetime: ", valid_dates, "/", total_rows, " valid values (", 
            round(valid_dates/total_rows*100, 1), "%)")
    
    # If we have too few valid datetime values, warn the user
    if(valid_dates < 10) {
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste0("Only ", valid_dates, " valid datetime values available.\n",
                               "Need more data points for meaningful trend analysis.")) +
        theme_void())
    }
    
    # Get trends data using only rows with valid datetime
    valid_data <- tcp_data %>% filter(!is.na(test_datetime))
    trends_data <- analyze_time_trends(valid_data, time_unit)
    
    # Create base plot
    p <- trends_data %>%
      ggplot(aes(x = time_group, y = avg_throughput)) +
      geom_line(size = 1, color = "steelblue") +
      geom_point(aes(size = test_count), color = "steelblue") +
      geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                      ymax = avg_throughput + stability), 
                  alpha = 0.2, fill = "steelblue") +
      labs(
        title = paste("Network Performance Trends by", str_to_title(time_unit)),
        subtitle = paste0("Average throughput with standard deviation bands\n",
                         "Using ", valid_dates, " valid datetime values"),
        x = str_to_title(time_unit),
        y = "Throughput (Mbps)",
        size = "Tests"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested and the column exists
    if(!is.null(facet_by) && facet_by %in% names(valid_data)) {
      # We need to regenerate the trends data with the facet variable
      facet_trends <- valid_data %>%
        group_by(!!sym(facet_by)) %>%
        group_modify(~ analyze_time_trends(.x, time_unit)) %>%
        ungroup()
      
      # Create faceted plot
      p <- facet_trends %>%
        ggplot(aes(x = time_group, y = avg_throughput, color = !!sym(facet_by), fill = !!sym(facet_by))) +
        geom_line(size = 1) +
        geom_point(aes(size = test_count)) +
        geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                        ymax = avg_throughput + stability), 
                    alpha = 0.1) +
        labs(
          title = paste("Network Performance Trends by", str_to_title(time_unit)),
          subtitle = paste0("Faceted by ", str_to_title(gsub("_", " ", facet_by)), "\n",
                           "Using ", valid_dates, " valid datetime values"),
          x = str_to_title(time_unit),
          y = "Throughput (Mbps)",
          size = "Tests",
          color = str_to_title(gsub("_", " ", facet_by)),
          fill = str_to_title(gsub("_", " ", facet_by))
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)), scales = "free_y")
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_performance_trends: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste0("Could not create Performance Trends plot:\n", e$message)) +
      theme_void()
  })
}

#' Create a day-of-week by hour heatmap of performance
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param facet_by Optional variable to facet by (e.g., "user_name")
#' @return A ggplot object
plot_time_pattern_heatmap <- function(tcp_data, facet_by = NULL) {
  # Try to create the heatmap with error handling
  tryCatch({
    # Check if test_datetime is available
    if(!"test_datetime" %in% names(tcp_data)) {
      # Create a helpful error message plot
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No test_datetime column found in data. Try refreshing the data or check parsing.") +
        theme_void())
    }
    
    # Check if all datetime values are NA
    if(all(is.na(tcp_data$test_datetime))) {
      # Try to construct datetime from individual components if available
      if(all(c("test_date", "test_time") %in% names(tcp_data))) {
        message("Converting test_date and test_time to test_datetime")
        tcp_data <- tcp_data %>%
          mutate(test_datetime = case_when(
            !is.na(test_date) & !is.na(test_time) ~ {
              # Parse date (assumed to be in YYYYMMDD format)
              year <- as.integer(substr(test_date, 1, 4))
              month <- as.integer(substr(test_date, 5, 6))
              day <- as.integer(substr(test_date, 7, 8))
              
              # Parse time (assumed to be in HHMMSS format)
              hour <- as.integer(substr(test_time, 1, 2))
              minute <- as.integer(substr(test_time, 3, 4))
              second <- if(nchar(test_time) >= 6) as.integer(substr(test_time, 5, 6)) else 0
              
              # Create datetime
              ymd_hms(paste(year, month, day, hour, minute, second, sep="-"))
            },
            TRUE ~ NA_POSIXct_
          ))
      } else {
        # Create a helpful error message plot
        return(ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "All test_datetime values are NA and could not reconstruct from test_date/test_time") +
          theme_void())
      }
    }
    
    # Debug info about test_datetime
    valid_dates <- sum(!is.na(tcp_data$test_datetime))
    total_rows <- nrow(tcp_data)
    message("test_datetime: ", valid_dates, "/", total_rows, " valid values (", 
            round(valid_dates/total_rows*100, 1), "%)")
    
    # Continue only with valid datetime values
    valid_data <- tcp_data %>% filter(!is.na(test_datetime))
    
    # Check if we have enough data for a meaningful heatmap
    if(n_distinct(wday(valid_data$test_datetime)) < 2 && 
       n_distinct(hour(valid_data$test_datetime)) < 3) {
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste0("Not enough time diversity to create a heatmap.\n",
                               "Need data from different days and hours.\n",
                               "Current data spans ", 
                               n_distinct(wday(valid_data$test_datetime)), " days and ",
                               n_distinct(hour(valid_data$test_datetime)), " hours.")) +
        theme_void())
    }
    
    # Extract day of week and hour from datetime
    heatmap_data <- valid_data %>%
      mutate(
        day_of_week = wday(test_datetime, label = TRUE),
        hour_of_day = hour(test_datetime)
      ) %>%
      group_by(day_of_week, hour_of_day) %>%
      summarize(
        avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
        test_count = n_distinct(file),
        data_points = n(),
        .groups = "drop"
      )
    
    # Create heatmap
    p <- heatmap_data %>%
      ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma", name = "Mbps") +
      scale_x_continuous(breaks = seq(0, 23, 3),
                       labels = c("12am", "3am", "6am", "9am", "12pm", "3pm", "6pm", "9pm")) +
      labs(
        title = "Performance Pattern by Day and Hour",
        subtitle = paste0("Average throughput (Mbps)\n",
                         "Using ", valid_dates, " valid datetime values"),
        x = "Hour of Day",
        y = "Day of Week"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested and the column exists
    if(!is.null(facet_by) && facet_by %in% names(valid_data)) {
      facet_heatmap <- valid_data %>%
        mutate(
          day_of_week = wday(test_datetime, label = TRUE),
          hour_of_day = hour(test_datetime)
        ) %>%
        group_by(!!sym(facet_by), day_of_week, hour_of_day) %>%
        summarize(
          avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
          test_count = n_distinct(file),
          data_points = n(),
          .groups = "drop"
        )
      
      p <- facet_heatmap %>%
        ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
        geom_tile() +
        scale_fill_viridis_c(option = "plasma", name = "Mbps") +
        scale_x_continuous(breaks = seq(0, 23, 6),
                         labels = c("12am", "6am", "12pm", "6pm")) +
        labs(
          title = "Performance Pattern by Day and Hour",
          subtitle = paste0("Faceted by ", str_to_title(gsub("_", " ", facet_by)), "\n",
                           "Using ", valid_dates, " valid datetime values"),
          x = "Hour of Day",
          y = "Day of Week"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)))
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_time_pattern_heatmap: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste0("Could not create Time Pattern Heatmap:\n", e$message)) +
      theme_void()
  })
}

#' Add to visualize_iperf.R

#' Plot time series comparison of multiple tests
#'
#' @param tcp_data Combined dataframe of all TCP test data 
#' @param color_by Variable to use for coloring (default: "file")
#' @param max_series Maximum number of series to show (default: 10)
#' @param smooth Whether to add smoothed trend lines (default: TRUE)
#' @return A ggplot object
plot_time_series_comparison <- function(tcp_data, color_by = "file", max_series = 10, smooth = TRUE) {
  # Ensure we have proper time information
  if(!"test_datetime" %in% names(tcp_data) || all(is.na(tcp_data$test_datetime))) {
    stop("Test datetime information is needed for time series comparison")
  }
  
  # For each file, standardize the time to start at 0
  tcp_data <- tcp_data %>%
    group_by(file) %>%
    mutate(
      relative_time = interval_start - first(interval_start),
      actual_time = test_datetime + seconds(interval_start)
    ) %>%
    ungroup()
  
  # Get the variable to color by
  if(!color_by %in% names(tcp_data)) {
    warning("Color variable not found, using file name")
    color_by <- "file"
  }
  
  # Limit number of series to avoid overplotting
  unique_values <- unique(tcp_data[[color_by]])
  if(length(unique_values) > max_series) {
    # If we have too many, take the most recent ones based on datetime
    if(color_by == "file") {
      # For files, get the most recent ones
      newest_files <- tcp_data %>%
        group_by(file) %>%
        summarize(last_time = max(test_datetime), .groups = "drop") %>%
        arrange(desc(last_time)) %>%
        slice_head(n = max_series) %>%
        pull(file)
      
      tcp_data <- tcp_data %>%
        filter(file %in% newest_files)
    } else {
      # For other variables, just take the first max_series values
      keep_values <- unique_values[1:max_series]
      tcp_data <- tcp_data %>%
        filter(!!sym(color_by) %in% keep_values)
    }
    
    message("Limited to ", max_series, " series to avoid overplotting")
  }
  
  # Plot the time series - Two options: by relative time or actual time
  
  # Option 1: Using relative time (aligned to start at 0)
  p1 <- tcp_data %>%
    ggplot(aes(x = relative_time, y = bitrate_mbps, color = !!sym(color_by), group = interaction(file, !!sym(color_by)))) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Network Performance Comparison Over Test Duration",
      x = "Time from start (seconds)",
      y = "Throughput (Mbps)",
      color = str_to_title(gsub("_", " ", color_by))
    ) +
    theme_minimal()
  
  # Option 2: Using actual datetime
  p2 <- tcp_data %>%
    ggplot(aes(x = actual_time, y = bitrate_mbps, color = !!sym(color_by), group = interaction(file, !!sym(color_by)))) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Network Performance Over Time",
      x = "Date/Time",
      y = "Throughput (Mbps)",
      color = str_to_title(gsub("_", " ", color_by))
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add smoothing if requested
  if(smooth) {
    p1 <- p1 + geom_smooth(method = "loess", se = FALSE, span = 0.3, linewidth = 1.2)
    p2 <- p2 + geom_smooth(method = "loess", se = FALSE, span = 0.3, linewidth = 1.2)
  }
  
  # Combine the plots
  combined <- p1 / p2 + plot_annotation(
    title = "Network Performance Time Series Analysis",
    subtitle = paste("Comparing", length(unique(tcp_data[[color_by]])), "different", gsub("_", " ", color_by), "values"),
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )
  
  return(combined)
}

#' Plot network performance trends over time
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param time_unit Unit for time grouping ('day', 'week', 'month', etc.)
#' @param facet_by Optional variable to facet by (e.g., "user_name") 
#' @return A ggplot object
plot_performance_trends <- function(tcp_data, time_unit = "day", facet_by = NULL) {
  # Try to analyze trends with error handling
  tryCatch({
    # Check if test_datetime is available
    if(!"test_datetime" %in% names(tcp_data)) {
      # Create a helpful error message plot
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No test_datetime column found in data. Try refreshing the data or check parsing.") +
        theme_void())
    }
    
    # Check if all datetime values are NA
    if(all(is.na(tcp_data$test_datetime))) {
      # Try to construct datetime from individual components if available
      if(all(c("test_date", "test_time") %in% names(tcp_data))) {
        message("Converting test_date and test_time to test_datetime")
        tcp_data <- tcp_data %>%
          mutate(test_datetime = case_when(
            !is.na(test_date) & !is.na(test_time) ~ {
              # Parse date (assumed to be in YYYYMMDD format)
              year <- as.integer(substr(test_date, 1, 4))
              month <- as.integer(substr(test_date, 5, 6))
              day <- as.integer(substr(test_date, 7, 8))
              
              # Parse time (assumed to be in HHMMSS format)
              hour <- as.integer(substr(test_time, 1, 2))
              minute <- as.integer(substr(test_time, 3, 4))
              second <- if(nchar(test_time) >= 6) as.integer(substr(test_time, 5, 6)) else 0
              
              # Create datetime
              ymd_hms(paste(year, month, day, hour, minute, second, sep="-"))
            },
            TRUE ~ NA_POSIXct_
          ))
      } else {
        # Create a helpful error message plot
        return(ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "All test_datetime values are NA and could not reconstruct from test_date/test_time") +
          theme_void())
      }
    }
    
    # Debug info about test_datetime
    valid_dates <- sum(!is.na(tcp_data$test_datetime))
    total_rows <- nrow(tcp_data)
    message("test_datetime: ", valid_dates, "/", total_rows, " valid values (", 
            round(valid_dates/total_rows*100, 1), "%)")
    
    # If we have too few valid datetime values, warn the user
    if(valid_dates < 10) {
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste0("Only ", valid_dates, " valid datetime values available.\n",
                               "Need more data points for meaningful trend analysis.")) +
        theme_void())
    }
    
    # Get trends data using only rows with valid datetime
    valid_data <- tcp_data %>% filter(!is.na(test_datetime))
    trends_data <- analyze_time_trends(valid_data, time_unit)
    
    # Create base plot
    p <- trends_data %>%
      ggplot(aes(x = time_group, y = avg_throughput)) +
      geom_line(size = 1, color = "steelblue") +
      geom_point(aes(size = test_count), color = "steelblue") +
      geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                      ymax = avg_throughput + stability), 
                  alpha = 0.2, fill = "steelblue") +
      labs(
        title = paste("Network Performance Trends by", str_to_title(time_unit)),
        subtitle = paste0("Average throughput with standard deviation bands\n",
                         "Using ", valid_dates, " valid datetime values"),
        x = str_to_title(time_unit),
        y = "Throughput (Mbps)",
        size = "Tests"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested and the column exists
    if(!is.null(facet_by) && facet_by %in% names(valid_data)) {
      # We need to regenerate the trends data with the facet variable
      facet_trends <- valid_data %>%
        group_by(!!sym(facet_by)) %>%
        group_modify(~ analyze_time_trends(.x, time_unit)) %>%
        ungroup()
      
      # Create faceted plot
      p <- facet_trends %>%
        ggplot(aes(x = time_group, y = avg_throughput, color = !!sym(facet_by), fill = !!sym(facet_by))) +
        geom_line(size = 1) +
        geom_point(aes(size = test_count)) +
        geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                        ymax = avg_throughput + stability), 
                    alpha = 0.1) +
        labs(
          title = paste("Network Performance Trends by", str_to_title(time_unit)),
          subtitle = paste0("Faceted by ", str_to_title(gsub("_", " ", facet_by)), "\n",
                           "Using ", valid_dates, " valid datetime values"),
          x = str_to_title(time_unit),
          y = "Throughput (Mbps)",
          size = "Tests",
          color = str_to_title(gsub("_", " ", facet_by)),
          fill = str_to_title(gsub("_", " ", facet_by))
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)), scales = "free_y")
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_performance_trends: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste0("Could not create Performance Trends plot:\n", e$message)) +
      theme_void()
  })
}

#' Create a day-of-week by hour heatmap of performance
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param facet_by Optional variable to facet by (e.g., "user_name")
#' @return A ggplot object
plot_time_pattern_heatmap <- function(tcp_data, facet_by = NULL) {
  # Try to create the heatmap with error handling
  tryCatch({
    # Check if test_datetime is available
    if(!"test_datetime" %in% names(tcp_data)) {
      # Create a helpful error message plot
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No test_datetime column found in data. Try refreshing the data or check parsing.") +
        theme_void())
    }
    
    # Check if all datetime values are NA
    if(all(is.na(tcp_data$test_datetime))) {
      # Try to construct datetime from individual components if available
      if(all(c("test_date", "test_time") %in% names(tcp_data))) {
        message("Converting test_date and test_time to test_datetime")
        tcp_data <- tcp_data %>%
          mutate(test_datetime = case_when(
            !is.na(test_date) & !is.na(test_time) ~ {
              # Parse date (assumed to be in YYYYMMDD format)
              year <- as.integer(substr(test_date, 1, 4))
              month <- as.integer(substr(test_date, 5, 6))
              day <- as.integer(substr(test_date, 7, 8))
              
              # Parse time (assumed to be in HHMMSS format)
              hour <- as.integer(substr(test_time, 1, 2))
              minute <- as.integer(substr(test_time, 3, 4))
              second <- if(nchar(test_time) >= 6) as.integer(substr(test_time, 5, 6)) else 0
              
              # Create datetime
              ymd_hms(paste(year, month, day, hour, minute, second, sep="-"))
            },
            TRUE ~ NA_POSIXct_
          ))
      } else {
        # Create a helpful error message plot
        return(ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "All test_datetime values are NA and could not reconstruct from test_date/test_time") +
          theme_void())
      }
    }
    
    # Debug info about test_datetime
    valid_dates <- sum(!is.na(tcp_data$test_datetime))
    total_rows <- nrow(tcp_data)
    message("test_datetime: ", valid_dates, "/", total_rows, " valid values (", 
            round(valid_dates/total_rows*100, 1), "%)")
    
    # Continue only with valid datetime values
    valid_data <- tcp_data %>% filter(!is.na(test_datetime))
    
    # Check if we have enough data for a meaningful heatmap
    if(n_distinct(wday(valid_data$test_datetime)) < 2 && 
       n_distinct(hour(valid_data$test_datetime)) < 3) {
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste0("Not enough time diversity to create a heatmap.\n",
                               "Need data from different days and hours.\n",
                               "Current data spans ", 
                               n_distinct(wday(valid_data$test_datetime)), " days and ",
                               n_distinct(hour(valid_data$test_datetime)), " hours.")) +
        theme_void())
    }
    
    # Extract day of week and hour from datetime
    heatmap_data <- valid_data %>%
      mutate(
        day_of_week = wday(test_datetime, label = TRUE),
        hour_of_day = hour(test_datetime)
      ) %>%
      group_by(day_of_week, hour_of_day) %>%
      summarize(
        avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
        test_count = n_distinct(file),
        data_points = n(),
        .groups = "drop"
      )
    
    # Create heatmap
    p <- heatmap_data %>%
      ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma", name = "Mbps") +
      scale_x_continuous(breaks = seq(0, 23, 3),
                       labels = c("12am", "3am", "6am", "9am", "12pm", "3pm", "6pm", "9pm")) +
      labs(
        title = "Performance Pattern by Day and Hour",
        subtitle = paste0("Average throughput (Mbps)\n",
                         "Using ", valid_dates, " valid datetime values"),
        x = "Hour of Day",
        y = "Day of Week"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested and the column exists
    if(!is.null(facet_by) && facet_by %in% names(valid_data)) {
      facet_heatmap <- valid_data %>%
        mutate(
          day_of_week = wday(test_datetime, label = TRUE),
          hour_of_day = hour(test_datetime)
        ) %>%
        group_by(!!sym(facet_by), day_of_week, hour_of_day) %>%
        summarize(
          avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
          test_count = n_distinct(file),
          data_points = n(),
          .groups = "drop"
        )
      
      p <- facet_heatmap %>%
        ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
        geom_tile() +
        scale_fill_viridis_c(option = "plasma", name = "Mbps") +
        scale_x_continuous(breaks = seq(0, 23, 6),
                         labels = c("12am", "6am", "12pm", "6pm")) +
        labs(
          title = "Performance Pattern by Day and Hour",
          subtitle = paste0("Faceted by ", str_to_title(gsub("_", " ", facet_by)), "\n",
                           "Using ", valid_dates, " valid datetime values"),
          x = "Hour of Day",
          y = "Day of Week"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)))
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_time_pattern_heatmap: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste0("Could not create Time Pattern Heatmap:\n", e$message)) +
      theme_void()
  })
}

#' Add to visualize_iperf.R

#' Plot time series comparison of multiple tests
#'
#' @param tcp_data Combined dataframe of all TCP test data 
#' @param color_by Variable to use for coloring (default: "file")
#' @param max_series Maximum number of series to show (default: 10)
#' @param smooth Whether to add smoothed trend lines (default: TRUE)
#' @return A ggplot object
plot_time_series_comparison <- function(tcp_data, color_by = "file", max_series = 10, smooth = TRUE) {
  # Ensure we have proper time information
  if(!"test_datetime" %in% names(tcp_data) || all(is.na(tcp_data$test_datetime))) {
    stop("Test datetime information is needed for time series comparison")
  }
  
  # For each file, standardize the time to start at 0
  tcp_data <- tcp_data %>%
    group_by(file) %>%
    mutate(
      relative_time = interval_start - first(interval_start),
      actual_time = test_datetime + seconds(interval_start)
    ) %>%
    ungroup()
  
  # Get the variable to color by
  if(!color_by %in% names(tcp_data)) {
    warning("Color variable not found, using file name")
    color_by <- "file"
  }
  
  # Limit number of series to avoid overplotting
  unique_values <- unique(tcp_data[[color_by]])
  if(length(unique_values) > max_series) {
    # If we have too many, take the most recent ones based on datetime
    if(color_by == "file") {
      # For files, get the most recent ones
      newest_files <- tcp_data %>%
        group_by(file) %>%
        summarize(last_time = max(test_datetime), .groups = "drop") %>%
        arrange(desc(last_time)) %>%
        slice_head(n = max_series) %>%
        pull(file)
      
      tcp_data <- tcp_data %>%
        filter(file %in% newest_files)
    } else {
      # For other variables, just take the first max_series values
      keep_values <- unique_values[1:max_series]
      tcp_data <- tcp_data %>%
        filter(!!sym(color_by) %in% keep_values)
    }
    
    message("Limited to ", max_series, " series to avoid overplotting")
  }
  
  # Plot the time series - Two options: by relative time or actual time
  
  # Option 1: Using relative time (aligned to start at 0)
  p1 <- tcp_data %>%
    ggplot(aes(x = relative_time, y = bitrate_mbps, color = !!sym(color_by), group = interaction(file, !!sym(color_by)))) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Network Performance Comparison Over Test Duration",
      x = "Time from start (seconds)",
      y = "Throughput (Mbps)",
      color = str_to_title(gsub("_", " ", color_by))
    ) +
    theme_minimal()
  
  # Option 2: Using actual datetime
  p2 <- tcp_data %>%
    ggplot(aes(x = actual_time, y = bitrate_mbps, color = !!sym(color_by), group = interaction(file, !!sym(color_by)))) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Network Performance Over Time",
      x = "Date/Time",
      y = "Throughput (Mbps)",
      color = str_to_title(gsub("_", " ", color_by))
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add smoothing if requested
  if(smooth) {
    p1 <- p1 + geom_smooth(method = "loess", se = FALSE, span = 0.3, linewidth = 1.2)
    p2 <- p2 + geom_smooth(method = "loess", se = FALSE, span = 0.3, linewidth = 1.2)
  }
  
  # Combine the plots
  combined <- p1 / p2 + plot_annotation(
    title = "Network Performance Time Series Analysis",
    subtitle = paste("Comparing", length(unique(tcp_data[[color_by]])), "different", gsub("_", " ", color_by), "values"),
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )
  
  return(combined)
}

#' Plot network performance trends over time
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param time_unit Unit for time grouping ('day', 'week', 'month', etc.)
#' @param facet_by Optional variable to facet by (e.g., "user_name") 
#' @return A ggplot object
plot_performance_trends <- function(tcp_data, time_unit = "day", facet_by = NULL) {
  # Try to analyze trends with error handling
  tryCatch({
    # Check if test_datetime is available
    if(!"test_datetime" %in% names(tcp_data)) {
      # Create a helpful error message plot
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No test_datetime column found in data. Try refreshing the data or check parsing.") +
        theme_void())
    }
    
    # Check if all datetime values are NA
    if(all(is.na(tcp_data$test_datetime))) {
      # Try to construct datetime from individual components if available
      if(all(c("test_date", "test_time") %in% names(tcp_data))) {
        message("Converting test_date and test_time to test_datetime")
        tcp_data <- tcp_data %>%
          mutate(test_datetime = case_when(
            !is.na(test_date) & !is.na(test_time) ~ {
              # Parse date (assumed to be in YYYYMMDD format)
              year <- as.integer(substr(test_date, 1, 4))
              month <- as.integer(substr(test_date, 5, 6))
              day <- as.integer(substr(test_date, 7, 8))
              
              # Parse time (assumed to be in HHMMSS format)
              hour <- as.integer(substr(test_time, 1, 2))
              minute <- as.integer(substr(test_time, 3, 4))
              second <- if(nchar(test_time) >= 6) as.integer(substr(test_time, 5, 6)) else 0
              
              # Create datetime
              ymd_hms(paste(year, month, day, hour, minute, second, sep="-"))
            },
            TRUE ~ NA_POSIXct_
          ))
      } else {
        # Create a helpful error message plot
        return(ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "All test_datetime values are NA and could not reconstruct from test_date/test_time") +
          theme_void())
      }
    }
    
    # Debug info about test_datetime
    valid_dates <- sum(!is.na(tcp_data$test_datetime))
    total_rows <- nrow(tcp_data)
    message("test_datetime: ", valid_dates, "/", total_rows, " valid values (", 
            round(valid_dates/total_rows*100, 1), "%)")
    
    # If we have too few valid datetime values, warn the user
    if(valid_dates < 10) {
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste0("Only ", valid_dates, " valid datetime values available.\n",
                               "Need more data points for meaningful trend analysis.")) +
        theme_void())
    }
    
    # Get trends data using only rows with valid datetime
    valid_data <- tcp_data %>% filter(!is.na(test_datetime))
    trends_data <- analyze_time_trends(valid_data, time_unit)
    
    # Create base plot
    p <- trends_data %>%
      ggplot(aes(x = time_group, y = avg_throughput)) +
      geom_line(size = 1, color = "steelblue") +
      geom_point(aes(size = test_count), color = "steelblue") +
      geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                      ymax = avg_throughput + stability), 
                  alpha = 0.2, fill = "steelblue") +
      labs(
        title = paste("Network Performance Trends by", str_to_title(time_unit)),
        subtitle = paste0("Average throughput with standard deviation bands\n",
                         "Using ", valid_dates, " valid datetime values"),
        x = str_to_title(time_unit),
        y = "Throughput (Mbps)",
        size = "Tests"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested and the column exists
    if(!is.null(facet_by) && facet_by %in% names(valid_data)) {
      # We need to regenerate the trends data with the facet variable
      facet_trends <- valid_data %>%
        group_by(!!sym(facet_by)) %>%
        group_modify(~ analyze_time_trends(.x, time_unit)) %>%
        ungroup()
      
      # Create faceted plot
      p <- facet_trends %>%
        ggplot(aes(x = time_group, y = avg_throughput, color = !!sym(facet_by), fill = !!sym(facet_by))) +
        geom_line(size = 1) +
        geom_point(aes(size = test_count)) +
        geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                        ymax = avg_throughput + stability), 
                    alpha = 0.1) +
        labs(
          title = paste("Network Performance Trends by", str_to_title(time_unit)),
          subtitle = paste0("Faceted by ", str_to_title(gsub("_", " ", facet_by)), "\n",
                           "Using ", valid_dates, " valid datetime values"),
          x = str_to_title(time_unit),
          y = "Throughput (Mbps)",
          size = "Tests",
          color = str_to_title(gsub("_", " ", facet_by)),
          fill = str_to_title(gsub("_", " ", facet_by))
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)), scales = "free_y")
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_performance_trends: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste0("Could not create Performance Trends plot:\n", e$message)) +
      theme_void()
  })
}

#' Create a day-of-week by hour heatmap of performance
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param facet_by Optional variable to facet by (e.g., "user_name")
#' @return A ggplot object
plot_time_pattern_heatmap <- function(tcp_data, facet_by = NULL) {
  # Try to create the heatmap with error handling
  tryCatch({
    # Check if test_datetime is available
    if(!"test_datetime" %in% names(tcp_data)) {
      # Create a helpful error message plot
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No test_datetime column found in data. Try refreshing the data or check parsing.") +
        theme_void())
    }
    
    # Check if all datetime values are NA
    if(all(is.na(tcp_data$test_datetime))) {
      # Try to construct datetime from individual components if available
      if(all(c("test_date", "test_time") %in% names(tcp_data))) {
        message("Converting test_date and test_time to test_datetime")
        tcp_data <- tcp_data %>%
          mutate(test_datetime = case_when(
            !is.na(test_date) & !is.na(test_time) ~ {
              # Parse date (assumed to be in YYYYMMDD format)
              year <- as.integer(substr(test_date, 1, 4))
              month <- as.integer(substr(test_date, 5, 6))
              day <- as.integer(substr(test_date, 7, 8))
              
              # Parse time (assumed to be in HHMMSS format)
              hour <- as.integer(substr(test_time, 1, 2))
              minute <- as.integer(substr(test_time, 3, 4))
              second <- if(nchar(test_time) >= 6) as.integer(substr(test_time, 5, 6)) else 0
              
              # Create datetime
              ymd_hms(paste(year, month, day, hour, minute, second, sep="-"))
            },
            TRUE ~ NA_POSIXct_
          ))
      } else {
        # Create a helpful error message plot
        return(ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "All test_datetime values are NA and could not reconstruct from test_date/test_time") +
          theme_void())
      }
    }
    
    # Debug info about test_datetime
    valid_dates <- sum(!is.na(tcp_data$test_datetime))
    total_rows <- nrow(tcp_data)
    message("test_datetime: ", valid_dates, "/", total_rows, " valid values (", 
            round(valid_dates/total_rows*100, 1), "%)")
    
    # Continue only with valid datetime values
    valid_data <- tcp_data %>% filter(!is.na(test_datetime))
    
    # Check if we have enough data for a meaningful heatmap
    if(n_distinct(wday(valid_data$test_datetime)) < 2 && 
       n_distinct(hour(valid_data$test_datetime)) < 3) {
      return(ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste0("Not enough time diversity to create a heatmap.\n",
                               "Need data from different days and hours.\n",
                               "Current data spans ", 
                               n_distinct(wday(valid_data$test_datetime)), " days and ",
                               n_distinct(hour(valid_data$test_datetime)), " hours.")) +
        theme_void())
    }
    
    # Extract day of week and hour from datetime
    heatmap_data <- valid_data %>%
      mutate(
        day_of_week = wday(test_datetime, label = TRUE),
        hour_of_day = hour(test_datetime)
      ) %>%
      group_by(day_of_week, hour_of_day) %>%
      summarize(
        avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
        test_count = n_distinct(file),
        data_points = n(),
        .groups = "drop"
      )
    
    # Create heatmap
    p <- heatmap_data %>%
      ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma", name = "Mbps") +
      scale_x_continuous(breaks = seq(0, 23, 3),
                       labels = c("12am", "3am", "6am", "9am", "12pm", "3pm", "6pm", "9pm")) +
      labs(
        title = "Performance Pattern by Day and Hour",
        subtitle = paste0("Average throughput (Mbps)\n",
                         "Using ", valid_dates, " valid datetime values"),
        x = "Hour of Day",
        y = "Day of Week"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested and the column exists
    if(!is.null(facet_by) && facet_by %in% names(valid_data)) {
      facet_heatmap <- valid_data %>%
        mutate(
          day_of_week = wday(test_datetime, label = TRUE),
          hour_of_day = hour(test_datetime)
        ) %>%
        group_by(!!sym(facet_by), day_of_week, hour_of_day) %>%
        summarize(
          avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
          test_count = n_distinct(file),
          data_points = n(),
          .groups = "drop"
        )
      
      p <- facet_heatmap %>%
        ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
        geom_tile() +
        scale_fill_viridis_c(option = "plasma", name = "Mbps") +
        scale_x_continuous(breaks = seq(0, 23, 6),
                         labels = c("12am", "6am", "12pm", "6pm")) +
        labs(
          title = "Performance Pattern by Day and Hour",
          subtitle = paste0("Faceted by ", str_to_title(gsub("_", " ", facet_by)), "\n",
                           "Using ", valid_dates, " valid datetime values"),
          x = "Hour of Day",
          y = "Day of Week"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)))
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_time_pattern_heatmap: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste0("Could not create Time Pattern Heatmap:\n", e$message)) +
      theme_void()
  })
}

#' Group data by date for analysis
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @return A dataframe grouped by date
group_by_date <- function(tcp_data) {
  # Check if we have test_datetime
  if(!"test_datetime" %in% names(tcp_data)) {
    stop("test_datetime column required for date grouping")
  }
  
  # Create date-only field for grouping
  tcp_data <- tcp_data %>%
    mutate(date_only = as.Date(test_datetime))
  
  # Group by date and calculate statistics
  daily_data <- tcp_data %>%
    group_by(test_date = date_only) %>%
    summarize(
      avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
      median_throughput = median(bitrate_mbps, na.rm = TRUE),
      min_throughput = min(bitrate_mbps, na.rm = TRUE),
      max_throughput = max(bitrate_mbps, na.rm = TRUE),
      std_dev = sd(bitrate_mbps, na.rm = TRUE),
      test_count = n_distinct(file),
      data_points = n(),
      .groups = "drop"
    ) %>%
    arrange(test_date)
  
  return(daily_data)
}

#' Group data by time of day
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @return A dataframe grouped by time periods
group_by_time_of_day <- function(tcp_data) {
  # Check if we have test_datetime
  if(!"test_datetime" %in% names(tcp_data)) {
    stop("test_datetime column required for time of day analysis")
  }
  
  # Extract hour and create time period
  tcp_data <- tcp_data %>%
    mutate(
      hour = hour(test_datetime),
      time_period = case_when(
        hour >= 6 & hour < 12 ~ "Morning (6AM-12PM)",
        hour >= 12 & hour < 18 ~ "Afternoon (12PM-6PM)",
        hour >= 18 & hour < 24 ~ "Evening (6PM-12AM)",
        TRUE ~ "Night (12AM-6AM)"
      )
    )
  
  # Group by time period
  tod_summary <- tcp_data %>%
    group_by(time_period) %>%
    summarize(
      avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
      median_throughput = median(bitrate_mbps, na.rm = TRUE),
      min_throughput = min(bitrate_mbps, na.rm = TRUE),
      max_throughput = max(bitrate_mbps, na.rm = TRUE),
      stability = sd(bitrate_mbps, na.rm = TRUE),
      test_count = n_distinct(file),
      data_points = n(),
      .groups = "drop"
    ) %>%
    # Sort by time of day
    mutate(time_period = factor(time_period, 
                               levels = c("Morning (6AM-12PM)", 
                                          "Afternoon (12PM-6PM)",
                                          "Evening (6PM-12AM)", 
                                          "Night (12AM-6AM)"))) %>%
    arrange(time_period)
  
  return(tod_summary)
}

#' Analyze trends in performance over time
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param time_unit Unit for time grouping ('day', 'week', 'month', etc.)
#' @return A dataframe with time trends analysis
analyze_time_trends <- function(tcp_data, time_unit = "day") {
  # Check if we have test_datetime
  if(!"test_datetime" %in% names(tcp_data) || all(is.na(tcp_data$test_datetime))) {
    stop("Valid test_datetime information is required for time trend analysis")
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
      user_count = if("user_name" %in% names(tcp_data)) n_distinct(user_name) else 1,
      avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
      median_throughput = median(bitrate_mbps, na.rm = TRUE),
      stability = sd(bitrate_mbps, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(time_group)
  
  # Calculate trend indicators if we have multiple data points
  if(nrow(time_trends) > 1) {
    time_trends <- time_trends %>%
      mutate(
        throughput_change = c(NA, diff(avg_throughput)),
        throughput_change_pct = ifelse(is.na(lag(avg_throughput)) | lag(avg_throughput) == 0, 
                                     NA, 
                                     throughput_change / lag(avg_throughput) * 100),
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

#' Modified plot_time_of_day with better error handling
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @return A ggplot object
plot_time_of_day <- function(tcp_data) {
  # Try to create time of day data, with error handling
  tryCatch({
    tod_data <- group_by_time_of_day(tcp_data)
    
    ggplot(tod_data, aes(x = time_period, y = avg_throughput)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_errorbar(aes(ymin = pmax(0, avg_throughput - stability), 
                        ymax = avg_throughput + stability),
                  width = 0.2) +
      labs(
        title = "Network Performance by Time of Day",
        subtitle = "Average throughput with standard deviation",
        x = "Time of Day",
        y = "Throughput (Mbps)",
        caption = paste0("Based on ", length(unique(tcp_data$file)), " tests")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  }, error = function(e) {
    message("Error in plot_time_of_day: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Could not create Time of Day plot:", e$message)) +
      theme_void()
  })
}

#' Modified plot_performance_trends with better error handling
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param time_unit Unit for time grouping ('day', 'week', 'month', etc.)
#' @param facet_by Optional variable to facet by (e.g., "user_name") 
#' @return A ggplot object
plot_performance_trends <- function(tcp_data, time_unit = "day", facet_by = NULL) {
  # Try to analyze trends with error handling
  tryCatch({
    # Get trends data
    trends_data <- analyze_time_trends(tcp_data, time_unit)
    
    # Create base plot
    p <- trends_data %>%
      ggplot(aes(x = time_group, y = avg_throughput)) +
      geom_line(size = 1, color = "steelblue") +
      geom_point(aes(size = test_count), color = "steelblue") +
      geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                      ymax = avg_throughput + stability), 
                  alpha = 0.2, fill = "steelblue") +
      labs(
        title = paste("Network Performance Trends by", str_to_title(time_unit)),
        subtitle = "Average throughput with standard deviation bands",
        x = str_to_title(time_unit),
        y = "Throughput (Mbps)",
        size = "Tests"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested and the column exists
    if(!is.null(facet_by) && facet_by %in% names(tcp_data)) {
      # We need to regenerate the trends data with the facet variable
      facet_trends <- tcp_data %>%
        group_by(!!sym(facet_by)) %>%
        group_modify(~ analyze_time_trends(.x, time_unit)) %>%
        ungroup()
      
      # Create faceted plot
      p <- facet_trends %>%
        ggplot(aes(x = time_group, y = avg_throughput, color = !!sym(facet_by), fill = !!sym(facet_by))) +
        geom_line(size = 1) +
        geom_point(aes(size = test_count)) +
        geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                        ymax = avg_throughput + stability), 
                    alpha = 0.1) +
        labs(
          title = paste("Network Performance Trends by", str_to_title(time_unit)),
          subtitle = paste("Faceted by", str_to_title(gsub("_", " ", facet_by))),
          x = str_to_title(time_unit),
          y = "Throughput (Mbps)",
          size = "Tests",
          color = str_to_title(gsub("_", " ", facet_by)),
          fill = str_to_title(gsub("_", " ", facet_by))
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)), scales = "free_y")
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_performance_trends: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Could not create Performance Trends plot:", e$message)) +
      theme_void()
  })
}

#' Modified plot_time_pattern_heatmap with better error handling
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param facet_by Optional variable to facet by (e.g., "user_name")
#' @return A ggplot object
plot_time_pattern_heatmap <- function(tcp_data, facet_by = NULL) {
  # Try to create the heatmap with error handling
  tryCatch({
    # Check if we have datetime information
    if(!"test_datetime" %in% names(tcp_data) || all(is.na(tcp_data$test_datetime))) {
      stop("Valid test_datetime information is required for time pattern heatmap")
    }
    
    # Extract day of week and hour from datetime
    heatmap_data <- tcp_data %>%
      mutate(
        day_of_week = wday(test_datetime, label = TRUE),
        hour_of_day = hour(test_datetime)
      ) %>%
      group_by(day_of_week, hour_of_day) %>%
      summarize(
        avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
        test_count = n_distinct(file),
        data_points = n(),
        .groups = "drop"
      )
    
    # Create heatmap
    p <- heatmap_data %>%
      ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma", name = "Mbps") +
      scale_x_continuous(breaks = seq(0, 23, 3),
                       labels = c("12am", "3am", "6am", "9am", "12pm", "3pm", "6pm", "9pm")) +
      labs(
        title = "Performance Pattern by Day and Hour",
        subtitle = "Average throughput (Mbps)",
        x = "Hour of Day",
        y = "Day of Week"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested
    if(!is.null(facet_by) && facet_by %in% names(tcp_data)) {
      facet_heatmap <- tcp_data %>%
        mutate(
          day_of_week = wday(test_datetime, label = TRUE),
          hour_of_day = hour(test_datetime)
        ) %>%
        group_by(!!sym(facet_by), day_of_week, hour_of_day) %>%
        summarize(
          avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
          test_count = n_distinct(file),
          data_points = n(),
          .groups = "drop"
        )
      
      p <- facet_heatmap %>%
        ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
        geom_tile() +
        scale_fill_viridis_c(option = "plasma", name = "Mbps") +
        scale_x_continuous(breaks = seq(0, 23, 6),
                         labels = c("12am", "6am", "12pm", "6pm")) +
        labs(
          title = "Performance Pattern by Day and Hour",
          subtitle = paste("Faceted by", str_to_title(gsub("_", " ", facet_by))),
          x = "Hour of Day",
          y = "Day of Week"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)))
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_time_pattern_heatmap: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Could not create Time Pattern Heatmap:", e$message)) +
      theme_void()
  })
}

#' Add to visualize_iperf.R

#' Plot time series comparison of multiple tests
#'
#' @param tcp_data Combined dataframe of all TCP test data 
#' @param color_by Variable to use for coloring (default: "file")
#' @param max_series Maximum number of series to show (default: 10)
#' @param smooth Whether to add smoothed trend lines (default: TRUE)
#' @return A ggplot object
plot_time_series_comparison <- function(tcp_data, color_by = "file", max_series = 10, smooth = TRUE) {
  # Ensure we have proper time information
  if(!"test_datetime" %in% names(tcp_data) || all(is.na(tcp_data$test_datetime))) {
    stop("Test datetime information is needed for time series comparison")
  }
  
  # For each file, standardize the time to start at 0
  tcp_data <- tcp_data %>%
    group_by(file) %>%
    mutate(
      relative_time = interval_start - first(interval_start),
      actual_time = test_datetime + seconds(interval_start)
    ) %>%
    ungroup()
  
  # Get the variable to color by
  if(!color_by %in% names(tcp_data)) {
    warning("Color variable not found, using file name")
    color_by <- "file"
  }
  
  # Limit number of series to avoid overplotting
  unique_values <- unique(tcp_data[[color_by]])
  if(length(unique_values) > max_series) {
    # If we have too many, take the most recent ones based on datetime
    if(color_by == "file") {
      # For files, get the most recent ones
      newest_files <- tcp_data %>%
        group_by(file) %>%
        summarize(last_time = max(test_datetime), .groups = "drop") %>%
        arrange(desc(last_time)) %>%
        slice_head(n = max_series) %>%
        pull(file)
      
      tcp_data <- tcp_data %>%
        filter(file %in% newest_files)
    } else {
      # For other variables, just take the first max_series values
      keep_values <- unique_values[1:max_series]
      tcp_data <- tcp_data %>%
        filter(!!sym(color_by) %in% keep_values)
    }
    
    message("Limited to ", max_series, " series to avoid overplotting")
  }
  
  # Plot the time series - Two options: by relative time or actual time
  
  # Option 1: Using relative time (aligned to start at 0)
  p1 <- tcp_data %>%
    ggplot(aes(x = relative_time, y = bitrate_mbps, color = !!sym(color_by), group = interaction(file, !!sym(color_by)))) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Network Performance Comparison Over Test Duration",
      x = "Time from start (seconds)",
      y = "Throughput (Mbps)",
      color = str_to_title(gsub("_", " ", color_by))
    ) +
    theme_minimal()
  
  # Option 2: Using actual datetime
  p2 <- tcp_data %>%
    ggplot(aes(x = actual_time, y = bitrate_mbps, color = !!sym(color_by), group = interaction(file, !!sym(color_by)))) +
    geom_line(alpha = 0.7) +
    labs(
      title = "Network Performance Over Time",
      x = "Date/Time",
      y = "Throughput (Mbps)",
      color = str_to_title(gsub("_", " ", color_by))
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add smoothing if requested
  if(smooth) {
    p1 <- p1 + geom_smooth(method = "loess", se = FALSE, span = 0.3, linewidth = 1.2)
    p2 <- p2 + geom_smooth(method = "loess", se = FALSE, span = 0.3, linewidth = 1.2)
  }
  
  # Combine the plots
  combined <- p1 / p2 + plot_annotation(
    title = "Network Performance Time Series Analysis",
    subtitle = paste("Comparing", length(unique(tcp_data[[color_by]])), "different", gsub("_", " ", color_by), "values"),
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )
  
  return(combined)
}

#' Plot network performance trends over time
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param time_unit Unit for time grouping ('day', 'week', 'month', etc.)
#' @param facet_by Optional variable to facet by (e.g., "user_name") 
#' @return A ggplot object
plot_performance_trends <- function(tcp_data, time_unit = "day", facet_by = NULL) {
  # Try to analyze trends with error handling
  tryCatch({
    # Get trends data
    trends_data <- analyze_time_trends(tcp_data, time_unit)
    
    # Create base plot
    p <- trends_data %>%
      ggplot(aes(x = time_group, y = avg_throughput)) +
      geom_line(size = 1, color = "steelblue") +
      geom_point(aes(size = test_count), color = "steelblue") +
      geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                      ymax = avg_throughput + stability), 
                  alpha = 0.2, fill = "steelblue") +
      labs(
        title = paste("Network Performance Trends by", str_to_title(time_unit)),
        subtitle = "Average throughput with standard deviation bands",
        x = str_to_title(time_unit),
        y = "Throughput (Mbps)",
        size = "Tests"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested and the column exists
    if(!is.null(facet_by) && facet_by %in% names(tcp_data)) {
      # We need to regenerate the trends data with the facet variable
      facet_trends <- tcp_data %>%
        group_by(!!sym(facet_by)) %>%
        group_modify(~ analyze_time_trends(.x, time_unit)) %>%
        ungroup()
      
      # Create faceted plot
      p <- facet_trends %>%
        ggplot(aes(x = time_group, y = avg_throughput, color = !!sym(facet_by), fill = !!sym(facet_by))) +
        geom_line(size = 1) +
        geom_point(aes(size = test_count)) +
        geom_ribbon(aes(ymin = pmax(0, avg_throughput - stability), 
                        ymax = avg_throughput + stability), 
                    alpha = 0.1) +
        labs(
          title = paste("Network Performance Trends by", str_to_title(time_unit)),
          subtitle = paste("Faceted by", str_to_title(gsub("_", " ", facet_by))),
          x = str_to_title(time_unit),
          y = "Throughput (Mbps)",
          size = "Tests",
          color = str_to_title(gsub("_", " ", facet_by)),
          fill = str_to_title(gsub("_", " ", facet_by))
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)), scales = "free_y")
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_performance_trends: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Could not create Performance Trends plot:", e$message)) +
      theme_void()
  })
}

#' Create a day-of-week by hour heatmap of performance
#'
#' @param tcp_data Combined dataframe of all TCP test data
#' @param facet_by Optional variable to facet by (e.g., "user_name")
#' @return A ggplot object
plot_time_pattern_heatmap <- function(tcp_data, facet_by = NULL) {
  # Try to create the heatmap with error handling
  tryCatch({
    # Check if we have datetime information
    if(!"test_datetime" %in% names(tcp_data) || all(is.na(tcp_data$test_datetime))) {
      stop("Valid test_datetime information is required for time pattern heatmap")
    }
    
    # Extract day of week and hour from datetime
    heatmap_data <- tcp_data %>%
      mutate(
        day_of_week = wday(test_datetime, label = TRUE),
        hour_of_day = hour(test_datetime)
      ) %>%
      group_by(day_of_week, hour_of_day) %>%
      summarize(
        avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
        test_count = n_distinct(file),
        data_points = n(),
        .groups = "drop"
      )
    
    # Create heatmap
    p <- heatmap_data %>%
      ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma", name = "Mbps") +
      scale_x_continuous(breaks = seq(0, 23, 3),
                       labels = c("12am", "3am", "6am", "9am", "12pm", "3pm", "6pm", "9pm")) +
      labs(
        title = "Performance Pattern by Day and Hour",
        subtitle = "Average throughput (Mbps)",
        x = "Hour of Day",
        y = "Day of Week"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")
      )
    
    # Add faceting if requested
    if(!is.null(facet_by) && facet_by %in% names(tcp_data)) {
      facet_heatmap <- tcp_data %>%
        mutate(
          day_of_week = wday(test_datetime, label = TRUE),
          hour_of_day = hour(test_datetime)
        ) %>%
        group_by(!!sym(facet_by), day_of_week, hour_of_day) %>%
        summarize(
          avg_throughput = mean(bitrate_mbps, na.rm = TRUE),
          test_count = n_distinct(file),
          data_points = n(),
          .groups = "drop"
        )
      
      p <- facet_heatmap %>%
        ggplot(aes(x = hour_of_day, y = day_of_week, fill = avg_throughput)) +
        geom_tile() +
        scale_fill_viridis_c(option = "plasma", name = "Mbps") +
        scale_x_continuous(breaks = seq(0, 23, 6),
                         labels = c("12am", "6am", "12pm", "6pm")) +
        labs(
          title = "Performance Pattern by Day and Hour",
          subtitle = paste("Faceted by", str_to_title(gsub("_", " ", facet_by))),
          x = "Hour of Day",
          y = "Day of Week"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold")
        ) +
        facet_wrap(vars(!!sym(facet_by)))
    }
    
    return(p)
  }, error = function(e) {
    message("Error in plot_time_pattern_heatmap: ", e$message)
    # Return a blank plot with error message
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("Could not create Time Pattern Heatmap:", e$message)) +
      theme_void()
  })
}