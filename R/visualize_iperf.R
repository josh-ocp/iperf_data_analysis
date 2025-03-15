library(tidyverse)
library(patchwork)  # For combining plots
library(scales)     # For nice formatting

#' Plot throughput over time for a single test
#'
#' @param data Processed iperf data
#' @param title Plot title
#' @return ggplot object
plot_throughput <- function(data, title = "Network Throughput") {
  # Sort data by time
  data <- data %>% arrange(interval_start)
  
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

# Add a new function for comparing users

#' Compare performance across different users
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
    group_by(user_label, protocol) %>%
    summarize(
      avg_bitrate = mean(bitrate_mbps),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = user_label, y = avg_bitrate, fill = protocol)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Average Throughput by User",
      x = "User (ISP)",
      y = "Average Throughput (Mbps)",
      fill = "Protocol"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Boxplot of throughput distribution by user
  p2 <- data %>%
    ggplot(aes(x = user_label, y = bitrate_mbps, fill = protocol)) +
    geom_boxplot() +
    labs(
      title = "Throughput Distribution by User",
      x = "User (ISP)",
      y = "Throughput (Mbps)",
      fill = "Protocol"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # For UDP tests, add packet loss comparison
  if("lost_percent" %in% names(data)) {
    p3 <- data %>%
      filter(protocol == "UDP") %>%
      group_by(user_label) %>%
      summarize(
        avg_loss = mean(lost_percent),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = user_label, y = avg_loss, fill = user_label)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Average Packet Loss by User (UDP)",
        x = "User (ISP)",
        y = "Packet Loss (%)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Combine all plots
    combined <- (p1 / p2 / p3) + 
      plot_annotation(title = "User Comparison", 
                      theme = theme(plot.title = element_text(face = "bold")))
  } else {
    # Combine just the throughput plots
    combined <- (p1 / p2) + 
      plot_annotation(title = "User Comparison", 
                      theme = theme(plot.title = element_text(face = "bold")))
  }
  
  return(combined)
}