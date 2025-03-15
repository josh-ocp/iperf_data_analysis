library(tidyverse)
library(patchwork)  # For combining plots

#' Plot throughput over time for a single test
#'
#' @param data Processed iperf data
#' @param title Plot title
#' @return ggplot object
plot_throughput <- function(data, title = "Network Throughput") {
  p <- data %>%
    ggplot(aes(x = interval_start, y = bitrate_mbps)) +
    geom_line() +
    geom_point(alpha = 0.5) +
    labs(
      title = title,
      x = "Time (seconds)",
      y = "Throughput (Mbps)"
    ) +
    theme_minimal()
  
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
  
  # Create throughput plot
  p1 <- plot_throughput(data, "UDP Throughput")
  
  # Create jitter plot
  p2 <- data %>%
    ggplot(aes(x = interval_start, y = jitter_ms)) +
    geom_line(color = "blue") +
    geom_point(alpha = 0.5, color = "blue") +
    labs(
      title = "Jitter",
      x = "Time (seconds)",
      y = "Jitter (ms)"
    ) +
    theme_minimal()
  
  # Create packet loss plot
  p3 <- data %>%
    ggplot(aes(x = interval_start, y = lost_percent)) +
    geom_line(color = "red") +
    geom_point(alpha = 0.5, color = "red") +
    labs(
      title = "Packet Loss",
      x = "Time (seconds)",
      y = "Loss (%)"
    ) +
    theme_minimal()
  
  # Combine plots
  combined_plot <- p1 / p2 / p3
  
  return(combined_plot)
}