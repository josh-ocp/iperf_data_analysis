library(tidyverse)
library(here)
library(jsonlite)
library(fs)  # For file operations

# Load configuration and utility functions
source(here("config.R"))
source(here("R", "parse_iperf.R"))
source(here("R", "analyze_iperf.R"))
source(here("R", "visualize_iperf.R"))

# Create directories if they don't exist
dir.create(here("data", "raw", "tcp"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "raw", "udp"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "reports"), recursive = TRUE, showWarnings = FALSE)

# Function to determine if an iperf file is TCP or UDP
identify_protocol <- function(file_path) {
  tryCatch({
    data <- fromJSON(file_path)
    if (data$start$test_start$protocol == "UDP") {
      return("UDP")
    } else {
      return("TCP")
    }
  }, error = function(e) {
    warning(paste("Could not parse file:", file_path, "-", e$message))
    return(NA)
  })
}

# Process files from the external input directory
process_input_files <- function() {
  cat("Looking for iperf files in:", IPERF_INPUT_DIR, "\n")
  
  # Check if input directory exists
  if (!dir.exists(IPERF_INPUT_DIR)) {
    stop("Input directory does not exist. Please create it or update the config.R file.")
  }
  
  # Get all JSON files in the input directory
  input_files <- list.files(IPERF_INPUT_DIR, pattern = "\\.json$", full.names = TRUE)
  
  if (length(input_files) == 0) {
    cat("No JSON files found in the input directory.\n")
    return(list(tcp = character(0), udp = character(0)))
  }
  
  cat("Found", length(input_files), "JSON files.\n")
  
  # Identify protocol for each file
  protocols <- sapply(input_files, identify_protocol)
  
  # Filter files by protocol
  tcp_files <- input_files[protocols == "TCP"]
  udp_files <- input_files[protocols == "UDP"]
  
  cat("- TCP files:", length(tcp_files), "\n")
  cat("- UDP files:", length(udp_files), "\n")
  
  # Copy files to project structure if configured to do so
  if (COPY_FILES_TO_PROJECT) {
    cat("Copying files to project structure...\n")
    
    # Copy TCP files
    for (file in tcp_files) {
      dest <- here("data", "raw", "tcp", basename(file))
      file.copy(file, dest, overwrite = TRUE)
    }
    
    # Copy UDP files
    for (file in udp_files) {
      dest <- here("data", "raw", "udp", basename(file))
      file.copy(file, dest, overwrite = TRUE)
    }
    
    # Use the local copies for processing
    tcp_files <- list.files(here("data", "raw", "tcp"), pattern = "\\.json$", full.names = TRUE)
    udp_files <- list.files(here("data", "raw", "udp"), pattern = "\\.json$", full.names = TRUE)
  }
  
  return(list(tcp = tcp_files, udp = udp_files))
}

# Process the input files
files <- process_input_files()
tcp_files <- files$tcp
udp_files <- files$udp

# Process TCP data (if any)
if (length(tcp_files) > 0) {
  cat("Processing TCP data...\n")
  tcp_data <- process_iperf_files(tcp_files)
  saveRDS(tcp_data, here("data", "processed", "tcp_data.rds"))
  
  # Generate summary statistics
  tcp_summary <- summarize_iperf_data(tcp_data)
  write_csv(tcp_summary, here("output", "reports", "tcp_summary.csv"))
  
  # Generate visualizations
  for (file in unique(tcp_data$file)) {
    single_test <- tcp_data %>% filter(file == !!file)
    p <- plot_throughput(single_test, paste("TCP Throughput:", file))
    ggsave(here("output", "figures", paste0("tcp_", gsub("\\.json$", "", file), ".png")), 
           p, width = 8, height = 5)
  }
  
  # Create comparison plot if we have multiple tests
  if (length(unique(tcp_data$file)) > 1) {
    tcp_comparison <- tcp_data %>%
      ggplot(aes(x = interval_start, y = bitrate_mbps, color = file)) +
      geom_line() +
      labs(title = "TCP Throughput Comparison", x = "Time (seconds)", y = "Throughput (Mbps)") +
      theme_minimal()
    
    ggsave(here("output", "figures", "tcp_comparison.png"), tcp_comparison, width = 10, height = 6)
  }
} else {
  cat("No TCP files to process.\n")
}

# Process UDP data (if any)
if (length(udp_files) > 0) {
  cat("Processing UDP data...\n")
  udp_data <- process_iperf_files(udp_files)
  saveRDS(udp_data, here("data", "processed", "udp_data.rds"))
  
  # Generate summary statistics
  udp_summary <- summarize_iperf_data(udp_data)
  write_csv(udp_summary, here("output", "reports", "udp_summary.csv"))
  
  # Generate visualizations
  for (file in unique(udp_data$file)) {
    single_test <- udp_data %>% filter(file == !!file)
    p <- plot_udp_metrics(single_test)
    ggsave(here("output", "figures", paste0("udp_", gsub("\\.json$", "", file), ".png")), 
           p, width = 8, height = 12)
  }
} else {
  cat("No UDP files to process.\n")
}

cat("Analysis complete! Results saved to the output directory.\n")