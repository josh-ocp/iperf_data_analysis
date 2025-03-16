# iPerf Data Analysis

This project provides tools to collect, analyze, and visualize data from iPerf3 network performance tests.

## Project Structure

- `config.R`: Configuration settings
- `data/`: Raw and processed iPerf data
- `R/`: Functions for parsing, analyzing, and visualizing iPerf data
- `scripts/`: Main processing scripts
- `output/`: Generated results and visualizations
- `data_collection/`: Scripts for standardized data collection

## Workflow

### 1. Collect Data

Use the provided data collection script:

```bash
cd data_collection
chmod +x run_iperf_test.sh
./run_iperf_test.sh
```

Options:
- `-s SERVER`: Server to test (can specify multiple times)
- `-d SECONDS`: Test duration in seconds (default: 120)
- `-u USER`: Username to record (default: current user)
- `-j`: Use JSON output format (recommended for more reliable parsing)

#### Best Practices for Data Collection

**Test Setup:**
- Use geographically diverse servers for comprehensive testing
- Ensure test duration is at least 60 seconds (120s recommended)
- Minimize other network activity during tests
- Run tests at different times of day for better data variety

**Recommended Command Options:**
```bash
# Basic test
./run_iperf_test.sh -s speedtest.serverius.net -d 120

# Advanced test with JSON output
./run_iperf_test.sh -s speedtest.serverius.net -d 300 -j

# Multiple servers
./run_iperf_test.sh -s speedtest.serverius.net -s speedtest.london.linode.com -d 120 -j
```

See `data_collection/BEST_PRACTICES.md` for more detailed guidance.

### 2. Analyze Data

Copy the generated files to your input directory (set in `config.R`), then run:

```r
source("scripts/process_all_data.R")
```

This will:
- Parse raw iPerf3 output files
- Clean and process the data
- Generate performance summaries
- Create visualizations

For customized analysis, you can also use individual scripts:
```r
source("scripts/parse_raw_data.R")   # Convert raw data to processed format
source("scripts/generate_reports.R")  # Create summary reports
source("scripts/create_plots.R")      # Generate visualizations
```

### 3. View Results

Results are saved to:
- Summary reports: `output/reports/`
- Visualizations: `output/figures/`
- Processed data: `data/processed/`

## Supported Formats

The analysis can process:
- Raw iPerf3 text output
- iPerf3 JSON output (recommended)
- Pre-processed CSV files

## Requirements

R packages:
- tidyverse
- jsonlite
- fs
- patchwork
- scales
- zoo
- lubridate

## Common Issues

1. **JSON parse errors**: Some iPerf3 versions may produce malformed JSON. Use raw output as fallback.
2. **Connectivity failures**: The script attempts to ping servers first to avoid hanging.
3. **Disk space**: Long-term testing may accumulate large data files. Monitor available space.
