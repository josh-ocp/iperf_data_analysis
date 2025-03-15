# iperf_data_analysis
Analyzing iperf network performance test data using R

## Overview
This project provides tools to analyze and visualize data from iperf3 network performance tests, supporting both TCP and UDP protocols.

## Setup

1. Clone this repository
2. Open the project in RStudio by double-clicking the `.Rproj` file
3. Edit the `config.R` file to set the input directory where you'll place your iperf JSON files
4. Install required packages:
   ```r
   install.packages(c("tidyverse", "jsonlite", "here", "patchwork", "fs"))
   ```

## Usage

### Input Data
Place your iperf3 JSON output files in the directory specified in `config.R`. The script will automatically:
- Detect whether each file contains TCP or UDP test data
- Copy the files to the project's data structure (optional, controlled by config setting)
- Process and analyze the data

### Running the Analysis
Run the main processing script from RStudio or the R console:

```r
source("scripts/process_all_data.R")
```

### Output
The analysis generates:
- Processed data files in `data/processed/`
- Summary statistics in `output/reports/`
- Visualizations in `output/figures/`

## Project Structure
- `config.R`: Configuration settings
- `data/`: Raw and processed iperf data
- `R/`: Functions for parsing, analyzing, and visualizing iperf data
- `scripts/`: Main processing scripts
- `output/`: Generated results and visualizations
