# Install required packages
required_packages <- c(
  # Shiny app packages
  "shiny",
  "shinydashboard",
  "DT",
  "plotly",
  "shinyjs",
  "shinycssloaders",
  
  # Data processing packages
  "tidyverse",
  "here",
  "jsonlite",
  "fs",
  "patchwork",
  "scales",
  "zoo",
  "lubridate"
)

# Check and install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse=", "), "\n")
  install.packages(new_packages)
}

cat("All required packages have been installed.\n")
