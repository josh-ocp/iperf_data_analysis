# Interactive Shiny Dashboard for iPerf Data Analysis

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(here)
library(DT)
library(plotly)
library(shinyjs)    # For loading indicators
library(shinycssloaders)  # For spinners

# Source configuration and utility functions
source(here("config.R"))
source(here("R", "parse_iperf.R"))
source(here("R", "analyze_iperf.R"))
source(here("R", "visualize_iperf.R"))

# Add debugging function for file finding
debug_file_paths <- function() {
  input_dir <- here("Input")
  message("Looking for input directory at: ", input_dir)
  message("Directory exists: ", dir.exists(input_dir))
  
  if(dir.exists(input_dir)) {
    files <- list.files(input_dir, pattern = "\\.(csv|txt|log)$", full.names = TRUE, recursive = TRUE)
    message("Found ", length(files), " files with .csv, .txt, or .log extensions")
    message("Files found: ", paste(basename(files), collapse = ", "))
    
    # Show a preview of the first file content for debugging
    if(length(files) > 0) {
      tryCatch({
        file_path <- files[1]
        message("File preview for: ", basename(file_path))
        first_lines <- readLines(file_path, n = 10)
        message("File content (first 10 lines):")
        message(paste(first_lines, collapse = "\n"))
        
        # Check for iperf data patterns
        if(any(grepl("^\\[\\s*\\d+\\]\\s+\\d+\\.\\d+-\\d+\\.\\d+\\s+sec", first_lines))) {
          message("File appears to be a raw iperf output file")
        } else if(any(grepl("interval_start|bitrate|bits_per_second", first_lines))) {
          message("File appears to be a formatted CSV file")
        } else {
          message("File format is unknown - may cause parsing issues")
        }
      }, error = function(e) {
        message("Error reading file preview: ", e$message)
      })
    }
    
    return(files)
  } else {
    message("Input directory not found. Creating it now.")
    dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
    return(character(0))
  }
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "iPerf Data Analysis"),
  
  # Sidebar with controls
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Test Comparison", tabName = "comparison", icon = icon("chart-bar")),
      menuItem("Time Analysis", tabName = "time", icon = icon("clock")),
      menuItem("Raw Data", tabName = "data", icon = icon("table")),
      menuItem("Reports", tabName = "reports", icon = icon("file-alt"))
    ),
    
    # Filters
    dateRangeInput("date_range", "Date Range",
                  start = Sys.Date() - 30, end = Sys.Date()),
    selectInput("user_filter", "User", choices = c("All"), selected = "All"),
    selectInput("isp_filter", "ISP", choices = c("All"), selected = "All"),
    sliderInput("throughput_range", "Throughput Range (Mbps)",
               min = 0, max = 1000, value = c(0, 1000)),
    actionButton("refresh_data", "Refresh Data", icon = icon("sync"))
  ),
  
  # Main content area
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
        fluidRow(
          infoBoxOutput("avg_throughput_box", width = 4),
          infoBoxOutput("stability_box", width = 4),
          infoBoxOutput("test_count_box", width = 4)
        ),
        fluidRow(
          box(title = "Performance Dashboard", width = 12, status = "primary",
              plotlyOutput("dashboard_plot", height = "600px"))
        )
      ),
      
      # Test comparison tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(title = "Compare Tests", width = 12, 
              checkboxGroupInput("selected_tests", "Select Tests:", choices = NULL),
              plotOutput("test_comparison_plot", height = "600px"))
        )
      ),
      
      # Time analysis tab
      tabItem(tabName = "time",
        fluidRow(
          tabBox(width = 12,
            tabPanel("Time of Day", plotOutput("time_of_day_plot")),
            tabPanel("Performance Trends", plotOutput("trends_plot")),
            tabPanel("Heatmap", plotOutput("heatmap_plot"))
          )
        )
      ),
      
      # Raw data tab
      tabItem(tabName = "data",
        fluidRow(
          box(width = 12, 
              DTOutput("data_table"))
        )
      ),
      
      # Reports tab
      tabItem(tabName = "reports",
        fluidRow(
          box(title = "Available Reports", width = 12, status = "info",
              uiOutput("report_links"))
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive data loading
  tcp_data <- reactiveVal(NULL)
  
  # Load data on startup and when refresh button is clicked
  observeEvent(c(input$refresh_data, 1), {
    # Debug file paths
    files <- debug_file_paths()
    
    if(length(files) == 0) {
      message("No files found. Please add CSV or TXT files to the Input directory.")
      showNotification("No files found in Input directory. Please add CSV or TXT files.", 
                      type = "error", duration = 10)
      return(NULL)
    }
    
    # Process data using existing functions from parse_iperf.R
    tryCatch({
      # Use the existing process_tcp_data function that handles all file types
      data <- process_tcp_data(files)
      
      if(is.null(data) || nrow(data) == 0) {
        message("Failed to parse any data files. Check console for details.")
        showNotification("Failed to parse any data files. Check console for details.", 
                      type = "error", duration = 10)
        return(NULL)
      }
      
      tcp_data(data)
    }, error = function(e) {
      message("Error processing files: ", e$message)
      showNotification(paste("Error processing files:", e$message), 
                     type = "error", duration = 10)
    })
  })
  
  # Filter data based on inputs
  filtered_data <- reactive({
    data <- tcp_data()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    # Apply date filter
    if("test_datetime" %in% names(data) && !is.null(input$date_range)) {
      data <- data %>%
        filter(test_datetime >= input$date_range[1],
               test_datetime <= input$date_range[2])
    }
    
    # Apply user filter
    if("user_name" %in% names(data) && input$user_filter != "All") {
      data <- data %>% filter(user_name == input$user_filter)
    }
    
    # Apply ISP filter
    if("isp" %in% names(data) && input$isp_filter != "All") {
      data <- data %>% filter(isp == input$isp_filter)
    }
    
    # Apply throughput filter
    if("bitrate_mbps" %in% names(data)) {
      data <- data %>%
        filter(bitrate_mbps >= input$throughput_range[1],
               bitrate_mbps <= input$throughput_range[2])
    }
    
    # If dataset is very large, consider sampling for certain visualizations
    if(nrow(data) > 10000 && !is.null(session$userData$sampling_enabled) && session$userData$sampling_enabled) {
      data <- data %>% sample_n(min(10000, nrow(data)))
    }
    
    return(data)
  })
  
  # Dashboard info boxes
  output$avg_throughput_box <- renderInfoBox({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) {
      avg <- 0
    } else {
      avg <- mean(data$bitrate_mbps, na.rm = TRUE)
    }
    
    infoBox(
      "Average Throughput", 
      paste0(round(avg, 1), " Mbps"),
      icon = icon("tachometer-alt"),
      color = "blue"
    )
  })
  
  output$stability_box <- renderInfoBox({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0 || length(unique(data$bitrate_mbps)) <= 1) {
      cv <- 0
    } else {
      cv <- sd(data$bitrate_mbps, na.rm = TRUE) / mean(data$bitrate_mbps, na.rm = TRUE) * 100
    }
    
    infoBox(
      "Stability (CV)", 
      paste0(round(cv, 1), "%"),
      icon = icon("chart-line"),
      color = ifelse(cv < 10, "green", ifelse(cv < 20, "yellow", "red"))
    )
  })
  
  output$test_count_box <- renderInfoBox({
    data <- filtered_data()
    if(is.null(data)) {
      count <- 0
    } else {
      count <- length(unique(data$file))
    }
    
    infoBox(
      "Tests", 
      count,
      icon = icon("vial"),
      color = "purple"
    )
  })
  
  # Main dashboard plot (convert existing dashboard to plotly)
  output$dashboard_plot <- renderPlotly({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) {
      return(plot_ly() %>%
        add_annotations(text = "No data available. Click 'Refresh Data' to load data.", 
                        x = 0.5, y = 0.5, font = list(size = 15)))
    }
    
    # Use your existing dashboard function, then convert to plotly
    dashboard <- create_performance_dashboard(data)
    ggplotly(dashboard, tooltip = "text") %>%
      layout(autosize = TRUE)
  })
  
  # Test comparison plot
  output$test_comparison_plot <- renderPlot({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0 || 
       is.null(input$selected_tests) || length(input$selected_tests) == 0) {
      return(NULL)
    }
    
    # Filter to selected tests
    data <- data %>% filter(file %in% input$selected_tests)
    
    # Use your existing comparison function
    plot_test_comparison(data)
  })
  
  # Time of day plot
  output$time_of_day_plot <- renderPlot({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    plot_time_of_day(data)
  })
  
  # Performance trends plot
  output$trends_plot <- renderPlot({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    plot_performance_trends(data)
  })
  
  # Heatmap plot
  output$heatmap_plot <- renderPlot({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    plot_time_pattern_heatmap(data)
  })
  
  # Data table
  output$data_table <- renderDT({
    data <- filtered_data()
    if(is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Create a simplified view of the data
    data %>%
      select(file, test_datetime, interval_start, bitrate_mbps, 
             user_name, isp, host) %>%
      arrange(desc(test_datetime), file, interval_start) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons'
      )
  })
  
  # Generate report links
  output$report_links <- renderUI({
    # Check what reports are available
    report_files <- list.files(here("output", "reports"), full.names = TRUE)
    
    if(length(report_files) == 0) {
      return(h4("No reports generated yet"))
    }
    
    # Create links to reports
    link_list <- lapply(report_files, function(file) {
      file_name <- basename(file)
      tags$li(
        tags$a(href = file, target = "_blank", file_name),
        " - Generated: ", format(file.info(file)$mtime, "%Y-%m-%d %H:%M:%S")
      )
    })
    
    tags$ul(link_list)
  })
  
  # Update UI elements based on loaded data
  observe({
    data <- tcp_data()
    if(!is.null(data) && nrow(data) > 0) {
      if("user_name" %in% names(data)) {
        updateSelectInput(session, "user_filter", 
                         choices = c("All", unique(data$user_name)))
      }
      
      if("isp" %in% names(data)) {
        updateSelectInput(session, "isp_filter", 
                         choices = c("All", unique(data$isp)))
      }
      
      if("file" %in% names(data)) {
        updateCheckboxGroupInput(session, "selected_tests", 
                               choices = unique(data$file))
      }
      
      if("bitrate_mbps" %in% names(data)) {
        max_throughput <- ceiling(max(data$bitrate_mbps, na.rm = TRUE))
        updateSliderInput(session, "throughput_range",
                         max = max(1000, max_throughput),
                         value = c(0, max_throughput))
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
