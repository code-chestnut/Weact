# Weact
Result
# app.R

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
library(con2aqi)

# ---------------------------
# Global Data Loading
# ---------------------------
# Load merged data from individual CSV files
merged_data <- {
  camp_q2_data <- read_csv("camp_q2_data.csv") %>%
    mutate(
      time_stamp   = ymd_hms(time_stamp),
      sensor_index = as.integer(sensor_index),
      pm25_alt     = `pm2.5_alt`,
      pm25_aqi     = `pm2.5_aqi`
    )
  sensor_info <- read_csv("sensorindex_name_latlong.csv") %>%
    mutate(
      sensor_index = as.integer(sensor_index),
      height_ft    = `height (ft)`,
      altitude_ft  = `altitude (ft)`
    ) %>%
    select(sensor_index, name_on_map, height_ft, altitude_ft, Latitude, Longitude)
  
  left_join(camp_q2_data, sensor_info, by = "sensor_index")
}

# Load the new combined, cleaned dataset
cleaned_data <- read_csv("combined_cleaned_data.csv") %>%
  mutate(
    time_stamp   = ymd_hms(time_stamp),
    sensor_index = as.integer(sensor_index)
    # Add any additional mutations if needed to ensure consistency
  )

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Sensor PM2.5 and AQI Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Data source selector: choose between merged and cleaned datasets
      selectInput(
        inputId = "data_source",
        label   = "Select Data Source:",
        choices = c("Merged Data", "Cleaned Data"),
        selected = "Merged Data"
      ),
      # Sensor selection (will update in the server)
      selectInput(
        inputId = "sensor",
        label   = "Select Sensor:",
        choices = NULL,
        selected = "All"
      ),
      # Date range input (will update in the server)
      dateRangeInput(
        inputId = "date_range",
        label   = "Select Date Range:",
        start   = NULL,
        end     = NULL
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("PM2.5 Distribution Map", leafletOutput("pm25_map")),
        tabPanel("AQI Time Series", plotOutput("aqi_plot"))
      )
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # Reactive: Return the selected dataset
  selected_data <- reactive({
    if (input$data_source == "Merged Data") {
      merged_data
    } else {
      cleaned_data
    }
  })
  
  # Update UI inputs when the data source changes
  observe({
    data <- selected_data()
    sensor_choices <- c("All", sort(unique(data$name_on_map)))
    updateSelectInput(session, "sensor", choices = sensor_choices, selected = "All")
    
    updateDateRangeInput(session, "date_range",
                         start = min(as.Date(data$time_stamp), na.rm = TRUE),
                         end   = max(as.Date(data$time_stamp), na.rm = TRUE))
  })
  
  # Reactive: Filter data based on sensor selection and date range
  filtered_data <- reactive({
    req(input$date_range)
    data <- selected_data() %>%
      filter(as.Date(time_stamp) >= input$date_range[1],
             as.Date(time_stamp) <= input$date_range[2])
    if (input$sensor != "All") {
      data <- data %>% filter(name_on_map == input$sensor)
    }
    data
  })
  
  # Reactive: Summarize data per sensor for mapping
  sensor_summary <- reactive({
    filtered_data() %>%
      group_by(sensor_index, name_on_map, Latitude, Longitude) %>%
      summarize(
        avg_pm25 = mean(pm25_alt, na.rm = TRUE),
        avg_aqi  = mean(pm25_aqi, na.rm = TRUE),
        .groups  = "drop"
      )
  })
  
  # Render the Leaflet map for PM2.5 distribution
  output$pm25_map <- renderLeaflet({
    summary_df <- sensor_summary()
    leaflet(summary_df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng         = ~Longitude,
        lat         = ~Latitude,
        radius      = ~avg_pm25,  # Marker size proportional to average PM2.5
        color       = "blue",
        fillOpacity = 0.7,
        popup       = ~paste0("<strong>", name_on_map, "</strong><br>",
                              "Avg PM2.5: ", round(avg_pm25, 2), "<br>",
                              "Avg AQI: ", round(avg_aqi, 2))
      )
  })
  
  # Render the time series plot for AQI changes over time
  output$aqi_plot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = time_stamp, y = pm25_aqi, color = name_on_map)) +
      geom_line() +
      labs(
        title  = "AQI Changes Over Time",
        x      = "Time",
        y      = "AQI",
        color  = "Sensor"
      ) +
      theme_minimal()
  })
}

# ---------------------------
# Run the Shiny App
# ---------------------------
shinyApp(ui = ui, server = server)
