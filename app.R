library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(readr)

# Load datasets
climate_temperature_interpolated <- read_csv("data/climate_temperature_interpolated.csv")
climate_rainfall_interpolated <- read_csv("data/climate_rainfall_interpolated.csv")
climate_windspeed_interpolated <- read_csv("data/climate_windspeed_interpolated.csv")

# Convert date columns
climate_temperature_interpolated$date <- as.Date(climate_temperature_interpolated$date)
climate_rainfall_interpolated$date <- as.Date(climate_rainfall_interpolated$date)
climate_windspeed_interpolated$date <- as.Date(climate_windspeed_interpolated$date)

# Create line chart function
create_line_chart <- function(data, selected_stations, dataset_type, var_type, date_range) {
  
  var_mapping <- list(
    "rainfall" = list(
      "Total Rainfall" = "Daily Rainfall Total (mm)",
      "Highest 30 Min Rainfall" = "Highest 30 Min Rainfall (mm)",
      "Highest 60 Min Rainfall" = "Highest 60 Min Rainfall (mm)",
      "Highest 120 Min Rainfall" = "Highest 120 Min Rainfall (mm)"
    ),
    "temperature" = list(
      "Mean Temperature" = "Mean Temperature (°C)",
      "Maximum Temperature" = "Maximum Temperature (°C)",
      "Minimum Temperature" = "Minimum Temperature (°C)"
    ),
    "windspeed" = list(
      "Mean Wind Speed" = "Mean Wind Speed (km/h)",
      "Max Wind Speed" = "Max Wind Speed (km/h)"
    )
  )
  
  agg_functions <- list(
    "rainfall" = list(
      "Total Rainfall" = "sum",
      "Highest 30 Min Rainfall" = "max",
      "Highest 60 Min Rainfall" = "min",
      "Highest 120 Min Rainfall" = "min"
    ),
    "temperature" = list(
      "Mean Temperature" = "mean",
      "Maximum Temperature" = "max",
      "Minimum Temperature" = "min"
    ),
    "windspeed" = list(
      "Mean Wind Speed" = "mean",
      "Max Wind Speed" = "max"
    )
  )
  
  units <- list(
    "rainfall" = "mm",
    "temperature" = "°C",
    "windspeed" = "km/h"
  )
  
  selected_var <- var_mapping[[dataset_type]][[var_type]]
  selected_unit <- units[[dataset_type]]
  selected_agg <- agg_functions[[dataset_type]][[var_type]]
  
  plot_data <- data %>%
    filter(Station %in% selected_stations,
           date >= as.Date(date_range[1]),
           date <= as.Date(date_range[2])) %>%
    group_by(Station, Year = year(date), Month = month(date)) %>%
    summarise(
      Value = case_when(
        selected_agg == "mean" ~ mean(get(selected_var), na.rm = TRUE),
        selected_agg == "max" ~ max(get(selected_var), na.rm = TRUE),
        selected_agg == "min" ~ min(get(selected_var), na.rm = TRUE),
        selected_agg == "sum" ~ sum(get(selected_var), na.rm = TRUE)
      ),
      .groups = 'drop'
    ) %>%
    mutate(Date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
    arrange(Station, Date)
  
  colors <- RColorBrewer::brewer.pal(12, "Dark2")[1:length(selected_stations)]
  
  if(length(selected_stations) > 12) {
    colors <- rep(colors, ceiling(length(selected_stations)/12))[1:length(selected_stations)]
  }
  
  p <- plot_ly() %>%
    layout(
      xaxis = list(
        title = "",
        tickformat = "%b %Y",
        tickangle = 45,
        range = c(as.Date(date_range[1]), as.Date(date_range[2]))
      ),
      yaxis = list(
        title = paste(var_type, "(", selected_unit, ")")
      ),
      title = list(
        text = paste(var_type, "Trends by Station"),
        y = 0.95
      ),
      hovermode = "x unified",
      showlegend = TRUE,
      legend = list(
        title = list(
          text = "Stations"
        ),
        x = 1.02,
        y = 1,
        xanchor = "left",
        font = list(
          size = 10
        ),
        itemsizing = "constant"
      ),
      margin = list(
        l = 50,
        r = 150,
        t = 50,
        b = 100
      )
    )
  
  for(i in seq_along(selected_stations)) {
    station_data <- plot_data %>% filter(Station == selected_stations[i])
    
    p <- p %>% add_trace(
      data = station_data,
      x = ~Date,
      y = ~Value,
      type = 'scatter',
      mode = 'lines',
      line = list(
        color = colors[i],
        width = 2
      ),
      hovertemplate = paste(
        "Station: %{fullData.name}<br>",
        "Month: %{x|%b %Y}<br>",
        paste(var_type, ": %{y:.1f}", selected_unit),
        "<extra></extra>"
      ),
      name = selected_stations[i]
    )
  }
  
  return(p)
}

# UI
ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center;",
    tags$img(src = "sun.png", height = "30px", style = "margin-right: 10px;"),
    "Weather Pulse"
  ),
  theme = bslib::bs_theme(version = 4),
  
  # Add custom CSS
  tags$head(
    tags$style(HTML("
        /* Style for checkbox container */
        .checkbox {
            margin: 5px 0;
        }
        
        /* Style for checkbox labels */
        .checkbox label {
            display: flex;
            align-items: center;
            padding: 3px 0;
        }
        
        /* Style for checkbox input */
        .checkbox input[type='checkbox'] {
            margin-right: 8px;
        }
        
        /* Hover effect */
        .checkbox label:hover {
            background-color: #f8f8f8;
        }
    "))
  ),
  
  # Home panel
  tabPanel("Home",
           fluidPage(
             div(
               class = "welcome-container",
               style = "padding: 20px;",
               h1("Welcome to Weather Pulse"),
               p("Explore Singapore's climate data through interactive visualizations and forecasting tools.")
             )
           )
  ),
  
  navbarMenu("Time Series Analysis",
             tabPanel("Exploratory Data Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("dataset_type", "Select Dataset:",
                                      choices = c("Temperature" = "temperature",
                                                  "Rainfall" = "rainfall",
                                                  "Wind Speed" = "windspeed")),
                          
                          selectInput("var_type", "Select Variable:",
                                      choices = NULL),
                          
                          # Checkbox group for stations
                          div(style = "margin-bottom: 15px;",
                              tags$label("Select Stations:"),
                              div(style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: white;",
                                  checkboxGroupInput("selected_stations", 
                                                     label = NULL,
                                                     choices = NULL)
                              )
                          ),
                          
                          dateRangeInput("date_range", "Select Date Range:",
                                         start = min(climate_temperature_interpolated$date),
                                         end = max(climate_temperature_interpolated$date)),
                          
                          width = 3
                        ),
                        mainPanel(
                          plotlyOutput("line_chart", height = "600px"),
                          width = 9
                        )
                      )
             ),
             tabPanel("Time Series Forecasting",
                      fluidPage(
                        h3("Time Series Forecasting"),
                        p("Coming soon...")
                      )
             )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive dataset based on selection
  selected_data <- reactive({
    switch(input$dataset_type,
           "temperature" = climate_temperature_interpolated,
           "rainfall" = climate_rainfall_interpolated,
           "windspeed" = climate_windspeed_interpolated)
  })
  
  # Update variable choices based on dataset
  observe({
    var_choices <- switch(input$dataset_type,
                          "temperature" = c(
                            "Mean Temperature" = "Mean Temperature",
                            "Maximum Temperature" = "Maximum Temperature",
                            "Minimum Temperature" = "Minimum Temperature"
                          ),
                          "rainfall" = c(
                            "Total Rainfall" = "Total Rainfall",
                            "Highest 30 Min Rainfall" = "Highest 30 Min Rainfall",
                            "Highest 60 Min Rainfall" = "Highest 60 Min Rainfall",
                            "Highest 120 Min Rainfall" = "Highest 120 Min Rainfall"
                          ),
                          "windspeed" = c(
                            "Mean Wind Speed" = "Mean Wind Speed",
                            "Max Wind Speed" = "Max Wind Speed"
                          )
    )
    updateSelectInput(session, "var_type", choices = var_choices)
  })
  
  # Update station choices based on dataset
  observe({
    req(input$dataset_type)
    stations <- sort(unique(selected_data()$Station))
    updateCheckboxGroupInput(session, "selected_stations",
                             choices = stations,
                             selected = stations[1])
  })
  
  # Generate line chart
  output$line_chart <- renderPlotly({
    req(input$dataset_type,
        input$var_type,
        input$selected_stations,
        input$date_range)
    
    create_line_chart(
      data = selected_data(),
      selected_stations = input$selected_stations,
      dataset_type = input$dataset_type,
      var_type = input$var_type,
      date_range = input$date_range
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)