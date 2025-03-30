library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(readr)
library(tsibble)
library(feasts)
library(ggplot2)

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


# Calendar heatmap function
create_calendar_heatmap <- function(data, selected_station, selected_year, dataset_type, var_type) {
  
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
  
  units <- list(
    "rainfall" = "mm",
    "temperature" = "°C",
    "windspeed" = "km/h"
  )
  
  color_schemes <- list(
    "rainfall" = "Blues",
    "temperature" = "Reds",
    "windspeed" = "Greens"
  )
  
  selected_var <- var_mapping[[dataset_type]][[var_type]]
  selected_unit <- units[[dataset_type]]
  selected_colorscale <- color_schemes[[dataset_type]]
  
  reverse_scale <- ifelse(dataset_type == "temperature", FALSE, TRUE)
  
  first_day <- as.Date(sprintf("%d-01-01", selected_year))
  last_date <- as.Date(sprintf("%d-12-31", selected_year))
  
  plot_data <- data %>%
    filter(Station == selected_station,
           date >= first_day,
           date <= last_date) %>%
    mutate(
      weekday = wday(date, label = TRUE, abbr = TRUE, week_start = 1),
      week_num = floor((yday(date) + wday(first_day, week_start = 1) - 1) / 7),
      month_label = factor(month(date, label = TRUE, abbr = TRUE))
    ) %>%
    mutate(
      weekday = factor(weekday, 
                       levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
    )
  
  week_to_month <- plot_data %>%
    group_by(week_num) %>%
    summarise(month_label = first(month_label))
  
  p <- plot_ly(
    data = plot_data,
    x = ~week_num,
    y = ~weekday,
    z = as.formula(paste0("~`", selected_var, "`")),
    type = "heatmap",
    colorscale = selected_colorscale,
    reversescale = reverse_scale,
    text = ~paste(
      "Date:", date,
      "<br>Day:", weekday,
      "<br>Month:", month_label,
      "<br>", var_type, ":", round(get(selected_var), 1), selected_unit
    ),
    hoverinfo = "text",
    hoverongaps = FALSE
  ) %>%
    layout(
      title = paste(var_type, "Calendar Heatmap -", selected_station, "(",selected_year,")"),
      xaxis = list(
        title = "Month",
        ticktext = as.character(unique(plot_data$month_label)),
        tickvals = sapply(unique(plot_data$month_label), 
                          function(m) median(plot_data$week_num[plot_data$month_label == m]))  
      ),
      yaxis = list(
        title = "Day of Week",
        categoryarray = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
        categoryorder = "array",
        autorange = "reversed"
      ),
      margin = list(
        l = 50,
        r = 50,
        b = 50,
        t = 50
      )
    ) %>%
    colorbar(
      title = paste(var_type, "(",selected_unit,")"),
      orientation = "h",
      len = 0.8,
      y = -0.4,
      thickness = 15
    )
  
  return(p)
}


# Create sunburst diagram function
create_sunburst <- function(data, selected_station, dataset_type, var_type, year_range) {
  
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
  
  color_schemes <- list(
    "rainfall" = "Blues",
    "temperature" = "OrRd",
    "windspeed" = "Greens"
  )
  
  selected_var <- var_mapping[[dataset_type]][[var_type]]
  selected_unit <- units[[dataset_type]]
  selected_colorscale <- color_schemes[[dataset_type]]
  selected_agg <- agg_functions[[dataset_type]][[var_type]]
  
  reverse_scale <- ifelse(dataset_type == "temperature", FALSE, TRUE)
  
  plot_data <- data %>%
    filter(Station == selected_station,
           Year >= year_range[1],
           Year <= year_range[2]) %>%
    group_by(Year, Month) %>%
    summarise(
      Value = case_when(
        selected_agg == "mean" ~ mean(get(selected_var), na.rm = TRUE),
        selected_agg == "max" ~ max(get(selected_var), na.rm = TRUE),
        selected_agg == "min" ~ min(get(selected_var), na.rm = TRUE),
        selected_agg == "sum" ~ sum(get(selected_var), na.rm = TRUE)
      ),
      .groups = 'drop'
    )
  
  
  p <- plot_ly()
  
  
  years <- year_range[1]:year_range[2]
  months <- 1:12
  angles <- seq(0, 330, by = 30)
  
  for(i in seq_along(years)) {
    year <- years[i]
    radius <- i
    
    year_data <- plot_data %>% filter(Year == year)
    
    vals <- sapply(months, function(m) {
      val <- year_data$Value[year_data$Month == m]
      if(length(val) == 0) return(0)
      return(val)
    })
    
    p <- p %>% add_trace(
      type = "barpolar",
      r = rep(1, 12),
      theta = angles,
      base = radius - 0.45,
      width = 29,
      marker = list(
        color = vals,
        colorscale = selected_colorscale,
        reversescale = reverse_scale,
        showscale = (i == 1),
        colorbar = list(
          title = paste(var_type, "(", selected_unit, ")"),
          len = 0.5,         
          thickness = 10,   
          x = 0.95,         
          y = 0.5,          
          tickfont = list(size = 10),  
          titlefont = list(size = 10)  
        ),
        line = list(
          color = 'white',
          width = 2
        )
      ),
      name = as.character(year),
      text = paste0(
        "Year: ", year, "<br>",
        "Month: ", month.abb, "<br>",
        var_type, ": ", round(vals, 1), " ", selected_unit
      ),
      hoverinfo = "text"
    )
  }
  
  
  p <- p %>% layout(
    polar = list(
      radialaxis = list(
        visible = FALSE,
        range = c(0, length(years) + 1),
        showline = FALSE,
        showgrid = FALSE
      ),
      angularaxis = list(
        ticktext = month.abb,
        tickvals = angles,
        direction = "clockwise",
        showline = FALSE,
        showgrid = FALSE
      ),
      bgcolor = "white"
    ),
    title = list(
      text = paste(var_type, "Patterns -", selected_station, 
                   "(", year_range[1], "-", year_range[2], ")"),
      y = 0.95,
      pad = list(b = 20)
    ),
    margin = list(
      t = 100
    ),
    showlegend = FALSE,
    paper_bgcolor = "white",
    plot_bgcolor = "white"
  )
  
  return(p)
}

# Create STL Decomposition Function
create_ts_decomposition <- function(dataset_type,
                                    selected_station,
                                    selected_var,
                                    date_range = c("2020-01-01", "2024-12-31")) {
  
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
  
  units <- list(
    "rainfall" = "mm",
    "temperature" = "°C",
    "windspeed" = "km/h"
  )
  
  data <- switch(dataset_type,
                 "rainfall" = climate_rainfall_interpolated,
                 "temperature" = climate_temperature_interpolated,
                 "windspeed" = climate_windspeed_interpolated,
                 stop("Invalid dataset type"))
  
  start_date <- as.Date(date_range[1])
  end_date <- as.Date(date_range[2])
  
  months_diff <- length(seq(start_date, end_date, by = "month"))
  
  message("Date range: ", start_date, " to ", end_date)
  message("Number of months in range: ", months_diff)
  
  ts_data <- data %>%
    filter(Station == selected_station,
           date >= start_date,
           date <= end_date) %>%
    select(date, Station, !!sym(var_mapping[[dataset_type]][[selected_var]])) %>%
    rename(Value = !!sym(var_mapping[[dataset_type]][[selected_var]])) %>%
    mutate(
      year_month = yearmonth(date)
    ) %>%
    group_by(year_month) %>%
    summarise(
      Value = mean(Value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    as_tsibble(index = year_month) %>%
    fill_gaps()
  
  message("Number of observations in dataset: ", nrow(ts_data))
  
  if (nrow(ts_data) < 24) {
    stop("Not enough data for decomposition. Need at least 24 months of data. Current months: ", nrow(ts_data))
  }
  
  title <- paste(selected_var, "Time Series Decomposition at", selected_station,
                 "\nPeriod:", format(start_date, "%b %Y"), 
                 "to", format(end_date, "%b %Y"))
  
  decomp_plot <- ts_data %>%
    model(
      stl = STL(Value ~ season(period = 12) + trend())
    ) %>%
    components() %>%
    autoplot() +
    theme_bw() +
    labs(
      title = title,
      x = "Period",
      y = paste(selected_var, "(", units[[dataset_type]], ")"),
      season_year = "Seasonal Pattern",
      trend = "Trend",
      remainder = "Random"
    ) +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.position = "none"
    )
  
  return(decomp_plot)
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
  tabPanel(
    div(style = "display: flex; align-items: center;",
        tags$img(src = "house.png", height = "15px", style = "margin-right: 5px;"),
        "Home"
    ),
    fluidPage(
      div(
        class = "welcome-container",
        style = "padding: 20px;",
        h1("Welcome to Weather Pulse"),
        p("Explore Singapore's climate data through interactive visualizations and forecasting tools.")
      )
    )
  ),
  
  navbarMenu(
    HTML(paste0(
      '<div style="display: inline-flex; align-items: center;">',
      '<img src="clock.png" height="15px" style="margin-right: 5px;">',
      'Time Series Analysis',
      '</div>'
    )),
    tabPanel("Exploratory Data Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset_type", "Select Dataset:",
                             choices = c("Temperature" = "temperature",
                                         "Rainfall" = "rainfall",
                                         "Wind Speed" = "windspeed")),
                 
                 selectInput("var_type", "Select Variable:",
                             choices = NULL),
                 
                 # Checkbox group for stations (for line chart)
                 conditionalPanel(
                   condition = "input.viz_type == 'Line Chart'",
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
                                  end = max(climate_temperature_interpolated$date))
                 ),
                 
                 # Controls for calendar heatmap
                 conditionalPanel(
                   condition = "input.viz_type == 'Calendar Heatmap'",
                   selectInput("heatmap_station", "Select Station:",
                               choices = NULL),
                   selectInput("selected_year", "Select Year:",
                               choices = NULL)
                 ),
                 
                 # Controls for sunburst plot
                 conditionalPanel(
                   condition = "input.viz_type == 'Sunburst Plot'",
                   selectInput("sunburst_station", "Select Station:",
                               choices = NULL),
                   sliderInput("year_range", "Select Year Range:",
                               min = 2020,
                               max = 2024,
                               value = c(2020, 2024),
                               step = 1,
                               sep = "")
                 ),
                 
                 # Controls for STL Decomposition
                 conditionalPanel(
                   condition = "input.viz_type == 'STL Decomposition'",
                   selectInput("decomp_station", "Select Station:",
                               choices = NULL),
                   dateRangeInput("decomp_date_range", "Select Date Range:",
                                  start = "2020-01-01",
                                  end = "2024-12-31",
                                  min = "2020-01-01",
                                  max = "2024-12-31")
                 ),
                 
                 width = 3
               ),
               mainPanel(
                 tabsetPanel(
                   id = "viz_type",
                   tabPanel("Line Chart",
                            plotlyOutput("line_chart", height = "600px")
                   ),
                   tabPanel("Calendar Heatmap",
                            plotlyOutput("calendar_heatmap", height = "600px")
                   ),
                   tabPanel("Sunburst Plot",
                            plotlyOutput("sunburst_plot", height = "600px")
                   ),
                   tabPanel("STL Decomposition",
                            plotOutput("decomposition_plot", height = "600px")
                   )
                 ),
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
  
  # Update station choices for all plots
  observe({
    req(input$dataset_type)
    stations <- sort(unique(selected_data()$Station))
    
    updateCheckboxGroupInput(session, "selected_stations",
                             choices = stations,
                             selected = stations[1])
    
    updateSelectInput(session, "heatmap_station",
                      choices = stations,
                      selected = stations[1])
    
    updateSelectInput(session, "sunburst_station",
                      choices = stations,
                      selected = stations[1])
    
    updateSelectInput(session, "decomp_station",
                      choices = stations,
                      selected = stations[1])
  })
  
  # Update year choices for heatmap
  observe({
    req(input$dataset_type, input$heatmap_station)
    years <- selected_data() %>%
      filter(Station == input$heatmap_station) %>%
      pull(date) %>%
      year() %>%
      unique() %>%
      sort()
    updateSelectInput(session, "selected_year",
                      choices = years,
                      selected = max(years))
  })
  
  # Update year range for sunburst
  observe({
    req(input$dataset_type, input$sunburst_station)
    years <- selected_data() %>%
      filter(Station == input$sunburst_station) %>%
      pull(date) %>%
      year() %>%
      unique() %>%
      sort()
    
    updateSliderInput(session, "year_range",
                      min = min(years),
                      max = max(years),
                      value = c(min(years), max(years)))
  })
  
  # Update date range for decomposition
  observe({
    req(input$dataset_type, input$decomp_station)
    date_range <- selected_data() %>%
      filter(Station == input$decomp_station) %>%
      summarise(
        min_date = min(date),
        max_date = max(date)
      )
    
    updateDateRangeInput(session, "decomp_date_range",
                         start = date_range$min_date,
                         end = date_range$max_date,
                         min = date_range$min_date,
                         max = date_range$max_date)
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
  
  # Generate calendar heatmap
  output$calendar_heatmap <- renderPlotly({
    req(input$dataset_type,
        input$var_type,
        input$heatmap_station,
        input$selected_year)
    
    create_calendar_heatmap(
      data = selected_data(),
      selected_station = input$heatmap_station,
      selected_year = as.numeric(input$selected_year),
      dataset_type = input$dataset_type,
      var_type = input$var_type
    )
  })
  
  # Generate sunburst plot
  output$sunburst_plot <- renderPlotly({
    req(input$dataset_type,
        input$var_type,
        input$sunburst_station,
        input$year_range)
    
    plot_data <- selected_data() %>%
      mutate(Year = year(date),
             Month = month(date))
    
    create_sunburst(
      data = plot_data,
      selected_station = input$sunburst_station,
      dataset_type = input$dataset_type,
      var_type = input$var_type,
      year_range = input$year_range
    )
  })
  
  # Generate decomposition plot
  output$decomposition_plot <- renderPlot({
    req(input$dataset_type,
        input$var_type,
        input$decomp_station,
        input$decomp_date_range)
    
    tryCatch({
      create_ts_decomposition(
        dataset_type = input$dataset_type,
        selected_station = input$decomp_station,
        selected_var = input$var_type,
        date_range = input$decomp_date_range
      )
    }, error = function(e) {
      # Create an error plot
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error:", e$message),
                 size = 5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)