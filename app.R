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
library(fable)
library(gridExtra)
library(magrittr)
library(kableExtra)
library(shinyjs)
library(forecast)  
library(stats)
library(urca)
library(tidyr)
library(ggrepel)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(units)
library(spdep)

# Load datasets
climate_temperature_interpolated <- read_csv("data/climate_temperature_interpolated.csv")
climate_rainfall_interpolated <- read_csv("data/climate_rainfall_interpolated.csv")
climate_windspeed_interpolated <- read_csv("data/climate_windspeed_interpolated.csv")
climate_rainfall_geospatial <- readRDS("data/climate_rainfall3414.rds")
climate_temperature_geospatial <- readRDS("data/climate_temperature3414.rds")
climate_windspeed_geospatial <- readRDS("data/climate_windspeed3414.rds")


# Convert date columns
climate_temperature_interpolated$date <- as.Date(climate_temperature_interpolated$date)
climate_rainfall_interpolated$date <- as.Date(climate_rainfall_interpolated$date)
climate_windspeed_interpolated$date <- as.Date(climate_windspeed_interpolated$date)


#Create overview plot function
create_overview_plot <- function() {
  # Calculate monthly means for temperature and wind speed
  temp_data <- climate_temperature_interpolated %>%
    mutate(date = as.Date(date)) %>%
    group_by(yearmonth = floor_date(date, "month")) %>%
    summarise(
      mean_temp = mean(`Mean Temperature (°C)`, na.rm = TRUE)
    )
  
  wind_data <- climate_windspeed_interpolated %>%
    mutate(date = as.Date(date)) %>%
    group_by(yearmonth = floor_date(date, "month")) %>%
    summarise(
      mean_wind = mean(`Mean Wind Speed (km/h)`, na.rm = TRUE)
    )
  
  # Calculate monthly total rainfall first, then average across stations
  rain_data <- climate_rainfall_interpolated %>%
    mutate(date = as.Date(date)) %>%
    group_by(yearmonth = floor_date(date, "month"), Station) %>%
    summarise(
      monthly_total = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(yearmonth) %>%
    summarise(
      mean_rain = mean(monthly_total, na.rm = TRUE)
    )
  
  # Find max and min points and print them for debugging
  temp_max <- temp_data %>% slice_max(mean_temp, n = 1)
  temp_min <- temp_data %>% slice_min(mean_temp, n = 1)
  
  rain_max <- rain_data %>% slice_max(mean_rain, n = 1)
  rain_min <- rain_data %>% slice_min(mean_rain, n = 1)
  
  wind_max <- wind_data %>% slice_max(mean_wind, n = 1)
  wind_min <- wind_data %>% slice_min(mean_wind, n = 1)
  
  # Create the three plots
  p1 <- ggplot(temp_data, aes(x = yearmonth, y = mean_temp)) +
    geom_line(color = "#FF6B6B", size = 1) +
    geom_smooth(method = "lm", color = "#FF9999", se = FALSE, linetype = "dashed") +
    geom_point(data = bind_rows(temp_max, temp_min), 
               aes(x = yearmonth, y = mean_temp),
               color = "#FF6B6B", size = 3) +
    geom_text_repel(
      data = bind_rows(temp_max, temp_min),
      aes(label = sprintf("%.1f°C", mean_temp)),
      color = "#FF6B6B",
      fontface = "bold",
      box.padding = 0.5,
      point.padding = 0.5,
      nudge_x =50,
      force = 2
    ) +
    labs(title = "Mean Temperature",
         y = "Temperature (°C)") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
  
  p2 <- ggplot(rain_data, aes(x = yearmonth, y = mean_rain)) +
    geom_line(color = "#4ECDC4", size = 1) +
    geom_smooth(method = "lm", color = "#7FDFD9", se = FALSE, linetype = "dashed") +
    geom_point(data = bind_rows(rain_max, rain_min), 
               aes(x = yearmonth, y = mean_rain),
               color = "#4ECDC4", size = 3) +
    geom_text_repel(
      data = bind_rows(rain_max, rain_min),
      aes(label = sprintf("%.1fmm", mean_rain)),
      color = "#4ECDC4",
      fontface = "bold",
      box.padding = 0.5,
      point.padding = 0.5,
      nudge_x =50,
      force = 2
    ) +
    labs(title = "Mean Rainfall",
         y = "Rainfall (mm)") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
  
  p3 <- ggplot(wind_data, aes(x = yearmonth, y = mean_wind)) +
    geom_line(color = "#95A5A6", size = 1) +
    geom_smooth(method = "lm", color = "#BDC3C7", se = FALSE, linetype = "dashed") +
    geom_point(data = bind_rows(wind_max, wind_min), 
               aes(x = yearmonth, y = mean_wind),
               color = "#95A5A6", size = 3) +
    geom_text_repel(
      data = bind_rows(wind_max, wind_min),
      aes(label = sprintf("%.1fkm/h", mean_wind)),
      color = "#95A5A6",
      fontface = "bold",
      box.padding = 0.5,
      point.padding = 0.5,
      nudge_x =50,
      force = 2
    ) +
    labs(title = "Mean Wind Speed",
         y = "Wind Speed (km/h)") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
  
  # Combine the plots
  combined_plot <- gridExtra::grid.arrange(
    p1, p2, p3,
    ncol = 1,
    heights = c(1, 1, 1)
  )
  
  return(combined_plot)
}


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
  
  first_day <- as.Date(paste0(selected_year, "-01-01"))
  last_date <- as.Date(paste0(selected_year, "-12-31"))
  
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

# Model Diagnostics Function
create_model_diagnostics <- function(dataset_type,
                                     selected_station,
                                     selected_var,
                                     selected_model,
                                     training_start = "2020-01-01",
                                     training_end = "2023-12-31") {
  
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
  
  data <- switch(dataset_type,
                 "rainfall" = climate_rainfall_interpolated,
                 "temperature" = climate_temperature_interpolated,
                 "windspeed" = climate_windspeed_interpolated,
                 stop("Invalid dataset type"))
  
  training_start_date <- as.Date(training_start)
  training_end_date <- as.Date(training_end)
  
  ts_data <- data %>%
    filter(Station == selected_station,
           date >= training_start_date,
           date <= training_end_date) %>%
    select(date, Station, !!sym(var_mapping[[dataset_type]][[selected_var]])) %>%
    rename(Value = !!sym(var_mapping[[dataset_type]][[selected_var]])) %>%
    mutate(year_month = yearmonth(date)) %>%
    group_by(year_month) %>%
    summarise(Value = mean(Value, na.rm = TRUE),
              .groups = 'drop') %>%
    as_tsibble(index = year_month) %>%
    fill_gaps()
  
  model_spec <- switch(selected_model,
                       "SES" = ETS(Value ~ error("A") + trend("N") + season("N")),
                       "Holt" = ETS(Value ~ error("A") + trend("A") + season("N")),
                       "Damped Holt" = ETS(Value ~ error("A") + trend("Ad") + season("N")),
                       "Winter-Add" = ETS(Value ~ error("A") + trend("A") + season("A")),
                       "Winter-Mult" = ETS(Value ~ error("M") + trend("A") + season("M")),
                       "ARIMA" = ARIMA(Value),
                       stop("Invalid model type"))
  
  fit <- ts_data %>%
    model(Model = model_spec) 
  
  residuals_plot <- fit %>%
    augment() %>%
    autoplot(.innov) +
    labs(title = paste("Residuals Plot for", selected_model, "Model"),
         subtitle = paste("Station:", selected_station, "| Variable:", selected_var),
         y = "Residuals",
         x = "Time") +
    theme_light() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9))
  
  acf_plot <- fit %>%
    augment() %>%
    ACF(.innov) %>%
    autoplot() +
    labs(title = "ACF of Residuals") +
    theme_light()
  
  hist_plot <- fit %>%
    augment() %>%
    ggplot(aes(x = .innov)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(title = "Histogram of Residuals",
         x = "Residuals",
         y = "Count") +
    theme_light()
  
  combined_plot <- gridExtra::grid.arrange(
    residuals_plot, acf_plot, hist_plot,
    ncol = 1,
    heights = c(1, 1, 1)
  )
  
  return(combined_plot)
}

# Time Series Forecasting Model Comparison
create_forecast <- function(dataset_type,
                            selected_station,
                            selected_var,
                            training_start = "2020-01-01",
                            training_end = "2023-12-31",
                            holdout_end = "2024-12-31",
                            models = c("SES", "Holt", "Damped Holt", "Winter-Add", "Winter-Mult", "ARIMA")) {
  
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
  
  training_start_date <- as.Date(training_start)
  training_end_date <- as.Date(training_end)
  holdout_end_date <- as.Date(holdout_end)
  
  horizon <- length(seq(training_end_date, holdout_end_date, by = "month")) - 1
  
  message("Training period: ", training_start_date, " to ", training_end_date)
  message("Holdout period end: ", holdout_end_date)
  message("Forecast horizon (months): ", horizon)
  
  ts_data <- data %>%
    filter(Station == selected_station,
           date >= training_start_date,
           date <= holdout_end_date) %>%
    select(date, Station, !!sym(var_mapping[[dataset_type]][[selected_var]])) %>%
    rename(Value = !!sym(var_mapping[[dataset_type]][[selected_var]])) %>%
    mutate(
      year_month = yearmonth(date),
      Type = if_else(date <= training_end_date, "Training", "Hold-out")
    ) %>%
    group_by(year_month) %>%
    summarise(
      Value = mean(Value, na.rm = TRUE),
      Type = first(Type),
      .groups = 'drop'
    ) %>%
    as_tsibble(index = year_month) %>%
    fill_gaps()
  
  training_data <- ts_data %>%
    filter(Type == "Training")
  
  model_spec <- list()
  
  if ("SES" %in% models) {
    model_spec$SES <- ETS(Value ~ error("A") + trend("N") + season("N"))
  }
  if ("Holt" %in% models) {
    model_spec$Holt <- ETS(Value ~ error("A") + trend("A") + season("N"))
  }
  if ("Damped Holt" %in% models) {
    model_spec$`Damped Holt` <- ETS(Value ~ error("A") + trend("Ad") + season("N"))
  }
  if ("Winter-Add" %in% models) {
    model_spec$`Winter-Add` <- ETS(Value ~ error("A") + trend("A") + season("A"))
  }
  if ("Winter-Mult" %in% models) {
    model_spec$`Winter-Mult` <- ETS(Value ~ error("M") + trend("A") + season("M"))
  }
  if ("ARIMA" %in% models) {
    model_spec$ARIMA <- ARIMA(Value)
  }
  
  fit_models <- training_data %>%
    model(!!!model_spec)
  
  forecast_data <- fit_models %>%
    forecast(h = paste(horizon, "months"))
  
  base_plot <- forecast_data %>%
    autoplot(ts_data, level = NULL) +
    theme_light() +
    geom_vline(xintercept = yearmonth(training_end_date +days(1)), 
               linetype = "dashed", 
               color = "grey50") +
    labs(
      title = paste("Forecast for", selected_var, "at", selected_station),
      subtitle = paste("Training:", format(training_start_date, "%b %Y"),
                       "to", format(training_end_date, "%b %Y"),
                       "| Holdout:", format(holdout_end_date, "%b %Y")),
      x = "Period",
      y = paste(selected_var, "(", units[[dataset_type]], ")")
    ) +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9)
    )
  
  forecast_plot <- ggplotly(base_plot, tooltip = c("x", "y", ".model")) %>%
    layout(
      hoverlabel = list(bgcolor = "white"),
      showlegend = TRUE,
      legend = list(title = list(text = "Models"))
    )
  
  for(i in 1:length(forecast_plot$x$data)) {
    forecast_plot$x$data[[i]]$text <- format(as.Date(forecast_plot$x$data[[i]]$x), "%b %Y")
    forecast_plot$x$data[[i]]$hovertemplate <- paste(
      "%{text}<br>",
      "Value: %{y:.1f}", units[[dataset_type]], "<br>",
      "Model: ", forecast_plot$x$data[[i]]$name, "<br>",
      "<extra></extra>"
    )
  }
  
  accuracy_table <- fit_models %>%
    accuracy() %>%
    select(.model, ME, RMSE, MAE, MPE, MAPE, MASE, RMSSE) %>%
    rename(
      Model = .model,
      "Mean Error" = ME,
      "Root Mean Square Error" = RMSE,
      "Mean Absolute Error" = MAE,
      "Mean Percentage Error" = MPE,
      "Mean Absolute Percentage Error" = MAPE,
      "Mean Absolute Scaled Error" = MASE,
      "Root Mean Square Scaled Error" = RMSSE
    ) %>%
    kable(
      caption = "Model Accuracy Metrics",
      format = "html",
      digits = 2
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE
    )
  
  # Alternative approach with more explicit data handling
  forecast_table <- forecast_data %>%
    as_tibble() %>%
    group_by(.model) %>%
    mutate(
      `Forecast Period` = row_number()
    ) %>%
    ungroup() %>%
    mutate(
      Value = round(.mean, 2)
    ) %>%
    select(`Forecast Period`, Model = .model, Value) %>%
    tidyr::pivot_wider(
      names_from = Model,
      values_from = Value,
      id_cols = `Forecast Period`
    ) %>%
    kable(
      caption = paste("Forecast Values for", selected_var),
      format = "html",
      digits = 2,
      align = "c"  # Center-align all columns
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = TRUE,
      position = "center",
      font_size = 14
    ) 
  
  
  # Modify the return statement at the end of the function
  return(list(
    plot = forecast_plot,
    table = accuracy_table,
    forecast_table = forecast_table
  ))
}


# Multiple Time Series Forecasting
create_station_comparison <- function(dataset_type,
                                      selected_stations,
                                      selected_var,
                                      selected_model,
                                      training_start = "2020-01-01",
                                      training_end = "2023-12-31",
                                      holdout_end = "2024-12-31") {
  
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
  
  training_start_date <- as.Date(training_start)
  training_end_date <- as.Date(training_end)
  holdout_end_date <- as.Date(holdout_end)
  
  horizon <- length(seq(from = training_end_date, to = holdout_end_date, by = "month")) - 1
  
  ts_data <- data %>%
    filter(Station %in% selected_stations,
           date >= training_start_date,
           date <= holdout_end_date) %>%
    select(date, Station, !!sym(var_mapping[[dataset_type]][[selected_var]])) %>%
    rename(Value = !!sym(var_mapping[[dataset_type]][[selected_var]])) %>%
    mutate(
      year_month = yearmonth(date),
      Type = if_else(date <= training_end_date, "Training", "Hold-out")
    ) %>%
    group_by(year_month, Station, Type) %>%
    summarise(Value = mean(Value, na.rm = TRUE),
              .groups = 'drop') %>%
    as_tsibble(key = Station, index = year_month) %>%
    fill_gaps()
  
  training_data <- ts_data %>%
    filter(Type == "Training")
  
  model_spec <- switch(selected_model,
                       "SES" = ETS(Value ~ error("A") + trend("N") + season("N")),
                       "Holt" = ETS(Value ~ error("A") + trend("A") + season("N")),
                       "Damped Holt" = ETS(Value ~ error("A") + trend("Ad") + season("N")),
                       "Winter-Add" = ETS(Value ~ error("A") + trend("A") + season("A")),
                       "Winter-Mult" = ETS(Value ~ error("M") + trend("A") + season("M")),
                       "ARIMA" = ARIMA(Value),
                       stop("Invalid model type"))
  
  fit_models <- training_data %>%
    model(fcst = model_spec)
  
  forecasts <- fit_models %>%
    forecast(h = horizon)
  
  forecast_plot <- forecasts %>%
    autoplot(ts_data, level = NULL) +
    geom_vline(xintercept = as.numeric(as.Date(training_end_date)), 
               linetype = "dashed", 
               color = "grey50") +
    facet_wrap(~Station, scales = "free_y", ncol = 1) + 
    labs(
      title = paste("Forecast Comparison for", selected_var),
      subtitle = paste("Model:", selected_model,
                       "| Training:", format(training_start_date, "%b %Y"),
                       "to", format(training_end_date, "%b %Y"),
                       "| Holdout:", format(holdout_end_date, "%b %Y")),
      x = "Period", 
      y = paste(selected_var, "(", units[[dataset_type]], ")")
    ) +
    theme_light() +
    theme(
      strip.text = element_text(face = "bold"), 
      strip.background = element_rect(fill = "grey30"),
      panel.spacing = unit(1, "lines"),  
      legend.position = "right",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9)
    )
  
  plot_height <- max(
    length(selected_stations) * 150,  
    length(selected_stations) * 200   
  )
  
  interactive_plot <- ggplotly(forecast_plot, tooltip = "none") %>%
    layout(
      hoverlabel = list(bgcolor = "white"),
      showlegend = TRUE,
      legend = list(title = list(text = "Type")),
      height = plot_height, 
      autosize = TRUE   
    ) %>%
    config(responsive = TRUE)
  
  # Add custom hovertemplate for each trace
  for(i in seq_along(interactive_plot$x$data)) {
    interactive_plot$x$data[[i]]$text <- format(as.Date(interactive_plot$x$data[[i]]$x), "%b %Y")
    interactive_plot$x$data[[i]]$hovertemplate <- paste(
      "%{text}<br>",
      paste(selected_var, ": %{y:.1f}", units[[dataset_type]]),
      "<extra></extra>"
    )
  }
  
  accuracy_table <- fit_models %>%
    accuracy() %>%
    select(Station, ME, RMSE, MAE, MPE, MAPE, MASE, RMSSE) %>%
    rename(
      "Mean Error" = ME,
      "Root Mean Square Error" = RMSE,
      "Mean Absolute Error" = MAE,
      "Mean Percentage Error" = MPE,
      "Mean Absolute Percentage Error" = MAPE,
      "Mean Absolute Scaled Error" = MASE,
      "Root Mean Square Scaled Error" = RMSSE
    ) %>%
    kable(
      caption = "Model Accuracy Metrics by Station",
      format = "html",
      digits = 2
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE
    )
  
  forecast_table <- forecasts %>%
    as_tibble() %>%
    group_by(Station) %>%
    mutate(
      # Only include rows up to the horizon
      `Forecast Period` = row_number()
    ) %>%
    ungroup() %>%
    # Filter to ensure we only show periods up to our horizon
    filter(`Forecast Period` <= horizon) %>%
    mutate(
      Value = round(.mean, 2)
    ) %>%
    select(`Forecast Period`, Station, Value) %>%
    tidyr::pivot_wider(
      names_from = Station,
      values_from = Value,
      id_cols = `Forecast Period`
    ) %>%
    kable(
      caption = paste("Forecast Values for", selected_var,
                      "\nForecast Period:", format(training_end_date + months(1), "%b %Y"),
                      "to", format(holdout_end_date, "%b %Y")),
      format = "html",
      digits = 2,
      align = "c"
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = TRUE,
      position = "center",
      font_size = 14
    )
  # Modify the return statement to include the forecast table
  return(list(
    plot = interactive_plot,
    table = accuracy_table,
    forecast_table = forecast_table
  ))
}




# Bubble Plot - Rainfall
plot_rainfall_map <- function(data, selected_year, selected_month) {
  # Aggregate rainfall data
  aggregated_data <- data %>%
    mutate(
      Year = year(date), 
      Month = month(date, label = TRUE, abbr = FALSE)
    ) %>%
    group_by(Station, Year, Month) %>%
    summarise(
      Total_Rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    st_as_sf()
  
  # Filter data
  filtered_data <- aggregated_data %>%
    filter(Year == selected_year, 
           as.character(Month) == as.character(selected_month))
  
  # Create map
  tm_shape(filtered_data) +
    tm_bubbles(
      size = "Total_Rainfall",
      fill = "Total_Rainfall",
      fill.scale = tm_scale_continuous(values = "brewer.blues"),
      size.scale = tm_scale_continuous(values.scale = 2),
      id = "Station",
      popup.vars = c(
        "Station" = "Station",
        "Total Rainfall (mm)" = "Total_Rainfall"
      )
    ) +
    tm_title(text = paste("Total Rainfall for", selected_month, selected_year)) +
    tm_view(
      set_zoom_limits = c(11, 14),
      bbox = st_bbox(c(xmin = 103.6, xmax = 104.1, 
                       ymin = 1.2, ymax = 1.5))
    )
}


# Bubble Plot - Temperature
plot_temperature_map <- function(data, selected_year, selected_month) {
  # Aggregate temperature data
  aggregated_data <- data %>%
    mutate(
      Year = year(date), 
      Month = month(date, label = TRUE, abbr = FALSE)
    ) %>%
    group_by(Station, Year, Month) %>%
    summarise(
      Mean_Temperature = mean(`Mean Temperature (°C)`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    st_as_sf()
  
  # Filter data
  filtered_data <- aggregated_data %>%
    filter(Year == selected_year, 
           as.character(Month) == as.character(selected_month))
  
  # Create map
  tm_shape(filtered_data) +
    tm_bubbles(
      size = "Mean_Temperature",
      fill = "Mean_Temperature",
      fill.scale = tm_scale_continuous(values = "brewer.reds"),
      size.scale = tm_scale_continuous(values.scale = 2),
      id = "Station",
      popup.vars = c(
        "Station" = "Station",
        "Mean Temperature (°C)" = "Mean_Temperature"
      )
    ) +
    tm_title(text = paste("Mean Temperature for", selected_month, selected_year)) +
    tm_view(
      set_zoom_limits = c(11, 14),
      bbox = st_bbox(c(xmin = 103.6, xmax = 104.1, 
                       ymin = 1.2, ymax = 1.5))
    )
}

# Bubble Plot - Wind Speed
plot_windspeed_map <- function(data, selected_year, selected_month) {
  # Aggregate wind speed data
  aggregated_data <- data %>%
    mutate(
      Year = year(date), 
      Month = month(date, label = TRUE, abbr = FALSE)
    ) %>%
    group_by(Station, Year, Month) %>%
    summarise(
      Mean_Wind_Speed = mean(`Mean Wind Speed (km/h)`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    st_as_sf()
  
  # Filter data
  filtered_data <- aggregated_data %>%
    filter(Year == selected_year, 
           as.character(Month) == as.character(selected_month))
  
  # Create map
  tm_shape(filtered_data) +
    tm_bubbles(
      size = "Mean_Wind_Speed",
      fill = "Mean_Wind_Speed",
      fill.scale = tm_scale_continuous(values = "brewer.greens"),
      size.scale = tm_scale_continuous(values.scale = 2),
      id = "Station",
      popup.vars = c(
        "Station" = "Station",
        "Mean Wind Speed (km/h)" = "Mean_Wind_Speed"
      )
    ) +
    tm_title(text = paste("Mean Wind Speed for", selected_month, selected_year)) +
    tm_view(
      set_zoom_limits = c(11, 14),
      bbox = st_bbox(c(xmin = 103.6, xmax = 104.1, 
                       ymin = 1.2, ymax = 1.5))
    )
}




# Local Moran's I function for rainfall
localmoran_i_rainfall <- function(data, year, month, k_neighbors = 2) {
  data <- data %>%
    mutate(Year = year(date), Month = month(date, label = TRUE, abbr = FALSE)) %>%
    group_by(Station, Year, Month) %>%
    summarise(Total_Rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop")
  
  filtered_data <- data %>%
    filter(Year == year, Month == month)
  
  sf_data <- st_as_sf(filtered_data, coords = c("Longitude", "Latitude"), crs = 3414)
  
  coordinates <- st_coordinates(sf_data)
  coordinates <- as.data.frame(coordinates)
  coordinates[] <- lapply(coordinates, as.numeric)
  
  neighbors <- knearneigh(coordinates, k = k_neighbors)  
  weights <- nb2listw(knn2nb(neighbors), style = "W")  
  
  variable <- filtered_data$Total_Rainfall
  local_moran_result <- localmoran(variable, weights)
  
  filtered_data$Local_Moran_I <- local_moran_result[, 1]  
  
  sf_data$Local_Moran_I <- filtered_data$Local_Moran_I
  
  return(sf_data)
}

# Local Moran's I function for temperature
localmoran_i_temperature <- function(data, year, month, k_neighbors = 2) {
  data <- data %>%
    mutate(Year = year(date), Month = month(date, label = TRUE, abbr = FALSE)) %>%
    group_by(Station, Year, Month) %>%
    summarise(Mean_Temperature = mean(`Mean Temperature (°C)`, na.rm = TRUE), .groups = "drop")
  
  filtered_data <- data %>%
    filter(Year == year, Month == month)
  
  sf_data <- st_as_sf(filtered_data, coords = c("Longitude", "Latitude"), crs = 3414)
  
  coordinates <- st_coordinates(sf_data)
  coordinates <- as.data.frame(coordinates)
  coordinates[] <- lapply(coordinates, as.numeric)
  
  neighbors <- knearneigh(coordinates, k = k_neighbors)  
  weights <- nb2listw(knn2nb(neighbors), style = "W")  
  
  variable <- filtered_data$Mean_Temperature
  local_moran_result <- localmoran(variable, weights)
  
  filtered_data$Local_Moran_I <- local_moran_result[, 1]  
  
  sf_data$Local_Moran_I <- filtered_data$Local_Moran_I
  
  return(sf_data)
}

# Local Moran's I function for wind speed
localmoran_i_windspeed <- function(data, year, month, k_neighbors = 2) {
  data <- data %>%
    mutate(Year = year(date), Month = month(date, label = TRUE, abbr = FALSE)) %>%
    group_by(Station, Year, Month) %>%
    summarise(Mean_Wind_Speed = mean(`Mean Wind Speed (km/h)`, na.rm = TRUE), .groups = "drop")
  
  filtered_data <- data %>%
    filter(Year == year, Month == month)
  
  sf_data <- st_as_sf(filtered_data, coords = c("Longitude", "Latitude"), crs = 3414)
  
  coordinates <- st_coordinates(sf_data)
  coordinates <- as.data.frame(coordinates)
  coordinates[] <- lapply(coordinates, as.numeric)
  
  neighbors <- knearneigh(coordinates, k = k_neighbors)  
  weights <- nb2listw(knn2nb(neighbors), style = "W")  
  
  variable <- filtered_data$Mean_Wind_Speed
  local_moran_result <- localmoran(variable, weights)
  
  filtered_data$Local_Moran_I <- local_moran_result[, 1]  
  
  sf_data$Local_Moran_I <- filtered_data$Local_Moran_I
  
  return(sf_data)
}


# Plot function for Rainfall Moran's I
plot_rainfall_morani <- function(data, year, month, k_neighbors = 2) {
  tmap_mode("view")
  
  rainfall_morani <- tm_shape(data) +
    tm_symbols(
      size = "Total_Rainfall", 
      fill = "Local_Moran_I",
      size.scale = tm_scale_continuous(values.scale = 3),
      fill.scale = tm_scale_intervals(
        values = "brewer.blues",
        style = "jenks"
      ),
      id = "Station",
      popup.vars = c(
        "Station" = "Station",
        "Total Rainfall (mm)" = "Total_Rainfall",
        "Local Moran's I" = "Local_Moran_I"
      )
    ) +
    tm_title(text = paste("Local Indicators of Spatial Association for", month, year)) +
    tm_view(
      set_zoom_limits = c(11, 14),
      bbox = st_bbox(c(xmin = 103.6, xmax = 104.1, 
                       ymin = 1.2, ymax = 1.5))
    )
  
  return(rainfall_morani)
}

# Plot function for Temperature Moran's I
plot_temperature_morani <- function(data, year, month, k_neighbors = 2) {
  tmap_mode("view")
  
  temperature_morani <- tm_shape(data) +
    tm_symbols(
      size = "Mean_Temperature", 
      fill = "Local_Moran_I",
      size.scale = tm_scale_continuous(values.scale = 3),
      fill.scale = tm_scale_intervals(
        values = "brewer.reds",
        style = "jenks"
      ),
      id = "Station",
      popup.vars = c(
        "Station" = "Station",
        "Mean Temperature (°C)" = "Mean_Temperature",
        "Local Moran's I" = "Local_Moran_I"
      )
    ) +
    tm_title(text = paste("Local Indicators of Spatial Association for", month, year)) +
    tm_view(
      set_zoom_limits = c(11, 14),
      bbox = st_bbox(c(xmin = 103.6, xmax = 104.1, 
                       ymin = 1.2, ymax = 1.5))
    )
  
  return(temperature_morani)
}

# Plot function for Wind Speed Moran's I
plot_windspeed_morani <- function(data, year, month, k_neighbors = 2) {
  tmap_mode("view")
  
  windspeed_morani <- tm_shape(data) +
    tm_symbols(
      size = "Mean_Wind_Speed", 
      fill = "Local_Moran_I",
      size.scale = tm_scale_continuous(values.scale = 3),
      fill.scale = tm_scale_intervals(
        values = "brewer.greens",
        style = "jenks"
      ),
      id = "Station",
      popup.vars = c(
        "Station" = "Station",
        "Mean Wind Speed (km/h)" = "Mean_Wind_Speed",
        "Local Moran's I" = "Local_Moran_I"
      )
    ) +
    tm_title(text = paste("Local Indicators of Spatial Association for", month, year)) +
    tm_view(
      set_zoom_limits = c(11, 14),
      bbox = st_bbox(c(xmin = 103.6, xmax = 104.1, 
                       ymin = 1.2, ymax = 1.5))
    )
  
  return(windspeed_morani)
}






















# UI
ui <- navbarPage(
  title = div(
    style = "display: flex; align-items: center;",
    tags$img(src = "sun.png", height = "30px", style = "margin-right: 10px;"),
    "Weather Pulse"
  ),
  useShinyjs(),
  theme = bslib::bs_theme(version = 4),
  
  # Add custom CSS
  tags$head(
    tags$style(HTML("
        .checkbox {
            margin: 5px 0;
        }
        .checkbox label {
            display: flex;
            align-items: center;
            padding: 3px 0;
        }
        .checkbox input[type='checkbox'] {
            margin-right: 8px;
        }
        .checkbox label:hover {
            background-color: #f8f8f8;
        }
        
        .home-page {
            background-image: url('background.jpg');
            background-attachment: fixed;
            background-position: center;
            background-repeat: no-repeat;
            background-size: cover;
            min-height: 100vh;
            margin: 0;
            padding: 0;
            width: 100vw;              /* Full viewport width */
            margin-left: -15px;        /* Remove default padding */
            margin-right: -15px;       /* Remove default padding */
            margin-top: -20px;         /* Remove gap below navbar */
            overflow-x: hidden;        /* Prevent horizontal scroll */
        }

        /* Adjust the content wrapper */
        .content-wrapper {
            position: relative;
            z-index: 1;
            padding: 20px;
            min-height: 100vh;         /* Ensure full height */
        }

        /* Add this to ensure navbar stays on top */
        .navbar {
            position: relative;
            z-index: 2;
        }
        
        .welcome-container {
            background-color: white;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            height: 100%;
            padding: 25px;
        }
        /* Adjust the container heights */
        .coming-soon-container {
            background-color: white;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            min-height: calc((100vh - 80px) / 2);  /* Adjust height to fill space */
            padding: 25px;
            margin-bottom: 20px;
            display: flex;
            justify-content: center;
            align-items: center;
            font-size: 24px;
            color: #666;
        }
        
        /* Adjust the fluid page container */
        .container-fluid {
            padding-left: 0;
            padding-right: 0;
            width: 100%;
        }
        
        .feature-box {
            display: flex;
            align-items: center;
            background: linear-gradient(135deg, #f6f9fc 0%, #edf2f7 100%);
            border-radius: 10px;
            padding: 20px;
            margin: 15px 0;
            transition: transform 0.2s;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .feature-box:hover {
            transform: translateY(-5px);
        }
        .feature-icon {
            flex: 0 0 80px;
            text-align: center;
            margin-right: 20px;
        }
        .feature-content {
            flex: 1;
        }
        .feature-title {
            color: #2c5282;
            font-size: 1.2em;
            font-weight: bold;
            margin-bottom: 10px;
        }
        .feature-text {
            color: #4a5568;
            line-height: 1.5;
        }
    "))
  ),
  
  # Home panel
  tabPanel(
    div(style = "display: flex; align-items: center;",
        tags$img(src = "house.png", height = "15px", style = "margin-right: 5px;"),
        "Home"
    ),
    div(class = "home-page",
        div(class = "content-wrapper",
            fluidPage(
              class="container-fluid",
              div(style = "padding: 20px;",
                  fluidRow(
                    # Left container
                    column(6,
                           div(class = "welcome-container",
                               div(style = "text-align: center;",
                                   tags$img(src = "sun_header.png", height = "200px", style = "margin-bottom: 10px;")
                               ),
                               div(style = "text-align: center; margin-bottom: 30px;",
                                   h2("WELCOME TO", style = "margin-bottom: 0;"),
                                   h1("WEATHER PULSE", style = "margin-top: 0; color: #337ab7;")
                               ),
                               p(style = "text-align: justify; line-height: 1.6;",
                                 "Singapore's climate has been experiencing rising temperatures and increasing weather extremes driven by climate change and urbanisation. In 2024, Singapore experienced one of its hottest years on record, with temperatures exceeding long-term averages. These climate trends pose significant risks, including heat stress, water resource management challenges and urban planning concerns.",
                                 br(), br(),
                                 "Existing reports and tools offer real-time weather forecasts and historical comparisons using long-term averages. However, they lack interactive analysis tools that would allow for a deeper exploration of historical trends, spatial patterns and future projections.",
                                 br(), br(),
                                 "Weather Pulse was developed to address these gaps. It is an R Shiny Application that has the following key features:"
                               ),
                               # First feature box
                               div(class = "feature-box",
                                   div(class = "feature-icon",
                                       tags$img(src = "clock.png", height = "50px")
                                   ),
                                   div(class = "feature-content",
                                       div(class = "feature-title", "Time-Series Analysis"),
                                       div(class = "feature-text", 
                                           "Explore historical trends, seasonal patterns, and forecast future values using advanced time series modeling techniques. Compare different stations and analyze various climate variables through interactive visualizations.")
                                   )
                               ),
                               # Second feature box
                               div(class = "feature-box",
                                   div(class = "feature-icon",
                                       tags$img(src = "google-maps.png", height = "50px")
                                   ),
                                   div(class = "feature-content",
                                       div(class = "feature-title", "Geospatial Analysis"),
                                       div(class = "feature-text", 
                                           "Visualize spatial patterns and relationships across different weather stations in Singapore. Analyze geographical distributions of temperature, rainfall, and wind speed through interactive maps and spatial analytics.")
                                   )
                               ),
                               
                               div(style = "margin-top: 30px;",
                                   h4("Dataset Information", style = "text-align: center; margin-bottom: 20px;"),
                                   div(style = "display: flex; justify-content: space-between; gap: 20px;",
                                       # First note box
                                       div(style = "
                                      flex: 1;
                                      background: linear-gradient(135deg, #EBF5FB 0%, #D6EAF8 100%);
                                      border-radius: 10px;
                                      padding: 20px;
                                      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                                      border-left: 4px solid #3498DB;
                                      ",
                                           div(style = "
                                        font-size: 14px;
                                        color: #444;
                                        line-height: 1.5;
                                        ",
                                               "Official Climate Data from",
                                               tags$br(),
                                               tags$span(
                                                 "Meteorological Services Singapore",
                                                 style = "font-weight: bold; color: #2874A6; font-size: 16px;"
                                               ),
                                               tags$br(),
                                               tags$a(
                                                 href = "https://www.weather.gov.sg/climate-historical-daily/",
                                                 "Access Source →",
                                                 style = "color: #3498DB; font-size: 12px; margin-top: 5px; display: inline-block;"
                                               )
                                           )
                                       ),
                                       
                                       # Second note box
                                       div(style = "
                                      flex: 1;
                                      background: linear-gradient(135deg, #EBF5FB 0%, #D6EAF8 100%);
                                      border-radius: 10px;
                                      padding: 20px;
                                      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                                      border-left: 4px solid #3498DB;
                                      ",
                                           div(style = "
                                        font-size: 14px;
                                        color: #444;
                                        line-height: 1.5;
                                        ",
                                               "Coverage of",
                                               tags$br(),
                                               tags$span(
                                                 "44", 
                                                 style = "font-size: 24px; font-weight: bold; color: #2874A6;"
                                               ),
                                               " Weather Stations",
                                               tags$br(),
                                               tags$span(
                                                 "80,388", 
                                                 style = "font-size: 24px; font-weight: bold; color: #2874A6;"
                                               ),
                                               " Observations",
                                               tags$br(),
                                               tags$span(
                                                 "From 2020 to 2024",
                                                 style = "font-size: 12px; color: #5499C7; font-style: italic;"
                                               )
                                           )
                                       ),
                                       
                                       # Third note box
                                       div(style = "
                                      flex: 1;
                                      background: linear-gradient(135deg, #EBF5FB 0%, #D6EAF8 100%);
                                      border-radius: 10px;
                                      padding: 20px;
                                      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                                      border-left: 4px solid #3498DB;
                                      ",
                                           div(style = "
                                        font-size: 14px;
                                        color: #444;
                                        line-height: 1.5;
                                        ",
                                               "Key Weather Variables:",
                                               tags$br(),
                                               tags$div(style = "margin-top: 5px;",
                                                        tags$span(
                                                          "Temperature",
                                                          style = "color: #2874A6; font-weight: bold;"
                                                        ),
                                                        " • ",
                                                        tags$span(
                                                          "Rainfall",
                                                          style = "color: #2874A6; font-weight: bold;"
                                                        ),
                                                        " • ",
                                                        tags$span(
                                                          "Wind Speed",
                                                          style = "color: #2874A6; font-weight: bold;"
                                                        )
                                               ),
                                               tags$div(style = "
                                            font-size: 12px;
                                            color: #5499C7;
                                            margin-top: 8px;
                                            font-style: italic;
                                            ",
                                                        "Daily measurements available"
                                               )
                                           )
                                       )
                                   )
                               )
                               
                           )
                    ),
                    # Right containers
                    column(6,
                           # Top right container
                           div(class = "overview-container",
                               style = "background-color: white; border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); min-height: calc((100vh - 80px) / 2); padding: 25px; margin-bottom: 20px;",
                               h4("Overview of Climate Variables (2020-2024)", style = "text-align: center; margin-bottom: 20px;"),
                               plotOutput("overview_plot", height = "calc(100% - 40px)")
                           ),
                           # Bottom right container
                           div(class = "coming-soon-container",
                               style = "background-color: white; border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); padding: 25px; margin-bottom: 20px; display: flex; flex-direction: column;",
                               # Title at the top
                               h4("Weather Highlights", 
                                  style = "text-align: center; margin-bottom: 20px;"),
                               # Grid container for boxes
                               div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; width: 100%;",
                                   # First highlight box (Highest Temperature)
                                   div(style = "
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            overflow: hidden;
            width: 100%;
            ",
                                       # Top half with icon and value
                                       div(style = "
                padding: 15px;
                text-align: center;
                border-bottom: 1px solid rgba(229, 62, 62, 0.2);
                background-color: white;
                ",
                                           tags$img(src = "hot.png", height = "40px", 
                                                    style = "margin-bottom: 10px;"),
                                           div(
                                             textOutput("highest_temp"),
                                             style = "
                    font-size: 24px;
                    font-weight: bold;
                    color: #E53E3E;
                    margin-bottom: 5px;
                    "
                                           ),
                                           div(
                                             "Highest Temperature Recorded",
                                             style = "
                    font-size: 12px;
                    color: #666;
                    "
                                           )
                                       ),
                                       # Bottom half with details - now with solid background
                                       div(style = "
                padding: 15px;
                text-align: center;
                font-size: 12px;
                background-color: #FFF5F5;
                ",
                                           uiOutput("highest_temp_details")
                                       )
                                   ),
                                   # Placeholder boxes for other highlights
                                   div(style = "
    border-radius: 10px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    overflow: hidden;
    width: 100%;
    ",
                                       # Top half with icon and value
                                       div(style = "
        padding: 15px;
        text-align: center;
        border-bottom: 1px solid rgba(229, 62, 62, 0.2);
        background-color: white;
        ",
                                           tags$img(src = "fever.png", height = "40px", 
                                                    style = "margin-bottom: 10px;"),
                                           div(
                                             textOutput("temp_growth"),
                                             style = "
            font-size: 24px;
            font-weight: bold;
            color: #E53E3E;
            margin-bottom: 5px;
            "
                                           ),
                                           div(
                                             "Mean Temperature Growth",
                                             style = "
            font-size: 12px;
            color: #666;
            "
                                           )
                                       ),
                                       # Bottom half with details
                                       div(style = "
        padding: 15px;
        text-align: center;
        font-size: 12px;
        background-color: #FFF5F5;
        ",
                                           uiOutput("temp_growth_details")
                                       )
                                   ),
                                   div(style = "
    border-radius: 10px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    overflow: hidden;
    width: 100%;
    ",
                                       # Top half with icon and month
                                       div(style = "
        padding: 15px;
        text-align: center;
        border-bottom: 1px solid rgba(66, 153, 225, 0.2);
        background-color: white;
        ",
                                           tags$img(src = "water.png", height = "40px", 
                                                    style = "margin-bottom: 10px;"),
                                           div(
                                             textOutput("wettest_month_value"),
                                             style = "
            font-size: 24px;
            font-weight: bold;
            color: #2B6CB0;
            margin-bottom: 5px;
            "
                                           ),
                                           div(
                                             "Wettest Month on Record",
                                             style = "
            font-size: 12px;
            color: #666;
            "
                                           )
                                       ),
                                       # Bottom half with rainfall details
                                       div(style = "
        padding: 15px;
        text-align: center;
        font-size: 12px;
        background-color: #EBF8FF;
        ",
                                           uiOutput("wettest_month_details")
                                       )
                                   ),
                                   div(style = "
    border-radius: 10px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    overflow: hidden;
    width: 100%;
    ",
                                       # Top half with icon and month
                                       div(style = "
        padding: 15px;
        text-align: center;
        border-bottom: 1px solid rgba(66, 153, 225, 0.2);
        background-color: white;
        ",
                                           tags$img(src = "weather.png", height = "40px", 
                                                    style = "margin-bottom: 10px;"),
                                           div(
                                             textOutput("driest_month_value"),
                                             style = "
            font-size: 24px;
            font-weight: bold;
            color: #2B6CB0;
            margin-bottom: 5px;
            "
                                           ),
                                           div(
                                             "Driest Month on Record",
                                             style = "
            font-size: 12px;
            color: #666;
            "
                                           )
                                       ),
                                       # Bottom half with rainfall details
                                       div(style = "
        padding: 15px;
        text-align: center;
        font-size: 12px;
        background-color: #EBF8FF;
        ",
                                           uiOutput("driest_month_details")
                                       )
                                   )
                               )
                           )
                           
                           
                           
                    )
                  )
              )
            )
        )
    )
  ),
  
  # Time Series Analysis Menu
  navbarMenu(
    HTML(paste0(
      '<div style="display: inline-flex; align-items: center;">',
      '<img src="clock.png" height="15px" style="margin-right: 5px;">',
      'Time Series Analysis',
      '</div>'
    )),
    
    # Exploratory Data Analysis Panel
    tabPanel("Exploratory Data Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset_type", "Select Variable:",
                             choices = c("Temperature" = "temperature",
                                         "Rainfall" = "rainfall",
                                         "Wind Speed" = "windspeed")),
                 
                 selectInput("var_type", "Select Metric:",
                             choices = NULL),
                 
                 # Controls for different visualizations
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
                 
                 conditionalPanel(
                   condition = "input.viz_type == 'Calendar Heatmap'",
                   selectInput("heatmap_station", "Select Station:",
                               choices = NULL),
                   selectInput("selected_year", "Select Year:",
                               choices = NULL)
                 ),
                 
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
                 
                 conditionalPanel(
                   condition = "input.viz_type == 'STL Decomposition'",
                   selectInput("decomp_station", "Select Station:",
                               choices = NULL),
                   dateRangeInput("decomp_date_range", "Select Date Range:",
                                  start = "2020-01-01",
                                  end = "2023-12-31",
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
    
    # Time Series Forecasting Panel
    tabPanel("Time Series Forecasting",
             sidebarLayout(
               sidebarPanel(
                 # Model Diagnostics inputs
                 conditionalPanel(
                   condition = "input.forecast_tabs == 'Model Diagnostics'",
                   selectInput("diag_dataset_type", "Select Variable:",
                               choices = c("Temperature" = "temperature",
                                           "Rainfall" = "rainfall",
                                           "Wind Speed" = "windspeed")),
                   
                   selectInput("diag_var_type", "Select Metric:",
                               choices = NULL),
                   
                   selectInput("diag_station", "Select Station:",
                               choices = NULL),
                   
                   selectInput("diag_model", "Select Model:",
                               choices = c("SES" = "SES",
                                           "Holt" = "Holt",
                                           "Damped Holt" = "Damped Holt",
                                           "Winter-Add" = "Winter-Add",
                                           "Winter-Mult" = "Winter-Mult",
                                           "ARIMA" = "ARIMA")),
                   
                   dateRangeInput("diag_date_range", "Select Training Period:",
                                  start = "2020-01-01",
                                  end = "2023-12-31",
                                  min = "2020-01-01",
                                  max = "2024-12-31")
                 ),
                 
                 # Forecast Model Comparison inputs
                 conditionalPanel(
                   condition = "input.forecast_tabs == 'Forecast Model Comparison'",
                   selectInput("forecast_dataset_type", "Select Variable:",
                               choices = c("Temperature" = "temperature",
                                           "Rainfall" = "rainfall",
                                           "Wind Speed" = "windspeed")),
                   
                   selectInput("forecast_var_type", "Select Metric:",
                               choices = NULL),
                   
                   selectInput("forecast_station", "Select Station:",
                               choices = NULL),
                   
                   dateRangeInput("training_period", "Select Training Period:",
                                  start = "2020-01-01",
                                  end = "2023-12-31",
                                  min = "2020-01-01",
                                  max = "2024-12-31"),
                   
                   dateInput("holdout_end", "Select Forecast End Date:",
                             value = "2024-12-31",
                             min = "2023-12-31",
                             max = "2025-12-31"),
                   
                   checkboxGroupInput("selected_models", "Select Models:",
                                      choices = c("SES", "Holt", "Damped Holt", 
                                                  "Winter-Add", "Winter-Mult", "ARIMA"),
                                      selected = c("SES", "Holt", "ARIMA"))
                 ),
                 
                 # Station Comparison inputs
                 conditionalPanel(
                   condition = "input.forecast_tabs == 'Station Forecast Comparison'",
                   selectInput("compare_dataset_type", "Select Variable:",
                               choices = c("Temperature" = "temperature",
                                           "Rainfall" = "rainfall",
                                           "Wind Speed" = "windspeed")),
                   
                   selectInput("compare_var_type", "Select Metric:",
                               choices = NULL),
                   
                   selectInput("compare_model", "Select Model:",
                               choices = c("SES" = "SES",
                                           "Holt" = "Holt",
                                           "Damped Holt" = "Damped Holt",
                                           "Winter-Add" = "Winter-Add",
                                           "Winter-Mult" = "Winter-Mult",
                                           "ARIMA" = "ARIMA")),
                   
                   div(style = "margin-bottom: 15px;",
                       tags$label("Select Stations:"),
                       div(style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: white;",
                           checkboxGroupInput("compare_stations", 
                                              label = NULL,
                                              choices = NULL)
                       )
                   ),
                   
                   dateRangeInput("compare_training_period", "Select Training Period:",
                                  start = "2020-01-01",
                                  end = "2023-12-31",
                                  min = "2020-01-01",
                                  max = "2024-12-31"),
                   
                   dateInput("compare_holdout_end", "Select Forecast End Date:",
                             value = "2024-12-31",
                             min = "2023-12-31",
                             max = "2025-12-31")
                 ),
                 
                 width = 3
               ),
               mainPanel(
                 tabsetPanel(
                   id = "forecast_tabs",
                   tabPanel("Model Diagnostics",
                            plotOutput("diagnostics_plot", height = "800px")
                   ),
                   tabPanel("Forecast Model Comparison",
                            # Add links for switching views
                            div(
                              style = "margin-bottom: 15px;",
                              actionLink(
                                "show_forecast_plot_link",
                                "Show Plot",
                                style = "margin-right: 15px; color: #337ab7; text-decoration: underline;"
                              ),
                              actionLink(
                                "show_forecast_metrics_link",
                                "Show Accuracy Metrics",
                                style = "margin-right: 15px; color: #337ab7; text-decoration: underline;"
                              ),
                              actionLink(
                                "show_forecast_results_link",
                                "Show Forecast Results",
                                style = "color: #337ab7; text-decoration: underline;"
                              )
                            ),
                            # Container for plot
                            div(
                              id = "forecast_plot_container",
                              plotlyOutput("forecast_plot")
                            ),
                            # Container for metrics table
                            div(
                              id = "forecast_metrics_container",
                              style = "display: none;",  # Initially hidden
                              htmlOutput("accuracy_table")
                            ),
                            # Container for forecast results table
                            div(
                              id = "forecast_results_container",
                              style = "display: none; width: 100%; padding: 15px;",  # Added width and padding
                              htmlOutput("forecast_results_table")
                            )
                   ),
                   tabPanel("Station Forecast Comparison",
                            # Links div
                            div(
                              style = "margin-bottom: 15px;",
                              actionLink(
                                "show_plot_link",
                                "Show Plot",
                                style = "margin-right: 15px; color: #337ab7; text-decoration: underline;"
                              ),
                              actionLink(
                                "show_metrics_link",
                                "Show Accuracy Metrics",
                                style = "margin-right: 15px; color: #337ab7; text-decoration: underline;"
                              ),
                              actionLink(
                                "show_station_forecast_link",
                                "Show Forecast Results",
                                style = "color: #337ab7; text-decoration: underline;"
                              )
                            ),
                            # Plot container
                            div(
                              id = "comparison_plot_container",
                              plotlyOutput("comparison_plot")
                            ),
                            # Metrics container
                            div(
                              id = "comparison_metrics_container",
                              style = "display: none;",  # Initially hidden
                              htmlOutput("comparison_table")
                            ),
                            # Forecast results container
                            div(
                              id = "station_forecast_container",
                              style = "display: none;",  # Initially hidden
                              htmlOutput("station_forecast_table")
                            )
                   )
                 ),
                 width = 9
               )
             )
    )
  ),
  
  # Geospatial Analysis Menu
  navbarMenu(
    HTML(paste0(
      '<div style="display: inline-flex; align-items: center;">',
      '<img src="google-maps.png" height="15px" style="margin-right: 5px;">',
      'Geospatial Analysis',
      '</div>'
    )),
    
    tabPanel("Exploratory Data Analysis",
             sidebarLayout(
               sidebarPanel(
                 # Year selection
                 selectInput("bubble_year", 
                             "Select Year",
                             choices = 2020:2024,
                             selected = 2024),
                 
                 # Month selection
                 selectInput("bubble_month",
                             "Select Month",
                             choices = month.name,
                             selected = month.name[1]),
                 width = 3
               ),
               mainPanel(
                 tabsetPanel(
                   id = "bubble_plot_tabs",
                   
                   # Rainfall Tab
                   tabPanel("Bubble Plot - Rainfall",
                            tmapOutput("rainfall_bubble_map", height = "600px")
                   ),
                   
                   # Temperature Tab
                   tabPanel("Bubble Plot - Temperature",
                            tmapOutput("temperature_bubble_map", height = "600px")
                   ),
                   
                   # Wind Speed Tab
                   tabPanel("Bubble Plot - Wind Speed",
                            tmapOutput("windspeed_bubble_map", height = "600px")
                   )
                 ),
                 width = 9
               )
             )
    ),
    
    # Replace the existing Spatial Autocorrelation tabPanel with:
    tabPanel("Spatial Autocorrelation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("morani_year", 
                             "Select Year",
                             choices = 2020:2024,
                             selected = 2024),
                 
                 selectInput("morani_month",
                             "Select Month",
                             choices = month.name,
                             selected = month.name[1]),
                 
                 # In your UI code, update the sliderInput for k_neighbors:
                 sliderInput("k_neighbors",
                             "Number of Nearest Neighbors:",
                             min = 1,
                             max = 5,  # Reduced from 10 to 5
                             value = 2,
                             step = 1),
                 width = 3
               ),
               mainPanel(
                 tabsetPanel(
                   id = "morani_tabs",
                   
                   tabPanel("Rainfall",
                            tmapOutput("rainfall_morani_map", height = "600px")),
                   
                   tabPanel("Temperature",
                            tmapOutput("temperature_morani_map", height = "600px")),
                   
                   tabPanel("Wind Speed",
                            tmapOutput("windspeed_morani_map", height = "600px"))
                 ),
                 width = 9
               )
             ))
    
  )
)






























# Server
server <- function(input, output, session) {
  
  # Initialize tmap settings at startup
  tmap_mode("view")
  
  # Ensure data is properly loaded with CRS
  climate_rainfall_sf <- reactive({
    req(climate_rainfall_geospatial)
    if (!inherits(climate_rainfall_geospatial, "sf")) {
      stop("Data must be an sf object")
    }
    if (is.na(st_crs(climate_rainfall_geospatial))) {
      climate_rainfall_geospatial <- st_set_crs(climate_rainfall_geospatial, 3414)
    }
    return(climate_rainfall_geospatial)
  })
  
  
  # Reactive dataset based on selection
  selected_data <- reactive({
    switch(input$dataset_type,
           "temperature" = climate_temperature_interpolated,
           "rainfall" = climate_rainfall_interpolated,
           "windspeed" = climate_windspeed_interpolated)
  })
  
  # Calculate highest temperature
  highest_temp_data <- reactive({
    climate_temperature_interpolated %>%
      arrange(desc(`Maximum Temperature (°C)`)) %>%
      slice(1) %>%
      select(Station, date, `Maximum Temperature (°C)`)
  })
  
  # Render highest temperature value
  output$highest_temp <- renderText({
    temp_data <- highest_temp_data()
    paste0(round(temp_data$`Maximum Temperature (°C)`, 1), "°C")
  })
  
  # Render highest temperature details
  output$highest_temp_details <- renderUI({
    temp_data <- highest_temp_data()
    tagList(
      div(temp_data$Station,
          style = "margin-bottom: 5px; color: #C53030;"), # Darker red for better readability
      div(format(temp_data$date, "%d %b %Y"),
          style = "color: #E53E3E;")
    )
  })
  
  # Calculate temperature growth
  temp_growth_data <- reactive({
    climate_temperature_interpolated %>%
      mutate(year = year(date)) %>%
      group_by(year) %>%
      summarise(mean_temp = mean(`Mean Temperature (°C)`, na.rm = TRUE)) %>%
      filter(year %in% c(2020, 2024)) %>%
      summarise(
        start_temp = first(mean_temp),
        end_temp = last(mean_temp),
        growth_pct = ((end_temp - start_temp) / start_temp) * 100
      )
  })
  
  # Render temperature growth value
  output$temp_growth <- renderText({
    growth_data <- temp_growth_data()
    sprintf("%.1f%%", growth_data$growth_pct)
  })
  
  # Render temperature growth details
  output$temp_growth_details <- renderUI({
    growth_data <- temp_growth_data()
    tagList(
      div(
        sprintf("%.1f°C → %.1f°C", growth_data$start_temp, growth_data$end_temp),
        style = "margin-bottom: 5px; color: #C53030;"
      ),
      div(
        "2020 to 2024",
        style = "color: #E53E3E;"
      )
    )
  })
  
  # Calculate wettest month data
  wettest_month_data <- reactive({
    climate_rainfall_interpolated %>%
      mutate(
        year_month = floor_date(date, "month"),
        month_name = format(date, "%B %Y")  # Changed from %b to %B for full month name
      ) %>%
      group_by(year_month, month_name) %>%
      summarise(
        total_rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(total_rainfall)) %>%
      slice(1)
  })
  
  # Render wettest month value
  output$wettest_month_value <- renderText({
    rain_data <- wettest_month_data()
    rain_data$month_name
  })
  
  # Render wettest month details (now shows rainfall with comma)
  output$wettest_month_details <- renderUI({
    rain_data <- wettest_month_data()
    tagList(
      div(
        "Total Rainfall",
        style = "margin-bottom: 5px; color: #2B6CB0;"
      ),
      div(
        format(round(rain_data$total_rainfall, 1), big.mark = ",", scientific = FALSE),
        style = "color: #4299E1; font-size: 18px; font-weight: bold;"
      ),
      div(
        "millimeters",
        style = "color: #4299E1; font-size: 12px; margin-top: 2px;"
      )
    )
  })
  
  driest_month_data <- reactive({
    climate_rainfall_interpolated %>%
      mutate(
        year_month = floor_date(date, "month"),
        month_name = format(date, "%B %Y")
      ) %>%
      group_by(year_month, month_name) %>%
      summarise(
        total_rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(total_rainfall) %>%  # Changed from desc to get the minimum
      slice(1)
  })
  
  # Add the output renderers
  output$driest_month_value <- renderText({
    rain_data <- driest_month_data()
    rain_data$month_name
  })
  
  output$driest_month_details <- renderUI({
    rain_data <- driest_month_data()
    tagList(
      div(
        "Total Rainfall",
        style = "margin-bottom: 5px; color: #2B6CB0;"
      ),
      div(
        format(round(rain_data$total_rainfall, 1), big.mark = ",", scientific = FALSE),
        style = "color: #4299E1; font-size: 18px; font-weight: bold;"
      ),
      div(
        "millimeters",
        style = "color: #4299E1; font-size: 12px; margin-top: 2px;"
      )
    )
  })
  
  
  # Update variable choices for EDA
  observe({
    var_choices <- switch(input$dataset_type,
                          "temperature" = c(
                            "Mean Temperature" = "Mean Temperature",
                            "Maximum Temperature" = "Maximum Temperature",
                            "Minimum Temperature" = "Minimum Temperature"
                          ),
                          "rainfall" = c(
                            "Total Rainfall" = "Total Rainfall",
                            "Highest 30 Min Rainfall" = "Highest 30 Min Rainfall"
                          ),
                          "windspeed" = c(
                            "Mean Wind Speed" = "Mean Wind Speed",
                            "Max Wind Speed" = "Max Wind Speed"
                          )
    )
    updateSelectInput(session, "var_type", choices = var_choices)
  })
  
  # Update station choices for all visualizations
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
  
  # Update year choices for calendar heatmap
  observe({
    req(input$dataset_type)
    years <- sort(unique(year(selected_data()$date)))
    updateSelectInput(session, "selected_year",
                      choices = years,
                      selected = max(years))
  })
  
  # Line Chart Output
  output$line_chart <- renderPlotly({
    req(input$dataset_type,
        input$var_type,
        input$selected_stations,
        input$date_range)
    
    tryCatch({
      create_line_chart(
        data = selected_data(),
        selected_stations = input$selected_stations,
        dataset_type = input$dataset_type,
        var_type = input$var_type,
        date_range = input$date_range
      )
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(
          text = paste("Error:", e$message),
          showarrow = FALSE,
          font = list(size = 14)
        )
    })
  })
  
  # Calendar Heatmap Output
  output$calendar_heatmap <- renderPlotly({
    req(input$dataset_type,
        input$var_type,
        input$heatmap_station,
        input$selected_year)
    
    tryCatch({
      create_calendar_heatmap(
        data = selected_data(),
        selected_station = input$heatmap_station,
        selected_year = input$selected_year,
        dataset_type = input$dataset_type,
        var_type = input$var_type
      )
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(
          text = paste("Error:", e$message),
          showarrow = FALSE,
          font = list(size = 14)
        )
    })
  })
  
  # Sunburst Plot Output
  output$sunburst_plot <- renderPlotly({
    req(input$dataset_type,
        input$var_type,
        input$sunburst_station,
        input$year_range)
    
    tryCatch({
      create_sunburst(
        data = selected_data(),
        selected_station = input$sunburst_station,
        dataset_type = input$dataset_type,
        var_type = input$var_type,
        year_range = input$year_range
      )
    }, error = function(e) {
      plot_ly() %>%
        add_annotations(
          text = paste("Error:", e$message),
          showarrow = FALSE,
          font = list(size = 14)
        )
    })
  })
  
  # STL Decomposition Output
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
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Error:", e$message),
                 size = 5) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    })
  })
  
  
  # Overview plot output
  output$overview_plot <- renderPlot({
    create_overview_plot()
  }, height = 600)
  
  
  # Update variable choices for diagnostics
  observe({
    var_choices <- switch(input$diag_dataset_type,
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
    updateSelectInput(session, "diag_var_type", choices = var_choices)
  })
  
  # Update station choices for diagnostics
  observe({
    req(input$diag_dataset_type)
    data <- switch(input$diag_dataset_type,
                   "temperature" = climate_temperature_interpolated,
                   "rainfall" = climate_rainfall_interpolated,
                   "windspeed" = climate_windspeed_interpolated)
    
    stations <- sort(unique(data$Station))
    updateSelectInput(session, "diag_station",
                      choices = stations,
                      selected = stations[1])
  })
  
  # Output for diagnostics plot
  output$diagnostics_plot <- renderPlot({
    req(input$diag_dataset_type,
        input$diag_var_type,
        input$diag_station,
        input$diag_model,
        input$diag_date_range)
    
    tryCatch({
      create_model_diagnostics(
        dataset_type = input$diag_dataset_type,
        selected_station = input$diag_station,
        selected_var = input$diag_var_type,
        selected_model = input$diag_model,
        training_start = input$diag_date_range[1],
        training_end = input$diag_date_range[2]
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
  }, height = function() {
    800  # Fixed height for the plot
  })
  
  # Update variable choices for forecast comparison
  observe({
    var_choices <- switch(input$forecast_dataset_type,
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
    updateSelectInput(session, "forecast_var_type", choices = var_choices)
  })
  
  
  
  # Update station choices for forecast comparison
  observe({
    req(input$forecast_dataset_type)
    data <- switch(input$forecast_dataset_type,
                   "temperature" = climate_temperature_interpolated,
                   "rainfall" = climate_rainfall_interpolated,
                   "windspeed" = climate_windspeed_interpolated)
    
    stations <- sort(unique(data$Station))
    updateSelectInput(session, "forecast_station",
                      choices = stations,
                      selected = stations[1])
  })
  
  # Update variable choices for station comparison
  observe({
    var_choices <- switch(input$compare_dataset_type,
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
    updateSelectInput(session, "compare_var_type", choices = var_choices)
  })
  
  # Update station choices for comparison
  observe({
    req(input$compare_dataset_type)
    data <- switch(input$compare_dataset_type,
                   "temperature" = climate_temperature_interpolated,
                   "rainfall" = climate_rainfall_interpolated,
                   "windspeed" = climate_windspeed_interpolated)
    
    stations <- sort(unique(data$Station))
    updateCheckboxGroupInput(session, "compare_stations",
                             choices = stations,
                             selected = stations[1:2])
  })
  
  # Generate forecast comparison outputs
  forecast_results <- reactive({
    req(input$forecast_dataset_type,
        input$forecast_var_type,
        input$forecast_station,
        input$training_period,
        input$holdout_end,
        input$selected_models)
    
    tryCatch({
      create_forecast(
        dataset_type = input$forecast_dataset_type,
        selected_station = input$forecast_station,
        selected_var = input$forecast_var_type,
        training_start = input$training_period[1],
        training_end = input$training_period[2],
        holdout_end = input$holdout_end,
        models = input$selected_models
      )
    }, error = function(e) {
      list(
        plot = plot_ly() %>%
          add_annotations(
            text = paste("Error:", e$message),
            showarrow = FALSE,
            font = list(size = 14)
          ),
        table = paste("Error:", e$message),
        forecast_table = paste("Error:", e$message)
      )
    })
  })
  
  # Generate station comparison outputs
  comparison_results <- reactive({
    req(input$compare_dataset_type,
        input$compare_var_type,
        input$compare_stations,
        input$compare_model,
        input$compare_training_period,
        input$compare_holdout_end,
        length(input$compare_stations) >= 1)
    
    tryCatch({
      create_station_comparison(
        dataset_type = input$compare_dataset_type,
        selected_stations = input$compare_stations,
        selected_var = input$compare_var_type,
        selected_model = input$compare_model,
        training_start = input$compare_training_period[1],
        training_end = input$compare_training_period[2],
        holdout_end = input$compare_holdout_end
      )
    }, error = function(e) {
      list(
        plot = plot_ly() %>%
          add_annotations(
            text = paste("Error:", e$message),
            showarrow = FALSE,
            font = list(size = 14)
          ),
        table = paste("Error:", e$message)
      )
    })
  })
  
  # Render forecast plot
  output$forecast_plot <- renderPlotly({
    req(forecast_results())
    forecast_results()$plot
  })
  
  # Render forecast accuracy table
  output$accuracy_table <- renderUI({
    req(forecast_results())
    HTML(forecast_results()$table)
  })
  
  # Render forecast results table
  output$forecast_results_table <- renderUI({
    req(forecast_results())
    div(
      style = "width: 100%; margin: auto;",  # Center the table and use full width
      HTML(forecast_results()$forecast_table)
    )
  })
  
  # Render comparison plot
  output$comparison_plot <- renderPlotly({
    req(comparison_results())
    n_stations <- length(input$compare_stations)
    plot_height <- max(n_stations * 200, 400)  # Minimum height of 400px
    
    comparison_results()$plot %>%
      layout(
        height = plot_height,
        autosize = TRUE
      )
  })
  
  # Render comparison accuracy table
  output$comparison_table <- renderUI({
    req(comparison_results())
    div(
      style = "overflow-x: auto;",  # Makes table scrollable if too wide
      HTML(comparison_results()$table)
    )
  })
  
  # Render station forecast table
  output$station_forecast_table <- renderUI({
    req(comparison_results())
    div(
      style = "width: 100%; margin: auto;",
      HTML(comparison_results()$forecast_table)
    )
  })
  
  # Add observers for the forecast comparison link clicks
  observeEvent(input$show_forecast_plot_link, {
    shinyjs::show("forecast_plot_container")
    shinyjs::hide("forecast_metrics_container")
    shinyjs::hide("forecast_results_container")
    shinyjs::runjs("
      document.getElementById('show_forecast_plot_link').style.fontWeight = 'bold';
      document.getElementById('show_forecast_metrics_link').style.fontWeight = 'normal';
      document.getElementById('show_forecast_results_link').style.fontWeight = 'normal';
    ")
  })
  
  observeEvent(input$show_forecast_metrics_link, {
    shinyjs::hide("forecast_plot_container")
    shinyjs::show("forecast_metrics_container")
    shinyjs::hide("forecast_results_container")
    shinyjs::runjs("
      document.getElementById('show_forecast_plot_link').style.fontWeight = 'normal';
      document.getElementById('show_forecast_metrics_link').style.fontWeight = 'bold';
      document.getElementById('show_forecast_results_link').style.fontWeight = 'normal';
    ")
  })
  
  observeEvent(input$show_forecast_results_link, {
    shinyjs::hide("forecast_plot_container")
    shinyjs::hide("forecast_metrics_container")
    shinyjs::show("forecast_results_container")
    shinyjs::runjs("
      document.getElementById('show_forecast_plot_link').style.fontWeight = 'normal';
      document.getElementById('show_forecast_metrics_link').style.fontWeight = 'normal';
      document.getElementById('show_forecast_results_link').style.fontWeight = 'bold';
    ")
  })
  
  # Initialize the forecast comparison view (show plot by default)
  observe({
    req(input$forecast_tabs == "Forecast Model Comparison")
    shinyjs::show("forecast_plot_container")
    shinyjs::hide("forecast_metrics_container")
    shinyjs::hide("forecast_results_container")
    shinyjs::runjs("
      document.getElementById('show_forecast_plot_link').style.fontWeight = 'bold';
      document.getElementById('show_forecast_metrics_link').style.fontWeight = 'normal';
      document.getElementById('show_forecast_results_link').style.fontWeight = 'normal';
    ")
  })
  
  # Add observers for the station comparison link clicks
  observeEvent(input$show_plot_link, {
    shinyjs::show("comparison_plot_container")
    shinyjs::hide("comparison_metrics_container")
    shinyjs::hide("station_forecast_container")  # Make sure this is hidden
    shinyjs::runjs("
      document.getElementById('show_plot_link').style.fontWeight = 'bold';
      document.getElementById('show_metrics_link').style.fontWeight = 'normal';
      document.getElementById('show_station_forecast_link').style.fontWeight = 'normal';
    ")
  })
  
  # Observer for metrics link
  observeEvent(input$show_metrics_link, {
    shinyjs::hide("comparison_plot_container")
    shinyjs::show("comparison_metrics_container")
    shinyjs::hide("station_forecast_container")  # Make sure this is hidden
    shinyjs::runjs("
      document.getElementById('show_plot_link').style.fontWeight = 'normal';
      document.getElementById('show_metrics_link').style.fontWeight = 'bold';
      document.getElementById('show_station_forecast_link').style.fontWeight = 'normal';
    ")
  })
  
  # Observer for forecast results link
  observeEvent(input$show_station_forecast_link, {
    shinyjs::hide("comparison_plot_container")
    shinyjs::hide("comparison_metrics_container")
    shinyjs::show("station_forecast_container")
    shinyjs::runjs("
      document.getElementById('show_plot_link').style.fontWeight = 'normal';
      document.getElementById('show_metrics_link').style.fontWeight = 'normal';
      document.getElementById('show_station_forecast_link').style.fontWeight = 'bold';
    ")
  })
  
  # Initialize the view (when the tab is loaded)
  observe({
    req(input$forecast_tabs == "Station Forecast Comparison")
    # Show plot by default, hide others
    shinyjs::show("comparison_plot_container")
    shinyjs::hide("comparison_metrics_container")
    shinyjs::hide("station_forecast_container")
    shinyjs::runjs("
      document.getElementById('show_plot_link').style.fontWeight = 'bold';
      document.getElementById('show_metrics_link').style.fontWeight = 'normal';
      document.getElementById('show_station_forecast_link').style.fontWeight = 'normal';
    ")
  })
  
  # Render the bubble map
  output$rainfall_bubble_map <- renderTmap({
    req(input$bubble_year, input$bubble_month)
    req(climate_rainfall_sf())
    
    tryCatch({
      withProgress(message = 'Creating map...', {
        plot_rainfall_map(
          data = climate_rainfall_sf(),
          selected_year = input$bubble_year,
          selected_month = input$bubble_month
        )
      })
    }, error = function(e) {
      message("Error in renderTmap: ", e$message)
      # Return a minimal valid tmap object
      tm_shape(st_sf(geometry = st_sfc())) +
        tm_text("Error creating map. Please check the console for details.")
    })
  })
  
  # Update month choices based on available data
  observe({
    req(input$bubble_year)
    available_months <- climate_rainfall_geospatial %>%
      filter(year(date) == input$bubble_year) %>%
      pull(date) %>%
      month(label = TRUE, abbr = FALSE) %>%
      unique()
    
    updateSelectInput(session, "bubble_month",
                      choices = available_months,
                      selected = available_months[1])
  })
  
  # Update year choices based on available data
  observe({
    available_years <- unique(year(climate_rainfall_geospatial$date))
    updateSelectInput(session, "bubble_year",
                      choices = available_years,
                      selected = max(available_years))
  })
  
  # Render the temperature bubble map
  output$temperature_bubble_map <- renderTmap({
    req(input$bubble_year, input$bubble_month)
    
    withProgress(message = 'Creating map...', {
      plot_temperature_map(
        data = climate_temperature_geospatial,
        selected_year = input$bubble_year,
        selected_month = input$bubble_month
      )
    })
  })
  
  # Render the wind speed bubble map
  output$windspeed_bubble_map <- renderTmap({
    req(input$bubble_year, input$bubble_month)
    
    withProgress(message = 'Creating map...', {
      plot_windspeed_map(
        data = climate_windspeed_geospatial,
        selected_year = input$bubble_year,
        selected_month = input$bubble_month
      )
    })
  })
  
  # Rainfall Moran's I Map
  output$rainfall_morani_map <- renderTmap({
    req(input$morani_year, input$morani_month)
    
    withProgress(message = 'Calculating Local Moran\'s I...', {
      morani_data <- localmoran_i_rainfall(
        data = climate_rainfall_geospatial,
        year = input$morani_year,
        month = input$morani_month,
        k_neighbors = input$k_neighbors
      )
      
      plot_rainfall_morani(
        data = morani_data,
        year = input$morani_year,
        month = input$morani_month,
        k_neighbors = input$k_neighbors
      )
    })
  })
  
  # Temperature Moran's I Map
  output$temperature_morani_map <- renderTmap({
    req(input$morani_year, input$morani_month)
    
    withProgress(message = 'Calculating Local Moran\'s I...', {
      morani_data <- localmoran_i_temperature(
        data = climate_temperature_geospatial,
        year = input$morani_year,
        month = input$morani_month,
        k_neighbors = input$k_neighbors
      )
      
      plot_temperature_morani(
        data = morani_data,
        year = input$morani_year,
        month = input$morani_month,
        k_neighbors = input$k_neighbors
      )
    })
  })
  
  # Wind Speed Moran's I Map
  output$windspeed_morani_map <- renderTmap({
    req(input$morani_year, input$morani_month)
    
    withProgress(message = 'Calculating Local Moran\'s I...', {
      morani_data <- localmoran_i_windspeed(
        data = climate_windspeed_geospatial,
        year = input$morani_year,
        month = input$morani_month,
        k_neighbors = input$k_neighbors
      )
      
      plot_windspeed_morani(
        data = morani_data,
        year = input$morani_year,
        month = input$morani_month,
        k_neighbors = input$k_neighbors
      )
    })
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)