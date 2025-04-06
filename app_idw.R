library(shiny)
library(shinyjs)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(readr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(tmap)
library(tmaptools)
library(leaflet)
library(sf)
library(gstat)


# Load datasets
climate_temperature_interpolated <- read_csv("data/climate_temperature_interpolated.csv")
climate_rainfall_interpolated <- read_csv("data/climate_rainfall_interpolated.csv")
climate_windspeed_interpolated <- read_csv("data/climate_windspeed_interpolated.csv")
climate_geospatial <- readRDS("data/climate_geospatial.rds")


# Convert date columns
#climate_temperature_interpolated$date <- as.Date(climate_temperature_interpolated$date)
#climate_rainfall_interpolated$date <- as.Date(climate_rainfall_interpolated$date)
#climate_windspeed_interpolated$date <- as.Date(climate_windspeed_interpolated$date)


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


generate_idw_rainfall <- function(data, resolution, nmax, mpsz) {
  
  # Read the shapefile inside the function
  mpsz <- st_read("data/geospatial/MP14_SUBZONE_WEB_PL.shp") 
  mpsz <- st_transform(mpsz, crs = 3414)
  
  # Create a new 'rainfall' column with random values between 200 and 400
  data$value_rainfall <- runif(nrow(data), min = 200, max = 400)
  
  set.seed(123)
  
  # Compute bounding box and transform to EPSG:3414
  bbox <- st_bbox(mpsz)
  
  # Extract min/max coordinates
  lon_min <- st_bbox(bbox)["xmin"]
  lon_max <- st_bbox(bbox)["xmax"]
  lat_min <- st_bbox(bbox)["ymin"]
  lat_max <- st_bbox(bbox)["ymax"]
  
  # Calculate number of points based on resolution
  n_points <- ceiling((lon_max - lon_min) / resolution) * ceiling((lat_max - lat_min) / resolution)
  
  # Generate random points within the bounding box
  random_points <- data.frame(
    X = runif(n_points, lon_min, lon_max),
    Y = runif(n_points, lat_min, lat_max)
  )
  
  # Convert to sf object with correct CRS
  random_points_sf <- st_as_sf(random_points, coords = c("X", "Y"), crs = 3414)
  
  # Perform IDW interpolation
  idw_rainfall_random <- idw(formula = value_rainfall ~ 1, 
                             locations = data, 
                             newdata = random_points_sf, 
                             nmax = nmax)
  
  # Convert IDW results into an sf object
  coords <- st_coordinates(idw_rainfall_random)
  idw_rainfall_sf <- cbind(idw_rainfall_random, coords)
  idw_rainfall_sf <- st_as_sf(idw_rainfall_sf, coords = c("X", "Y"), crs = 3414)
  
  # Clip points to Singapore boundary
  idw_rainfall_sf <- idw_rainfall_sf[mpsz, ]
  
  return(idw_rainfall_sf)  
}

plot_rainfall_idw <- function(data, resolution, nmax) {
  tmap_mode("plot")
  map <- tm_shape(data) +
    tm_symbols(col = "var1.pred", 
               size = "var1.pred", 
               scale = 3, style = "jenks", palette = "Blues",  
               border.col = "black", 
               border.lwd = 0.5) +
    tm_layout(
      title = "Interpolated Rainfall at Random Points in Singapore",
      frame = TRUE,          
      frame.lwd = 2,         
      title.size = 1.5
    )
  
  tmap_mode("view")
  
  return(map)
}


# IDW function for temperature

generate_idw_temperature <- function(data, resolution, nmax, mpsz) {
  
  # Read the shapefile inside the function
  mpsz <- st_read("data/geospatial/MP14_SUBZONE_WEB_PL.shp") 
  mpsz <- st_transform(mpsz, crs = 3414)
  
  # Create a new 'value_temperature' column with no values (NA initially)
  data$value_temperature <- runif(nrow(data), min = 20, max = 35)
  
  set.seed(123)
  
  # Compute bounding box and transform to EPSG:3414
  bbox <- st_bbox(mpsz)
  
  # Extract min/max coordinates
  lon_min <- bbox["xmin"]
  lon_max <- bbox["xmax"]
  lat_min <- bbox["ymin"]
  lat_max <- bbox["ymax"]
  
  # Calculate number of points based on resolution
  n_points <- ceiling((lon_max - lon_min) / resolution) * ceiling((lat_max - lat_min) / resolution)
  
  # Generate random points within the bounding box
  random_points <- data.frame(
    X = runif(n_points, lon_min, lon_max),
    Y = runif(n_points, lat_min, lat_max)
  )
  
  # Convert to sf object with correct CRS
  random_points_sf <- st_as_sf(random_points, coords = c("X", "Y"), crs = 3414)
  
  # Perform IDW interpolation using the new 'value_temperature' column
  idw_temperature_random <- idw(formula = value_temperature ~ 1, 
                                locations = data, 
                                newdata = random_points_sf, 
                                nmax = nmax)
  
  # Convert IDW results into an sf object
  coords <- st_coordinates(idw_temperature_random)
  idw_temperature_sf <- cbind(idw_temperature_random, coords)
  idw_temperature_sf <- st_as_sf(idw_temperature_sf, coords = c("X", "Y"), crs = 3414)
  
  # Clip points to Singapore boundary
  idw_temperature_sf <- idw_temperature_sf[mpsz, ]
  
  return(idw_temperature_sf)
} 

plot_temperature_idw <- function(data, resolution, nmax) {
  tmap_mode("plot")
  map <- tm_shape(data) +
    tm_symbols(col = "var1.pred", 
               size = "var1.pred", 
               scale = 3, style = "jenks", palette = "Reds",  
               border.col = "black", 
               border.lwd = 0.5) +
    tm_layout(
      title = "Interpolated Temperature at Random Points in Singapore",
      frame = TRUE,          
      frame.lwd = 2,         
      title.size = 1.5
    )
  
  tmap_mode("view")
  
  return(map)
}

# IDW function for wind speed

generate_idw_windspeed <- function(data, resolution, nmax, mpsz) {
  
  # Read the shapefile inside the function
  mpsz <- st_read("data/geospatial/MP14_SUBZONE_WEB_PL.shp") 
  mpsz <- st_transform(mpsz, crs = 3414)
  
  # Populate 'value_windspeed' with random values (for testing)
  data$value_windspeed <- runif(nrow(data), min = 0, max = 30)
  
  set.seed(123)
  
  # Compute bounding box and transform to EPSG:3414
  bbox <- st_bbox(mpsz)
  
  # Extract min/max coordinates
  lon_min <- bbox["xmin"]
  lon_max <- bbox["xmax"]
  lat_min <- bbox["ymin"]
  lat_max <- bbox["ymax"]
  
  # Calculate number of points based on resolution
  n_points <- ceiling((lon_max - lon_min) / resolution) * ceiling((lat_max - lat_min) / resolution)
  
  # Generate random points within the bounding box
  random_points <- data.frame(
    X = runif(n_points, lon_min, lon_max),
    Y = runif(n_points, lat_min, lat_max)
  )
  
  # Convert to sf object with correct CRS
  random_points_sf <- st_as_sf(random_points, coords = c("X", "Y"), crs = 3414)
  
  # Perform IDW interpolation using the new 'value_windspeed' column
  idw_windspeed_random <- idw(formula = value_windspeed ~ 1, 
                              locations = data, 
                              newdata = random_points_sf, 
                              nmax = nmax)
  
  # Convert IDW results into an sf object
  coords <- st_coordinates(idw_windspeed_random)
  idw_windspeed_sf <- cbind(idw_windspeed_random, coords)
  idw_windspeed_sf <- st_as_sf(idw_windspeed_sf, coords = c("X", "Y"), crs = 3414)
  
  # Clip points to Singapore boundary
  idw_windspeed_sf <- idw_windspeed_sf[mpsz, ]
  
  return(idw_windspeed_sf)
} 

plot_windspeed_idw <- function(data, resolution, nmax) {
  tmap_mode("plot")
  map <- tm_shape(data) +
    tm_symbols(col = "var1.pred", 
               size = "var1.pred", 
               scale = 3, style = "jenks", palette = "Greens",  
               border.col = "black", 
               border.lwd = 0.5) +
    tm_layout(
      title = "Interpolated Wind Speed at Random Points in Singapore",
      frame = TRUE,          
      frame.lwd = 2,         
      title.size = 1.5
    )
  
  tmap_mode("view")
  
  return(map)
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
  
  
  
  # Geospatial Analysis Menu
  navbarMenu(
    HTML(paste0(
      '<div style="display: inline-flex; align-items: center;">',
      '<img src="google-maps.png" height="15px" style="margin-right: 5px;">',
      'Geospatial Analysis',
      '</div>'
    )),
    
    tabPanel("Inverse Distance Weighted (IDW) Interpolation",
             sidebarLayout(
               sidebarPanel(
                 selectInput("idw_year", 
                             "Select Year",
                             choices = 2020:2024,
                             selected = 2024),
                 
                 selectInput("idw_month",
                             "Select Month",
                             choices = month.name,
                             selected = month.name[1]),
                 
                 sliderInput("resolution",
                             "Resolution:",
                             min = 3000,
                             max = 5000,
                             value = 1000,
                             step = 200),
                 
                 sliderInput("nmax",
                             "Nmax:",
                             min = 1,
                             max = 10,
                             value = 1,
                             step = 1),
                 width = 3
               ),
               mainPanel(
                 tabsetPanel(
                   id = "idw_tabs",
                   
                   tabPanel("Rainfall",
                            tmapOutput("rainfall_idw_map", height = "600px")),
                   
                   tabPanel("Temperature",
                            tmapOutput("temperature_idw_map", height = "600px")),
                   
                   tabPanel("Wind Speed",
                            tmapOutput("windspeed_idw_map", height = "600px"))
                 ),
                 width = 9
               )
             )
    )
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
  
  
  
  
  # Overview plot output
  output$overview_plot <- renderPlot({
    create_overview_plot()
  }, height = 600)
  
  
  
  
  # Rainfall IDW Map
  output$rainfall_idw_map <- renderTmap({
    req(input$idw_year, input$idw_month)
    
    withProgress(message = 'Calculating IDW...', {
      idw_data <- generate_idw_rainfall(
        data = climate_geospatial,
        resolution = input$resolution,
        nmax = input$nmax,
        mpsz = mpsz
      )
      
      plot_rainfall_idw(
        data = idw_data,
        resolution = input$resolution,
        nmax = input$nmax
      )
    })
  })  
  
  # temperature IDW Map
  output$temperature_idw_map <- renderTmap({
    req(input$idw_year, input$idw_month)
    
    withProgress(message = 'Calculating IDW...', {
      idw_data <- generate_idw_temperature(
        data = climate_geospatial,
        resolution = input$resolution,
        nmax = input$nmax,
        mpsz = mpsz
      )
      
      plot_temperature_idw(
        data = idw_data,
        resolution = input$resolution,
        nmax = input$nmax
      )
    })
  })  
  
  # windspeed IDW Map
  output$windspeed_idw_map <- renderTmap({
    req(input$idw_year, input$idw_month)
    
    withProgress(message = 'Calculating IDW...', {
      idw_data <- generate_idw_windspeed(
        data = climate_geospatial,
        resolution = input$resolution,
        nmax = input$nmax,
        mpsz = mpsz
      )
      
      plot_windspeed_idw(
        data = idw_data,
        resolution = input$resolution,
        nmax = input$nmax
      )
      
    })
  })    
}

# Run the application 
shinyApp(ui = ui, server = server)