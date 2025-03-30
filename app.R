library(shiny)
library(bslib)
library(htmltools)

# Define UI for application
# ui.R
# ui.R
# ui.R
# ui.R
ui <- navbarPage(
  title = "Weather Pulse",
  theme = bslib::bs_theme(version = 4),
  
  # CSS for styling
  header = tags$head(
    tags$style(HTML("
      .welcome-container {
        padding: 40px;
        max-width: 1000px;
        margin: 0 auto;
      }
      .welcome-title {
        text-align: center;
        color: #2c3e50;
        margin-bottom: 40px;
      }
      .welcome-text {
        font-size: 16px;
        line-height: 1.6;
        color: #34495e;
        text-align: justify;
      }
      .main-title {
        font-size: 48px;
        font-weight: bold;
        margin-bottom: 10px;
      }
    "))
  ),
  
  # Home Page
  tabPanel("Home",
           div(class = "welcome-container",
               div(class = "welcome-title",
                   div(class = "main-title", "WELCOME TO"),
                   div(class = "main-title", "WEATHER PULSE")
               ),
               
               div(class = "welcome-text",
                   tags$p(
                     "Singapore's climate has been experiencing ",
                     tags$strong("rising temperatures and increasing weather extremes"),
                     " driven by climate change and urbanisation. In 2024, Singapore experienced one of its ",
                     tags$strong("hottest years on record"),
                     ", with temperatures exceeding long-term averages. These climate trends pose significant risks, including ",
                     tags$strong("heat stress, water resource management challenges and urban planning concerns"),
                     "."
                   ),
                   
                   tags$p(
                     "Existing reports and tools offer real-time weather forecasts and historical comparisons using long-term averages. However, they lack interactive analysis tools that would allow for a deeper exploration of historical trends, spatial patterns and future projections."
                   ),
                   
                   tags$p(
                     tags$strong("Weather Pulse"),
                     " was developed to address these gaps. It is an R Shiny Application that has the following key features:"
                   ),
                   
                   tags$ul(
                     tags$li("Feature 1: ..."),
                     tags$li("Feature 2: ..."),
                     tags$li("Feature 3: ...")
                   )
               )
           )
  ),
  
  # Time Series Analysis dropdown menu
  navbarMenu("Time Series Analysis",
             tabPanel("Exploratory Data Analysis",
                      h2("Exploratory Data Analysis")
                      # Add EDA content here
             ),
             tabPanel("Time Series Forecasting",
                      h2("Time Series Forecasting")
                      # Add forecasting content here
             )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Server logic will be added as we build more functionality
}

# Run the application 
shinyApp(ui = ui, server = server)

