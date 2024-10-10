# Load necessary libraries
library(shiny)
library(httr)
library(jsonlite)

# Function to get municipality ID
get_municipality_id <- function(municipality_name) {
  base_url <- "https://api.kolada.se/v2/"
  endpoint <- "municipality"
  url <- paste0(base_url, endpoint)
  
  response <- httr::GET(url)
  if (httr::http_status(response)$category != "Success") {
    stop("API request failed: ", httr::http_status(response)$message)
  }
  
  content <- httr::content(response, "text")
  municipalities_data <- jsonlite::fromJSON(content)
  
  if (!"values" %in% names(municipalities_data)) {
    stop("No 'values' field found in the API response.")
  }
  
  match <- municipalities_data$values[municipalities_data$values$title == municipality_name, ]
  if (nrow(match) == 0) {
    stop("Municipality not found: ", municipality_name)
  }
  
  return(match$id[1])
}

# Function to get Kolada data using KPI, municipality ID, and year
get_kolada_data <- function(kpi, municipality_id, year) {
  base_url <- "https://api.kolada.se/v2/data"
  url <- paste(base_url, "kpi", kpi, "municipality", municipality_id, "year", year, sep = "/")
  
  response <- httr::GET(url)
  if (httr::http_status(response)$category != "Success") {
    stop("API request failed: ", httr::http_status(response)$message)
  }
  
  content <- httr::content(response, "text")
  parsed_content <- jsonlite::fromJSON(content)
  
  return(parsed_content)
}

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Kolada Data Viewer"),
  
  # Sidebar for input
  sidebarLayout(
    sidebarPanel(
      textInput("municipality", "Enter Municipality Name:", value = "Stockholm"),
      selectInput("year", "Select Year:", choices = c("2019", "2020", "2021", "2022", "2023")),
      actionButton("goButton", "Get Data")
    ),
    
    # Show results in the main panel
    mainPanel(
      verbatimTextOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$goButton, {
    municipality_name <- input$municipality
    year <- input$year
    kpi <- "N02799"  # Example KPI for maternal care
    
    output$result <- renderPrint({
      tryCatch({
        # Get municipality ID
        municipality_id <- get_municipality_id(municipality_name)
        
        # Fetch Kolada data
        data <- get_kolada_data(kpi, municipality_id, year)
        
        # Check if data is empty
        if (data$count == 0) {
          return("No data available for the selected municipality and year.")
        } else {
          browser()
          # res_data <- data.frame(kpi = data$kpi, )
          # res_data <- data.frame()
          # res_data$kpi <- data$kpi
          # res_data$municipality <- data$municipality
          # res_data$period <- data$period
          # data$values$count <- NULL
          
          # Display the result
          return(data)
        }
        
      }, error = function(e) {
        # If there's an error, print the error message
        paste("Error: ", e$message)
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
