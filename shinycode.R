# Load necessary libraries
library(shiny)
library(ggplot2)
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


# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("Kolada Children Born Plot"),
  
  # Sidebar for input
  sidebarLayout(
    sidebarPanel(
      textInput("municipality", "Enter Municipality Name:", value = "Stockholm"),
      actionButton("goButton", "Get Data")
    ),
    
    # Main panel to show the plot
    mainPanel(
      plotOutput("kpiPlot")  # The plot output
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  observeEvent(input$goButton, {
    municipality_name <- input$municipality
    kpi <- "N02799" 
    
    output$kpiPlot <- renderPlot({
      tryCatch({
        # Get municipality ID
        municipality_id <- get_municipality_id(municipality_name)
        
        # Fetch data for multiple years (2019 to 2023)
        years <- 2019:2023
        values <- numeric(length(years))
        
        for (i in seq_along(years)) {
          data <- get_kolada_data(kpi, municipality_id, as.character(years[i]))
          
          # Check if the 'values' field contains data and has the nested structure
          if (!is.null(data$values) && nrow(data$values) > 0 && !is.null(data$values$values[[1]])) {
            # Extract the 'value' field from the inner data frame
            gender_data <- data$values$values[[1]]  # Access the inner data frame
            if (!is.null(gender_data$value)) {
              # Assuming we want the first row (e.g., gender "K" for "female")
              values[i] <- gender_data$value[gender_data$gender == "K"][1]  # First "K" value
            } else {
              values[i] <- NA
            }
          } else {
            # Handle cases where no valid data is available
            values[i] <- NA
          }
        }
        
        # Create a data frame for plotting
        plot_data <- data.frame(Year = years, KPI_Value = values)
        
        # Create the plot using ggplot2
        ggplot(plot_data, aes(x = Year, y = KPI_Value)) +
          geom_line(color = "blue") +
          geom_point() +
          labs(title = "Number of children born / 1000", x = "Year", y = "KPI Value") +
          theme_minimal()
        
      }, error = function(e) {
        # Print error message to console or handle in some other way
        print(paste("Error: ", e$message))
      })
    })
  })
}




# Run the application 
shinyApp(ui = ui, server = server)
