library(shiny)
library(tidyverse)
library(cartography)
library(leaflet)
library(scales)

# Load data
load("Shreya.RData")

# Define names of the variables
indicator_choice_values <- c("Adol_birthrate", "Parliament", "Secondary", "LF_Part")
indicator_choice_names <- c("Adolescent Birthrate", "Political Participation", "Secondary Education", "Labor Force Participation")
names(indicator_choice_values) <- indicator_choice_names

rank_choice_values <- c("High" = indicator_choice_values[indicator_choice_values >= 80], 
                        "Medium" = indicator_choice_values[indicator_choice_values >= 50 & indicator_choice_values < 80],
                        "Low" = indicator_choice_values[indicator_choice_values < 50])
rank_choice_names <- c("High", "Medium", "Low")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Women Empowerment"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "indicator",
                  label = "Choose an Indicator:",
                  choices = indicator_choice_values,
                  selected = "Adol_birthrate"),
      ##############################################
      checkboxGroupInput(inputId = "rank"
                   , label = "Choose the range:"
                   , choices = rank_choice_names
                   , selected = "NULL"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput(outputId = "ChoroplethMap")
    )
  )
)

# Define server logic required to generate cartogram plot
server <- function(input, output) {
  
  
  mypalette <- colorNumeric("Reds", domain = indicatorMap$indicator)

  
  mytext <- paste(
    "Country: ", indicatorMap$Country, "<br/>",
    "Adolescent BirthRate: ", indicatorMap$Adol_birthrate, "<br/>",
    "Political Participation: ", indicatorMap$Parliament, "<br/>",
    "Labour Force Participation: ", indicatorMap$LF_Part, "<br/>",
    sep = ""
  ) %>%
    lapply(htmltools::HTML)
  
  
  # Generate cartogram plot
  output$ChoroplethMap <- renderLeaflet({
    indicator <- as.numeric(indicatorMap[[input$indicator]])
    #rank <- as.numeric(indicatorMap[[input$rank]])
    
    #if (rank == "High") {
    #  indicator_rescaled <- rescale(indicator, to = c(0.8, 1))
   # } else if (rank == "Medium") {
    #  indicator_rescaled <- rescale(indicator, to = c(0.5, 0.8))
   # } else {
    #  indicator_rescaled <- rescale(indicator, to = c(0, 0.5))
   # }
    
    indicator_rescaled <- rescale(indicator, to = c(0, 1))
    
    leaflet() %>%
      addTiles() %>%
      setView(lat = 10, lng = 0, zoom = 2) %>%
      addPolygons(data = indicatorMap, fillColor = ~mypalette(indicator_rescaled),
                  stroke = TRUE,
                  color = "black",
                  weight = 0.5,
                  fillOpacity = 0.7,
                  label = mytext)
  })
}

# Run the app
shinyApp(ui, server)
