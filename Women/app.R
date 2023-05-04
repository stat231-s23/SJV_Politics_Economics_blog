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
rank_choice_names <- c("Very High", "High", "Medium", "Low", "All")

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
      radioButtons(inputId = "rank"
                   , label = "Choose the range:"
                   , choices = rank_choice_names
                   , selected = "All"),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput(outputId = "ChoroplethMap")
    )
  )
)

# Define server logic required to generate cartogram plot
server <- function(input, output) {
  
  
  

  
  mytext <- paste(
    "Country: ", indicatorMap$Country, "<br/>",
    "Adolescent BirthRate: ", indicatorMap$Adol_birthrate, "<br/>",
    "Political Participation: ", indicatorMap$Parliament, "<br/>",
    "Labour Force Participation: ", indicatorMap$LF_Part, "<br/>",
    sep = ""
  ) %>%
    lapply(htmltools::HTML)
  
  my_bins <- reactive({
    if("Low" == input$rank ) {
      return(c(0, 0.3))
    }
    if("Medium"== input$rank) {
      return(c(0.3, 0.5))
    }
    if("High"== input$rank) {
      return(c(0.5, 0.8))
    }
    if("Very High"== input$rank) {
      return(c(0.8, 1.0))
    }
    vec <- c(0, 1.0)
    return(vec)
  })
  
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
    mypalette <- colorBin(c("green"), domain = indicator_rescaled, bins = my_bins(), reverse = TRUE, na.color = "#FAF9F6")
    
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
