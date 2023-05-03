library(shiny)
library(tidyverse)
library(cartography)

# Load data
load("Shreya.RData")

# Define names of the variables
indicator_choice_values <- c("Adol_birthrate", "Parliament", "Secondary", "LF_Part")
indicator_choice_names <- c("Adolescent Birthrate", "Political Participation", "Secondary Education", "Labor Force Participation")
names(indicator_choice_values) <- indicator_choice_names

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
                  selected = "Adol_birthrate")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput(outputId = "ChoroplethMap")
    )
  )
)

# Define server logic required to generate cartogram plot
server <- function(input, output) {
  
  
  mypalette <- colorNumeric("Reds", domain = indicatorMap$Parliament)

  
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
    leaflet() %>%
      addTiles() %>%
      setView(lat = 10, lng = 0, zoom = 2) %>%
      addPolygons(data = indicatorMap, fillColor = ~mypalette(indicatorMap$Parliament), stroke = FALSE,
                  fillOpacity = 0.7,
                  label = mytext)
  })
}

# Run the app
shinyApp(ui, server)
