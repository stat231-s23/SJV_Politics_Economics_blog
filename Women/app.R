library(shiny)
library(tidyverse)
library(cartography)
library(leaflet)
library(scales)

# Load data
load("Shreya.RData")

# Define names of the variables
rank_choice_values <- c("High" = indicatorMap$GII_value >= 0.08, 
                        "Medium" = indicatorMap$GII_value >= 0.04 & indicatorMap$GII_value >= 0.08,
                        "Low" = indicatorMap$GII_valuee < 0.04)
rank_choice_names <- c("High", "Medium", "Low", "All")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Gender Inequality Index"),
  
  sidebarPanel(
    # Sidebar with a radio input for number of bins 
    radioButtons(inputId = "rank"
                 , label = "Choose the range:"
                 , choices = rank_choice_names
                 , selected = "All"),
    "Higher values of Gender Inequality Index(GII) correspond to more gender inequality."
  ),
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput(outputId = "ChoroplethMap")
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
      return(c(0.3, 0.7))
    }
    if("High"== input$rank) {
      return(c(0.7, 1.0))
    }
    vec <- c(0, 1.0)
    return(vec)
  })
  
  output$ChoroplethMap <- renderLeaflet({
    indicator <- as.numeric(indicatorMap$GII_value)
    
    indicator_rescaled <- rescale(indicator, to = c(0, 1))
    mypalette <- colorBin(c("#F4BB44"), domain = indicator_rescaled, bins = my_bins(), reverse = TRUE, na.color = "#FAF9F6")
    
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
