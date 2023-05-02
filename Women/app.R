library(shiny)
library(tidyverse)
library(cartogram)

# Load data
GIIHDI <- read.csv("/Users/shreyasusanmathew/Desktop/STAT 231/SJV_Politics_Economics_blog/Women/GIIHDI.csv")

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
      cartogramOutput("cartogram")
    )
  )
)

# Define server logic required to generate cartogram plot
server <- function(input, output) {
  
  # Create reactive expression for cartogram data
  carto_data <- reactive({
    # Subset data based on selected indicator
    selected_var <- input$indicator
    subset(GIIHDI, select = c("ISO_Code", selected_var))
  })
  
  # Generate cartogram plot
  output$cartogram <- renderCartogram({
    cartogram(
      data = carto_data(),
      projection = "mercator",  # Choose map projection
      weight = input$indicator,  # Choose variable to weight by
      group = "ISO_Code",  # Specify country grouping variable
      itermax = 5  # Set maximum number of iterations for cartogram algorithm
    )
  })
}

# Run the app
shinyApp(ui, server)
