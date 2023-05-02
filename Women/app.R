library(shiny)
library(tidyverse)

#load data
GIIHDI <- read_csv("data/GIIHDI.csv")

#defining names of the variables
hist_choice_values <- c("price","range","top_speed","weight","battery")
hist_choice_names <- c("Price","Range","Top Speed","Weight","Battery")
names(hist_choice_values) <- hist_choice_names

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Women Empowerment"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "cartograph"
                      , label = "Choose an Indicator:"
                      , choices = hist_choice_values
                      , selected = "price"),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
           )
        ) 
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
