library(ggplot2)
library(tidyverse)
library(shiny)
library(shinythemes)
library(dplyr)
library(igraph)
library(ggnetwork)


unempData <- read.csv("data/wrangled.csv")
countries <- unique(unempData$Country)

############
#    ui    #
############

ui <-  navbarPage(
  theme = shinytheme("slate"),
  title="Country Development and Unemployment",
  #########################Tab 1: Victor##########################
  #Creating a tab for a web graph showing the distribution of AAS club budget
  #with check boxes to choose what types of clubs you would like to 
  #display in the graph
  tabPanel(
    title = "Education",
    sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "eduScatter",
                           label = "Club Types",
                           min = 0,
                           max = 1,
                    value = 1,
                    step = 0.05
                    
        )
      ),
      mainPanel(
        plotOutput("fundNet"),
        "This web graph summarizes the distribution of AAS's spring club budget
      distribution."
      )
    )
  ),
  ###########################Tab 2: Victor#############################
  #Creating a tab to search clubs and compare their requested and allocated
  #funds through column charts
  tabPanel(
    title = "Search",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "searchme", 
                       label = "Search Bar",
                       multiple = TRUE,
                       choices = c("Search Bar" = "", clubs),
                       options = list(create = FALSE,
                                      placeholder = "Search Me",
                                      onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                      onType = I("function (str) {if (str === \"\") {this.close();}}"),
                                      onItemAdd = I("function() {this.close();}"))
        ),
      )
      ,
      mainPanel(
        plotOutput(outputId = "col"),
        tableOutput(outputId = "table"),
        "Here, clubs can be searched and selected to have their data displayed
        in the column chart as well as in the table."
      )
    )
  ),
  ######################## TAB 3: Shreya ###############################
  tabPanel(
    title = "Comparison",
    sidebarLayout(
      sidebarPanel(
        
        # Two drop down menus to choose the two different clubs to compare.
        selectInput(inputId = "club1", label = "Select Club 1", choices = clubs, selected = "Film Society"),
        selectInput(inputId = "club2", label = "Select Club 2", choices = clubs, selected = "Outing Club"),
        
        # radio buttons to choose the type of graph.
        radioButtons(inputId = "chart_type", label = "Select Chart Type:",
                     choices = c("Stacked Bar Chart", "Pie Chart"),
                     selected = "Stacked Bar Chart") #default stacked bar chart
      ),
      # Output area for plots
      mainPanel(
        # output areas for either pie chart or stacked bar chart
        plotOutput("plot"),
        # output space for the table representing the information of the clubs selected from the drop down menu.
        tableOutput(outputId = "summary"),
        "The graphs help to compare the requested v/s allocated budgets for two different clubs at a time. \n
        Pink represents the amount requested and the overlaying Purple represents the amount allocated to each club. \n
        For example, if the pie chart or the barplot completely appears purple, it means that all the amount requested \n
        was allocated to the club. Conversely, the comparitive height of the purple block shows the discrepency between \n
        allocation of budget between the clubs."
      )
    )
  ),
  
  ####################### TAB 4: Jesse ##############################
  tabPanel(
    title = "Budget Table with Slider",
    sidebarLayout(
      sidebarPanel(
        #Makes a slider 
        sliderInput(inputId = "sliderInput",
                    label = "Budget Slider",
                    min = 0,
                    #I chose the value $13000 because only one club had a budget more than this amount. 
                    #If you could keep increasing the slider past this point, nothing would change
                    #in the table output.
                    max = 13000,
                    value = 7000),
        "Amount of money allocated (US Dollars)"
      ),
      #Outputs the table
      mainPanel(
        tableOutput(outputId = "sliderTable"),
        "Clubs allocated more money than the amount choosen with the slider will show up in this table.",
        "Using the slider and table together allows for an interactive way to see how many clubs are allocated more money than a certain amount."
      )
    )
  ),
  #############################Tab 5: Jesse################################
  tabPanel(
    title = "Scatter Plots By Type of Club",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "checkBoxInput",
                           label = "Club Type",
                           #These are the choices of clubs for the buttons
                           choices = list("Activism", "Affinity",
                                          "Recreational", "Religious",
                                          "Club Sports", "Publications",
                                          "Service")
        )
      ),
      mainPanel(
        #I named this plotJ to distinguish from Shreya's plot.
        plotOutput("plotJ"),
        "Is there are relationship between the type of club and how much of the money
        requested was actually granted? This plot can help to answer this question.
        For instance, if we compare 'Affinity' and 'Recreational' we can see that
        the slope of the line for 'Recreational' is steeper, indicating that Recreational
        clubs tend to be granted more of the money they request than 'Affinity' clubs."
      )
    )
  )
)

##############################################################################


############
# server   #
############
server <- function(input,output){
  
  ####################################Jesse#################################
  #Puts clubData into the data frame data_for_ScatterPlot_by_Type and filters only
  #Those clubs whose type matches the types the user selects with the buttons.
  data_for_ScatterPlot_by_Type <- reactive({
    data <- clubData %>%
      filter(clubType %in% input$checkBoxInput)
  })
  
  output$plotJ <- renderPlot({
    
    #The code to make the scatter plot
    ggplot(data = data_for_ScatterPlot_by_Type(), aes(x = Allocated, y = Requested, color = clubType)) +
      geom_point(size = 5) +
      geom_smooth(method = "lm") +
      #Labels and title for the scatter plot
      labs (y = "Money Requested (US Dollars)",
            x = "Money Allocated (US Dollars)",
            title = "Funding Requested vs. Allocated by Club Type")
  })
  
  
  data_for_sliderTable <- reactive({
    #The slider table displays clubs whose value for Allocated is greater than the slider value.
    data <- clubData %>% filter(Allocated > input$sliderInput)
  })
  
  ################################Victor########################################
  # Reactive dataset that filters the selected club types for the web graph
  # and turns the dataset into an igraph
  data_for_graph <- reactive({
    data <- graphData %>% 
      filter(clubType %in% input$networkG) %>% 
      graph_from_data_frame()
  })
  # Reactive dataset that filters the searched clubs to display in the column
  #chart
  data_for_col <- reactive({
    data <- budgetLong %>% filter(Club %in% input$searchme)
  })
  # Reactive dataset that filters the searched clubs to display in a table
  data_for_table <- reactive({
    data <- clubData %>% filter(Club %in% input$searchme)
  })
  #Plot for the web graph with the edges changing by color depending what 
  #club type they belong to and changing by thickness of the edge depending
  #on how much funding they received (more funding, thicker edge)
  #The plot reacts to the chosen club types
  output$fundNet <- renderPlot({
    ggplot(data=data_for_graph()
           , aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_edges(angle = 30,
                 curvature = 0.4,
                 aes(color = clubType,
                     size = Allocated)) +
      geom_nodes(aes(color = clubType), size = 10) +
      geom_nodelabel(aes(label = name), size = 2.5) +
      labs(title = "AAS Club Budget Distribution",
           caption = "Source: Amherst Association of Students",
           color = "Club Type",
           size = "Amount Allocated to the Club") +
      theme_blank() +
      theme(legend.position = "bottom", legend.box="vertical")
  })
  #Column plot that displays data of searched clubs. Can be used to simply
  #display and compare
  output$col <- renderPlot({
    ggplot(data = data_for_col(), aes_string(fill = "action", x = "Club", y = "money")) +
      geom_col(color = "#2c7fb8", position = position_dodge()) +
      labs(y = "Funding (USD)", 
           title = "Club Funding Comparisons", 
           fill = "Action",
           caption = "Source: Amherst Association of Students"
      )
    
  })
  #Table that displays data of searched clubs. Can be used to simply
  #display and compare
  output$table <- renderTable(data_for_table()) 
  output$sliderTable <- renderTable(data_for_sliderTable()) 
  
  
  ############################### TAB 2: Shreya ##################################
  # creating a new object that filters out the two separate rows of clubs that are selected in the drop down menu
  filtered_data <- reactive({
    clubData %>% 
      filter(Club == input$club1 | Club == input$club2) %>% 
      select(Club, Requested, Allocated)
  })
  
  # creating a new object for the first pie chart for club1 selected
  filtered_data1 <- reactive({
    clubData %>% 
      filter(Club == input$club1) %>% 
      select(Club, Requested, Allocated)
  })
  
  # creating a new object for the second pie chart for club2 selected
  filtered_data2 <- reactive({
    clubData %>% 
      filter(Club == input$club2) %>% 
      select(Club, Requested, Allocated)
  })
  
  
  output$plot <- renderPlot({
    
    # specifying condition of the radio button
    if (input$chart_type == "Stacked Bar Chart") {
      
      # Create the barplot
      ggplot(filtered_data(), aes(x = Club, y = Requested, fill = "Requested")) +
        geom_bar(stat = "identity", position = "dodge") +
        # overlaying allocated budget
        geom_bar(aes(y = Allocated, fill = "Allocated"), stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Requested" = "#bd2f80", "Allocated" = "#6f2da8")) +
        labs(title = paste("Requested v/s Allocated Budget for", input$club1, "and", input$club2),
             y = "Budget") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", colour = "black", size = 16),
              plot.background = element_rect(fill = "grey100", colour = "black"))
      
      # specifying condition of the radio button  
    } else if (input$chart_type == "Pie Chart") {
      # Generate pie chart for club1
      plot1 <- ggplot(filtered_data1(), aes(x = "", y = Requested, fill = "Requested")) +
        geom_bar(stat = "identity") +
        geom_bar(aes(y = Allocated, fill = "Allocated"), stat = "identity") +
        # converting the barplot to radial graph
        coord_polar(theta = "y") +
        scale_fill_manual(values = c("Requested" = "#bd2f80", "Allocated" = "#6f2da8")) +
        ggtitle(paste("Requested v/s Allocated for", input$club1)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", colour = "black", size = 16,  hjust = 1),
              plot.background = element_rect(fill = "grey100", colour = "black"))
      
      #generate piechart for club2
      plot2 <- ggplot(filtered_data2(), aes(x = "", y = Requested, fill = "Requested")) +
        geom_bar(stat = "identity") +
        geom_bar(aes(y = Allocated, fill = "Allocated"), stat = "identity") +
        # converting the barplot to radial graph
        coord_polar(theta = "y") +
        scale_fill_manual(values = c("Requested" = "#bd2f80", "Allocated" = "#6f2da8")) +
        ggtitle(paste("Requested v/s Allocated for", input$club2)) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 1),
              plot.background = element_rect(fill = "grey100", colour = "black"))
      
      #arranging the pie chart side by side 
      gridExtra::grid.arrange(plot1, plot2, ncol = 2)
    }
  })
  
  # for the table output
  output$summary <- renderTable(filtered_data()) 
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)