library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)


# Load the dataset and cleaning
data = read.csv('housing.csv')
colnames(data)[1] = "Neighborhood"


# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Pittsburgh Neighborhood Housing Data"
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    
    # Inputs: select population range ---------------------------------------
    sliderInput("Pop", "Population (2010)",
                min = min(data$Pop_2010), max = max(data$Pop_2010),
                value = range(data$Pop_2010)
    ),
    
    # Input: select land area size ------------------------------------------
    sliderInput("area", "Land Area (acre)",
                min = min(data$Area), max = max(data$Area),
                value = range(data$Area)
    ),
    
    # Input: Select Sector ----------------------------------------------
    selectInput("Sect", "Sector",
                    choices = sort(unique(data$Sector)),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("1", "2", "3", "4", "5", "6", "7", "8",
                                 "9", "10", "11", "12", "13", "14", "15", "16"))
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("PopChange89"),
            valueBoxOutput("PopChange90"),
            infoBoxOutput("PopChange01")
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("land", plotlyOutput("plot_land")),
                   tabPanel("race", plotlyOutput("plot_race")),
                   tabPanel("age", plotlyOutput("plot_age")))
          )
  ),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Selected Pittsburgh Population Data", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  data_subset <- reactive({
    data <- data %>%
      
      # Slider Filter ----------------------------------------------
    filter(
      Pop_2010 >= input$Pop[1] &
      Pop_2010 <= input$Pop[2],
      Area >= input$area[1] &
      Area <= input$area[2])
    
    # Sector Filter ----------------------------------------------
    if (length(input$Sect) > 0 ) {
      data <- subset(data, Sector %in% input$Sect)
    }
    
    # Return dataframe ----------------------------------------------
    return(data)
  })
  
  # Reactive melted data ----------------------------------------------
  # mwInput <- reactive({
  #   swInput() %>%
  #     melt(id = "Neighborhood")
  # })
  
  # A plot showing land size and population size -----------------------------
  output$plot_land <- renderPlotly({
    dat <- subset(data_subset() ) #, variable == "land")
    
    # Generate Plot ----------------------------------------------
    ggplot(data = dat, aes(x = Area, y = Pop_2010, color = Neighborhood)) +
           geom_point()
  })
  
  # A plot showing the height of characters -----------------------------------
  output$plot_race <- renderPlotly({
    dat <- subset(data_subset() )#,  variable == "race")
    ggplot(data = dat, aes(x = Perc_White, y = Perc_African_American, 
                           color = Neighborhood)) +
             geom_point()
  })
  
  # A plot showing the height of characters -----------------------------------
  output$plot_age <- renderPlotly({
    dat <- subset(data_subset() )#,  variable == "age")
    ggplot(data = dat, aes(x = Perc_Pop_Age_20.34, y = Perc_Pop_Age.60.74, 
                           color = Neighborhood)) +
      geom_point()
  })
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(data_subset(), select = c(Neighborhood, Sector, Pop_2010, 
                                     Perc_Pop_Change_80.90,
                                     Perc_Pop_Change_90.00, Perc_Pop_Change_00.10,
                                     Area, Perc_African_American, Perc_White, 
                                     Perc_Pop_Age_20.34, Perc_Pop_Age.60.74))
  })
  
  # PopChange89 info box ----------------------------------------------
  output$PopChange89 <- renderInfoBox({
    new <- data_subset()
    num <- round(mean(new$Perc_Pop_Change_80.90, na.rm = T), 2)
    
    infoBox("Average Percent Population Change 1980-1990", value = num, 
            subtitle = paste(nrow(new), "characters"), 
            icon = icon("balance-scale"), color = "red")
  })
  
  # PopChange90 value box ----------------------------------------------
  output$PopChange90 <- renderValueBox({
    new <- data_subset()
    num <- round(mean(new$Perc_Pop_Change_90.00, na.rm = T), 2)
    
    valueBox(subtitle = "Average Percent Population Change 1990-2000", 
             value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
  
  # PopChange01 info box ----------------------------------------------
  output$PopChange01 <- renderInfoBox({
    new <- data_subset()
    num <- round(mean(new$Perc_Pop_Change_00.10, na.rm = T), 2)
    
    infoBox("Average Percent Population Change 2000-2010", value = num, 
            subtitle = paste(nrow(new), "characters"), 
            icon = icon("balance-scale"), color = "purple")
  })
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)