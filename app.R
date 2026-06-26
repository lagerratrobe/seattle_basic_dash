library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
source("utils.R")

# Pull in the last 48 hours of Seattle data
df <- getData()

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel( h1("Sequim Weather", align = "left"),
              windowTitle = "Sequim Weather Dash"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      # Create a selector aka `input$variable`...
      selectInput(inputId = "variable",
                  label = h4("Choose Variable to Plot"),
                  choices = c("Temperature", "Precip"),
                  selected = "Temperature")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Verbatim text for data summary ----
      plotOutput("variablePlot"),
      # Output: HTML table with requested number of observations ----
      tableOutput("table_data")
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    select(df, 
           input$variable,
           Time)
  })
  
  dataPlot <- reactive({
    getPlot( df, input$variable )
  })

  # Plot section 
  # output$variablePlot <- renderPlot({
  #   ggplot(df, mapping = aes(x = Time, y=.data[[input$variable]])) +
  #   geom_line() 
  # })
  output$variablePlot <- renderPlot({
    dataPlot()
  })
  
   # DT table section
  output$table_data <- renderTable({
    mutate(df, 
           Time = as.character(Time)
           )
  })
  
}

# Create Shiny app ----
shinyApp(ui=ui, server=server)