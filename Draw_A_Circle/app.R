#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
source("Circle_Engine.R")

# Define UI for application that draws a circle
ui <- fluidPage(
    # Application title
    titlePanel("Draw A Circle"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "radius",
                         label = "Radius (mm)",
                         value = 1, #Defaults to 1
                         min=1), #Sets the minimum possible value to 1, since any lower breaks the graph
            numericInput(inputId = "xCenter",
                         label ="X Center of Circle (mm)",
                          value = 0), #Defaults to 0
            numericInput(inputId = "yCenter",
                         label = "Y Center of Circle (mm)",
                         value = 0), #Defaults to o
            sliderInput(inputId = "res",
                        label = "Resolution or smoothness of circle (dot/mm)",
                        min = 0.5,
                        max = 10, 
                        value = 0.5), #Input slider that limits the resolution range between 1 and 10. Defaults to 0.5
            radioButtons(inputId="plotlyOrGGPlot",
                         label = "Choose Plot Type",
                         choices = list("GGPlot" = "ggplot", 
                                        "Plotly" = "plotly"), #Radio buttons for allowing user to select which plot library to use.
                         selected = "ggplot" #Default is ggplot
                         ),
            actionButton(inputId = "genCircle",
                         label = "Draw Circle")
        ),

        # Show a plot of the generated circle
        mainPanel(
          fluidRow( #Changes which graph depending on which radio button is selected # nolint
            conditionalPanel(condition = "input.plotlyOrGGPlot == 'plotly'", plotlyOutput("circPlotly")),
            conditionalPanel(condition = "input.plotlyOrGGPlot == 'ggplot'", plotOutput("circGGPlot"))  
          )
        )
    )
)

# Define server logic required to draw a circle
server <- function(input, output)
{
  observeEvent(input$genCircle, 
  {
      radius <- as.double(input$radius)
      xCent <- as.double(input$xCenter)
      yCent <- as.double(input$yCenter)
      resolution <- as.double(input$res)
      graphType <- input$plotlyOrGGPlot
    
      data <- Calcpoints(radius, xCent, yCent, resolution)
      
      #Notify user that the button needs to be clicked each time an input is altered.
      showNotification("Make sure to redraw circle after making changes, or else changes will not take effect.",
                       type="warning",
                       duration = 3)
      
      if(graphType == "plotly")
      {
        output$circPlotly <- renderPlotly({
        graphCircle(usePlotly = TRUE, data = data, x = xCent, y = yCent) #Output the plotly graph
        })
      }
      else
      {
         output$circGGPlot <- renderPlot({
         graphCircle(usePlotly = FALSE, data = data, x = xCent, y = yCent) #Output the ggplot graph
        })
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
