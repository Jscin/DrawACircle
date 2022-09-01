#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  Title: Draw a Circle Exercise
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose: Use R and R Shiny to graph a 'perfect' circle
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Libraries Needed for engine to function: ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(plotly)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Functions needed to run this engine. ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### * * Calculates the (x,y) coordinates around the circle's circumference and returns a dataframe. ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Calcpoints <- function(radius, xCenter, yCenter, resolution)
{
  
  circumference <- 2 * pi * radius
  dots <- circumference * resolution
  radian <- 0
  endLoop = 2 * pi #Ends the while loop once radian is >= 2*PI, moved here since it's unnecessary to recalculate each iteration.
  radianStep <- ((360 / dots) * pi) / 180 #The step counter for the loop, converts the angle into radians here to simplify loop calculations. 
 
  #Initialize x/y vectors as empty vectors
  x <- c()
  y <- c()
  
  while (radian <= endLoop) #Calculates the (x,y) coordinates around the circle's circumference and stores them in two vectors.
  {
    x <- c(x, (cos(radian) * radius) + xCenter)
    y <- c(y, (sin(radian) * radius) + yCenter)
    
    radian = radian + radianStep
  }
  
  return(data.frame(x = x, y = y)) #The x/y vectors are converted into a dataframe that is returned by the function.
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### * Core "Ring" Functions Below ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### * * Graph's the circle and allows user to select which graphing library to use. ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
graphCircle <- function(usePlotly, data, x, y)
{
  if(usePlotly)
  {
    circle <- plot_ly(rbind(data, head(data, 1)), x = ~x, y = ~y, type = "scatter", mode = "lines") #Sets up a plotly scatterplot connected by solid lines
    circle
  }
  else
  {
    ggplot(rbind(data, head(data, 1)), aes(x, y)) + geom_path() #Sets up a ggplot scatter plot connected by lines
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Beta Test Zone for the engine: ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

betaMode = FALSE # Set this to TRUE to test the engine, set to FALSE if it is to run with the UI.

    
if(betaMode == TRUE){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### * Input Section ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    radius <- readline(prompt = "Enter any radius greater than 0 (mm): ") 
    radius <- as.double(radius) #mm
    
    x <- readline(prompt = "Enter the center x value (mm): ") 
    x <- as.double(x)#mm
    
    y <- readline(prompt = "Enter the center y value (mm): ")
    y <- as.double(y)#mm
    
    resolution <- readline(prompt = "Enter any resolution greater than 0 (dot/mm): ")
    resolution <- as.double(resolution)#mm/dot
    
    plotIsPlotly <- readline(prompt = "Enter 1 for plotly and 0 for ggplot: ")
    plotIsPlotly <- as.integer(plotIsPlotly)
    
    if(plotIsPlotly == 1)
    {
      isPlotly <- TRUE
    }
    else
    {
      isPlotly <- FALSE
    }
    
    while(radius <= 0) #Forces a radius higher than 0, as any lower breaks the graph
    {
      radius <- readline(prompt = "Radius cannot be zero, please enter a higher value: ")
      radius <- as.double(radius)
    }
    
    while(resolution <= 0) #Forces a resolution higher than 0, as any lower breaks the graph
    {
      resolution<- readline(prompt = "Resolution cannot be zero, please enter a higher value: ")
      resolution <- as.double(resolution)
    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #### * Output Section ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data <- Calcpoints(radius = radius, x = x, y = y, resolution = resolution) #Returns the dataframe containing the coordinates
    graphCircle(usePlotly = isPlotly, data = data, x = x, y = y) #Outputs a plotly plot if isPlotly is true, if false then it outputs a ggplot plot
}
