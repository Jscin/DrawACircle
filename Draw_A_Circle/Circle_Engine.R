#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####  Title: Draw a Circle Exercise
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose: Use R and R Shiny to graph a 'perfect' circle
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Libraries: ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(plotly)
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
#### Beta Test: ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

betaMode = FALSE # Set this to TRUE to test the engine, set to FALSE if it is to run with the UI.

    
if(betaMode == TRUE){
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### * Input Section ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    beta.radius <- readline(prompt = "Enter any radius greater than 0 (mm): ") 
    beta.radius <- as.double(beta.radius) #mm
    
    beta.x <- readline(prompt = "Enter the center x value (mm): ") 
    beta.x <- as.double(beta.x)#mm
    
    beta.y <- readline(prompt = "Enter the center y value (mm): ")
    beta.y <- as.double(beta.y)#mm
    
    beta.resolution <- readline(prompt = "Enter any resolution greater than 0 (dot/mm): ")
    beta.resolution <- as.double(beta.resolution)#mm/dot
    
    beta.plotIsPlotly <- readline(prompt = "Enter 1 for plotly and 0 for ggplot: ")
    beta.plotIsPlotly <- as.integer(beta.plotIsPlotly)
    
    if(beta.plotIsPlotly == 1)
    {
      beta.isPlotly <- TRUE
    }
    else
    {
      beta.isPlotly <- FALSE
    }
    
    while(beta.radius <= 0) #Forces a radius higher than 0, as any lower breaks the graph
    {
      beta.radius <- readline(prompt = "Radius cannot be zero, please enter a higher value: ")
      beta.radius <- as.double(beta.radius)
    }
    
    while(beta.resolution <= 0) #Forces a resolution higher than 0, as any lower breaks the graph
    {
      beta.resolution<- readline(prompt = "Resolution cannot be zero, please enter a higher value: ")
      beta.resolution <- as.double(beta.resolution)
    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #### * Output Section ####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    beta.data <- Calcpoints(radius = beta.radius, x = beta.x, y = beta.y, resolution = beta.resolution) #Returns the dataframe containing the coordinates
    graphCircle(usePlotly = beta.isPlotly, data = beta.data, x = beta.x, y = beta.y) #Outputs a plotly plot if isPlotly is true, if false then it outputs a ggplot plot
}
