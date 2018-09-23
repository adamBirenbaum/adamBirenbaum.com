library(plotly)
library(shiny)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  plotlyOutput("plot", width = "100%", height = "100%"),
  absolutePanel(top = 100, left = 20, width = 300,
                sliderInput("a","Semi-Major axis",min = 1, max = 20, step = 1, value = 10),
                sliderInput("e","Eccentricity", min =0, max = 1, step = .05, value = .1),
                sliderInput("i","Inclination", min =0, max = 90, step = 5, value = 10),
                sliderInput("om","Longitude of Ascending Node", min =0, max = 180, step = 5, value = 10),
                sliderInput("w","Argument of Periapsis", min =0, max = 90, step = 5, value = 10),
                style = "opacity: 0.9; z-index: 1000;"
                
                
                
  )

  
)
