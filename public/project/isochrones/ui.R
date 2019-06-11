library(shiny)
library(httr)
library(ggmap)
library(geojsonR)
library(leaflet)
library(jsonlite)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 100, left = 20, width = 350,
                textInput("address","Address",value = "Madison WI 53719"),
                #uiOutput("ui_suggested"),
                radioButtons("vehicle","Method",choices = c("driving","cycling","walking"),selected = "driving"),
                actionButton("enter","Enter"),
                style = "opacity: 0.9; z-index: 1000;"
                
                
                
  )
  
  
)