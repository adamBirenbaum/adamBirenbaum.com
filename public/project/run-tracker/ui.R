library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(
      width = 4,
      sliderInput("miles",h3("Miles"), min = 0, max = 10, step = 0.1,value = 3)
      
    ),
    column(width = 4,
           shinyWidgets::radioGroupButtons(
             inputId = "person",
             label = h3("Runner"),
             choices = c("Bo", 
                         "Rachael", "Adam","Jo"),
             status = "success",
             checkIcon = list(
               yes = icon("ok", 
                          lib = "glyphicon"),
               no = icon("remove",
                         lib = "glyphicon"))
           )
           )
    
  ),
  fluidRow(
    column(width = 6,
           shinyWidgets::actionBttn(
             inputId = "enter",
             label = "Add Miles",
             style = "unite", 
             color = "danger"
           )
           )
  ),
  br(),
  hr(),
  
  fluidRow(
    column(width = 8,
           plotOutput("plot")
           )
  ),
  br(),
  hr(),
  fluidRow(
    shiny::tableOutput("table")
  )
  
  
  
)