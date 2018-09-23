library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)

header <- dashboardHeader(title = "Vector Calculus")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Divergence",tabName = "divergence")
              
              )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "divergence",
      fluidRow(
        column(width = 4,
               box(width = NULL,
                 title = "Vector Field",
                 fluidRow(
                   column(width = 2,
                          br(),
                          h4("F(x,y)   = ") 
                          ),
                   column(width = 3,
                          textInput("div_i","" ,value = "x^2-y^2")
                          ),
                   column(width = 1,
                          br(),
                          h4(tags$b("i"))
                   ),
                   column(width = 1,
                          br(),
                          h4("+") 
                   ),
                   column(width = 3,
                          textInput("div_j","" , value = "2*x*y")
                   ),
                   column(width = 1,
                          br(),
                          h4(tags$b("j"))
                   )
                 )
                 
               )
               )
      ),
      fluidRow(
        column(width = 12,
               uiOutput("ui_div_plot")
               )
      ),
      box(width = 6,
          fluidRow(
            column(
              width = 6,
              radioButtons("div_plot_type","Plot Type",choices = c("Vector Field","Animation")),#,selected = "Animation"),
              radioButtons("div_gradient","Show Divergence Gradient",choices = c("Yes" = T,"No" = F))
            )
            
          )
          )
      
    )
  )
)


dashboardPage(header, sidebar, body,skin = "black")