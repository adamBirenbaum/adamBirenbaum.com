
library(shiny)
library(shinydashboard)
library(ggplot2)
library(latex2exp)
library(dplyr)

header <- dashboardHeader(title = "Numerical Methods")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Roots",tabName = "roots"
              ),
              menuItem("Integration",tabName = "integration"
              ),
              menuItem("Derivatives")
              
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "roots",
      
      fluidRow(
        column(width = 3,
               box(
                 title = "Plot Parameters",width = NULL, status = "primary",
                 textInput("roots_function","f(x)",value = "7*exp(-x) + x*cos(3*x)-2"),
                 actionButton("random_function","Random Function"),
                 sliderInput("xrange","X-Range",min = -10, max = 10, step = 1, value = c(-5,5)),
                 sliderInput("yrange","Y-Range",min = -20, max = 20, step = 1, value = c(-10,10))
                 
               ),
               
               tabBox(width = NULL,id = "roots_tabset",
                      tabPanel("Newton-Raphson",
                               uiOutput("ui_guess"),
                               sliderInput("niter","Iterations",min = 1, max = 8,value = 3)
                               
                      ),
                      tabPanel("Bisection",
                                   h4("Bisection Limits"),
                                   textInput("bisect_start","Start",value = 2.5),
                                   textInput("bisect_end","End",value = 3.5),
                                   sliderInput("bisect_niter","Iterations",min = 1, max = 8,value = 3)
                                   
                               ),
                      tabPanel("Fixed-Point",
                               uiOutput("ui_guess2"),
                               sliderInput("fixed_niter","Iterations",min = 1, max = 8,value = 3)
                               )
                               
                               ),
               box(width = NULL,title = "Roots Table",
                 tableOutput("roots_df")
               )

        ),
        
        column(width = 9,
               box(width = NULL,status = "primary",
                   plotOutput("plot",height = "800px",dblclick = "plot_dblclick",
                              brush = brushOpts(
                                id = "plot_brush",
                                resetOnNew = TRUE
                              ))
               )
        )
        
      )
      #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      
      
      
    ),
    tabItem(
      tabName = "integration",
      fluidRow(
        column(width = 3,
               box(
                 title = "Plot Parameters",width = NULL, status = "primary",
                 textInput("integration_function","f(x)",value = "exp(-.3*x)*10*cos(x)"),
                 #textInput("integration_function","f(x)",value = ".2 + 25*x-200*x^2 +675*x^3-900*x^4+400*x^5"),
                 sliderInput("integration_xrange","X-Range",min = -10, max = 10, step = 1, value = c(0,10)),
                 sliderInput("integration_yrange","Y-Range",min = -20, max = 20, step = 1, value = c(-10,10))
                 
               ),
               
               tabBox(width = NULL,id = "integration_tabset",
                      tabPanel("Trapezoidal",
                               textInput("trapezoidal_lower_bound","Lower Bound",value = 0),
                               textInput("trapezoidal_upper_bound","Upper Bound",value = 10),
                               sliderInput("integration_nsubsets","Subsets",min = 1, max = 50,value = 6)
                               
                      ),
                      tabPanel("Simpson's 1/3",
                               textInput("simpson_third_lower_bound","Lower Bound", value = 0),
                               textInput("simpson_third_upper_bound","Upper Bound", value = 10),
                               sliderInput("simpson_integration_subsets","Subsets (Must be even for Simpson's 1/3)",min = 2, max = 50, step = 2, value = 16)
                               )
               ),
               box(width = NULL,title = "Summary Table",
                   tableOutput("integration_summary_df")
               )
               
        ),
        
        column(width = 9,
               box(width = NULL,status = "primary",
                   plotOutput("integration_plot",height = "800px",dblclick = "integration_plot_dblclick",
                              brush = brushOpts(
                                id = "integration_plot_brush",
                                resetOnNew = TRUE
                              ))
               )
        )
        
      )
        
      )

    )
  
  
  
  
)

dashboardPage(header, sidebar, body,skin = "black")