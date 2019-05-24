library(shiny)
library(httr)
library(ggplot2)

common_path <- "public/project/weather/"
if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_weather<<- paste0("/var/www/adambirenbaum.com/",common_path)
}else if(Sys.info()["sysname"] == "Windows"){
  path_to_weather<<- "D:/abire/Documents/weather/"
}else{
  path_to_weather<<- paste0("~/adambirenbaum.com/",common_path)
  
}


fluidPage(
  
  fluidRow(
    column(width = 6,
           textInput("zip","Zip Code", value = 53572),
           actionButton("enter","Get Weather")
           ),
    column(width = 3,
          uiOutput("error")
           )

  ),
  fluidRow(
    column(width = 10,
           tabsetPanel(type = "tabs",
                       tabPanel("Right Now",
                                fluidRow(
                                  column(width = 6, offset = 3,
                                         imageOutput("current_icon"),
                                         uiOutput("current_summary")
                                  )
                                  
                                  
                                  
                                )
                                
                       ),
                       
                       tabPanel("Hourly-Icons",
                                uiOutput("hourly_icons")    
                       ),
                       tabPanel("Hourly-Plots",
                                radioButtons("hourly_plots","Plot Options",choices = c("Temperature","Chance of Rain","Dew Point",
                                                                                       "Humidity","Pressure","Wind Speed","Cloud Cover"),selected = "Temperature",inline = T),
                                plotOutput("hour_plot")
                                ),
                       tabPanel("7 Day-Icons",
                                uiOutput("daily_icons")    
                                ),
                       tabPanel("7 Day-Plots",
                                radioButtons("daily_plots","Plot Options",choices = c("High Temperature","Low Temperature","Chance of Rain","Dew Point",
                                                                                       "Humidity","Pressure","Wind Speed","Cloud Cover"),selected = "High Temperature",inline = T),
                                plotOutput("daily_plot")
                                
                                )
                
           )
           )

    
  )
  
  
)