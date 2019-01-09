library(shiny)
library(ggplot2)
library(gganimate)


common_path <- "public/project/solar-system-flyby/"
if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_main <<- paste0("/var/www/adambirenbaum.com/",common_path)
}else{
  path_to_main <<- paste0("~/adambirenbaum.com/",common_path)
}

ui <- fluidPage(
  mainPanel(
   
    fluidRow(
      column(width = 4, 
             sliderInput("velx",label = h4("X-Velocity"),min = 0,max = 60000,value = 30000),
             sliderInput("vely",label = h4("Y-Velocity"),min = 0,max = 60000,value = 30000),
             sliderInput("nframes",label = h4("Time Steps"),min = 500,max = 20000,value = 1000)
             
             ),
             
             
    column(width = 8,
           imageOutput("animation_gif")
           )
    ),
    fluidRow(
      column(width = 2,
             actionButton("animate","Animate")
             )
    ),
    br(),
    br(),
    
    fluidRow(
      column(width = 12,
             plotOutput("static_trajectory")
            
             )
    )
    ) 
    
  )
    
  
  
