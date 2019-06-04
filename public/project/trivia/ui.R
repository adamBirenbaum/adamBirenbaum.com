library(shiny)
library(httr)
library(shinyWidgets)
library(shinydashboard)



if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_trivia <<- "/var/www/adambirenbaum.com/"
}else if(Sys.info()["sysname"] == "Windows"){
  path_to_trivia <<- "D:/abire/Documents/"
}else{
  path_to_trivia <<- "~/adambirenbaum.com/"
  
}


successActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId,type = "button", class = "btn btn-success action-button", label,style = list('width' = width))
warningActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-warning action-button", label)
infoActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-info action-button", label)
dangerActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-danger action-button", label)
primaryActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-primary action-button", label)



fluidPage(
  useShinydashboard(),
  fluidRow(
    column(width = 10,
           uiOutput("ui_start"),
           uiOutput("ui_new_game"),
           uiOutput("ui_join_game"),
           uiOutput("ui_wait_for_teams"),
           uiOutput("ui_game_board")

           )

  ),
  fluidRow(
    
    column(width = 10,
           uiOutput("ui_up_now"),
           uiOutput("ui_question")
           )

  )
  
)