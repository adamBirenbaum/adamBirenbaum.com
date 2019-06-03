library(shiny)
library(httr)


common_path <- "public/project/trivia_game/"
if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_trivia<<- paste0("/var/www/adambirenbaum.com/",common_path)
}else if(Sys.info()["sysname"] == "Windows"){
  path_to_trivia<<- "D:/abire/Documents/trivia_gamer/"
}else{
  path_to_trivia <<- paste0("~/adambirenbaum.com/",common_path)
  
}


successActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId,type = "button", class = "btn btn-success action-button", label,style = list('width' = width))
warningActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-warning action-button", label)
infoActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-info action-button", label)
dangerActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-danger action-button", label)
primaryActionButton <<- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-primary action-button", label)



fluidPage(
  fluidRow(
    column(width = 10,
           uiOutput("ui_start"),
           uiOutput("ui_new_game"),
           uiOutput("ui_join_game"),
           uiOutput("ui_wait_for_teams"),
           uiOutput("ui_game_board"),
           uiOutput("ui_question")
           )

  )
  
)