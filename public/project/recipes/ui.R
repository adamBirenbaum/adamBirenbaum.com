library(shiny)
library(dplyr)
library(tools)
library(shinyWidgets)
library(DT)


path_to_recipe <<- ifelse(Sys.info()['nodename'] =="DESKTOP-RFKFT3H", "D:/abire/Documents/recipes/", ifelse(Sys.info()['nodename'] == "adamubuntu", "~/adambirenbaum.com/public/project/recipes/","/var/www/adambirenbaum.com/public/project/recipes/")  )

categories <- read.csv(paste0(path_to_recipe,"categories.csv"),stringsAsFactors = F)
#preparation <-  c("",read.csv("D:/abire/Documents/recipes/preparation.csv",stringsAsFactors = F)[,1])

successActionButton <- function(inputId,label,width = NULL) tags$button(id = inputId,type = "button", class = "btn btn-success action-button", label,style = list('width' = width))
warningActionButton <- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-warning action-button", label)
infoActionButton <- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-info action-button", label)
dangerActionButton <- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-danger action-button", label)
primaryActionButton <- function(inputId,label,width = NULL) tags$button(id = inputId, type = "button", class = "btn btn-primary action-button", label)


ui <- navbarPage(
  theme = "paper",
  "Recipes",
  
  tabPanel("View",
           h3("Filter Recipes"),
           fluidRow(
             column(width = 3,
                    textInput("filter_name",label = "Name")
                    ),
             column(width = 3,
                    selectInput("filter_tag",label = "Tag",choices = categories,selected = NULL,multiple = T)
                    )
           ),
           fluidRow(
             column(width = 5,offset = 3,
                    DT::dataTableOutput("data")   
                    )
           ),
           fluidRow(
             column(width = 6,
                    uiOutput("view_recipe")
                    )
           )

           ),
  
  tabPanel("Add",
           fluidRow(
             column(width = 4,
                    textInput("add_title", label = h4("Title"))
                    ),
             column(width = 4,
                    selectizeInput("add_tag", label = h4("Tag"), choices = categories,multiple = T)
                    ),
             column(width = 3,
                    fileInput("add_img",label = h4("Image (optional)"))
                    )
           ),
           fluidRow(
             column(width = 3,
                    radioGroupButtons(
                      inputId = "add_time",
                      label = h5("Time Estimate"),
                      choices = c("< 1 hour", 
                                  "1-2 hours", "> 2 hours"),
                      status = "primary",
                      checkIcon = list(
                        yes = icon("ok", 
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon"))
                    )
                    ),
             column(width = 3,
                    radioGroupButtons(
                      inputId = "add_cheap",
                      label = h5("Cost"),
                      choices = c("$", "$$","$$$"),
                      status = "primary",
                      checkIcon = list(
                        yes = icon("ok", 
                                   lib = "glyphicon"),
                        no = icon("remove",
                                  lib = "glyphicon"))
                    )
                    )
           ),
           fluidRow(
             column(
               width = 12,
               hr()
             )
           ),
           fluidRow(
             column(width = 8,
                    splitLayout(cellWidths = c("10%","10%","10%","10%","20%","10%","20%"),
                                
                                column(width = 12,br(),br(),
                                       actionBttn("add_1_8",label = "1/8",color = "default",style = "material-flat",block = T),br(),br(),
                                       #successActionButton("add_1_8",label = "1/8",width = "100%"),br(),br(),
                                       actionBttn("add_1",label = "1",color = "default",style = "material-flat",block = T),br()
                                ),         
                                column(width = 12,br(),br(),
                                       actionBttn("add_1_4",label = "1/4",color = "default",style = "material-flat",block = T),br(),br(),
                                       actionBttn("add_2",label = "2",color = "default",style = "material-flat",block = T),br()
                                ),
                                column(width = 12,br(),br(),
                                       actionBttn("add_1_3",label = "1/3",color = "default",style = "material-flat",block = T),br(),br(),
                                       actionBttn("add_5",label = "5",color = "default",style = "material-flat",block = T),br()
                                ),
                                column(width = 12,br(),br(),
                                       actionBttn("add_1_2",label = "1/2",color = "default",style = "material-flat",block = T),br(),br(),
                                       actionBttn("add_10",label = "10",color = "default",style = "material-flat",block = T),br()
                                ),
                                column(width = 12,
                                       textInput("add_amount",label = h5("Amount"),value = "0"),
                                       actionBttn("add_reset_amount",label = "Reset",color = "success",style = "simple",block = T)
                                ),
                                column(width = 12,
                                       actionBttn("add_tsp",label = "tsp",style = "bordered",color = "royal",block = T,size = "sm"),
                                       actionBttn("add_tab",label = "T",style = "bordered",color = "royal",block = T,size = "sm"),
                                       actionBttn("add_c",label = "c.",style = "bordered",color = "royal",block = T,size = "sm"),
                                       actionBttn("add_quart",label = "qt.",style = "bordered",color = "royal",block = T,size = "sm"),
                                         actionBttn("add_oz",label = "oz",style = "bordered",color = "royal",block = T,size = "sm"),
                                       actionBttn("add_g",label = "g",style = "bordered",color = "royal",block = T,size = "sm")
   
                                ),
                                column(width = 12,
                                       textInput("add_unit",label = h5("Unit"), value = "")
                                )
                    )
             ),
             
             column(width = 2,
                    textInput("add_ingredient",label = h5("Ingredient"),value = ""),
                    br(),
                   actionBttn("add_add_ingredient","Add Ingredient",color = "success",style = "simple",block = T)

                      

                    ),
             column(width = 2,
                    textInput("add_preparation",label = h5("Prep. (optional"),value = "")
                    
                    )

             

           ),

           fluidRow(
             column(
               width = 12,
               hr()
             )
           ),
           fluidRow(
             column(width = 4,
                    textAreaInput("add_area_ingred",label = h5("Added Ingredients"),height = '300px')
             ),
             column(width = 6, offset = 1,
                    textAreaInput("add_directions",label = h5("Directions"),height = '300px',width = '600px')
             )
           ),
           fluidRow(
             column(width = 2,
                    actionBttn("add_make_recipe","Make Recipe",color = "success",style = "simple",block = T)
                    )
           ),
           fluidRow(
             column(width = 12,
                    uiOutput("add_recipe")
                    )
             
           )

           
           )
  
  
)