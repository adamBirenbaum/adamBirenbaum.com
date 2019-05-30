library(shiny)

common_path <- "public/project/earth/"
if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_gif<<- paste0("/var/www/adambirenbaum.com/",common_path)
}else if(Sys.info()["sysname"] == "Windows"){
  path_to_gif<<- "D:/abire/Documents/Earth/"
}else{
  path_to_gif<<- paste0("~/adambirenbaum.com/",common_path)
  
}

current_date <- read.table(paste0(path_to_gif,"date.txt"),stringsAsFactors = F)$V1

fluidPage(
  fluidRow(
    column(width=3, offset = 3,
           h2(current_date)
           )
  ),
  fluidRow(
    column(width = 12,
           
           imageOutput("gif")
           )
  )
  
)