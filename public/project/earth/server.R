server <- function(input,output){
  
  print(paste0(path_to_gif,"earth.gif"))
  output$gif <- renderImage(list(src = paste0(path_to_gif,"earth.gif"),contentType = 'image/gif'),deleteFile = F)
  
}