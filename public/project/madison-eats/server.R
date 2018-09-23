
server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  start <- T
  
  output$ui_data_source <- renderUI({
    common_path <<- "public/project/madison-eats/"
    if (Sys.info()["nodename"] == "ADAM-DROPLET"){
      path_to_main <<- paste0("/var/www/adambirenbaum.com/",common_path)
    }else{
      path_to_main <<- paste0("~/adambirenbaum.com/",common_path)
    }
    
    
    all_files <- list.files(path = paste0(path_to_main,"data_folder/"))
    counties3 <- gsub("(.*)_yelp_df.csv","\\1",all_files)
    
    
    selectizeInput("data_source","Counties",choices = counties3,multiple = T,selected = "Dane-WI")
    
  })
  
  
  yelp_data <- reactive({
    req(input$data_source)
    counties <- input$data_source
    
    county_file <- paste0(counties,"_yelp_df.csv")
    
    yelp_data <- read.csv(paste0(path_to_main,"data_folder/",county_file[1]),stringsAsFactors = F)
    if (length(county_file) > 1){
      for (i in 2:length(county_file)){
        yelp_data <- rbind(yelp_data,read.csv(paste0(path_to_main,"data_folder/",county_file[i]),stringsAsFactors = F))
      }
      
    }
    yelp_data
  })
  

  output$ui_category <- renderUI({
    
    categ <- gsub("|",",",yelp_data()$Categories,fixed = T)
    
    categ_list <<- strsplit(categ," , ")
    names(categ_list) <<- yelp_data()$Name
    
    # want the most popular tags towards the front... but not fast food
    sorted_categories <- names(sort(table(unlist(categ_list)),decreasing = T))
    sorted_categories <- sorted_categories[sorted_categories != "Fast Food"]
    sorted_categories <- c(sorted_categories, "Fast Food")
    
    
    selectizeInput("category","Filter by Categories",choices = sorted_categories,multiple = T)
    
  })
  
  filter_rating <- reactive(input$rating)
  filter_review <- reactive(input$n_reviews)
  filter_categ <- reactive(input$category)
  
  output$mymap <- renderLeaflet({
    
    
    if (length(filter_categ()) != 0){
      matrix_categ_in_rest <- sapply(filter_categ(), function(x) grepl(x,categ_list,fixed=T))
      cat_in_rest <- apply(matrix_categ_in_rest,1,function(x) any(x))
      new_data <- yelp_data()[cat_in_rest,]
      
      new_data <- new_data %>% filter(Rating >= filter_rating(), 
                                      Review_Count >= filter_review())
    }else{
      
      new_data <- yelp_data() %>% filter(Rating >= filter_rating(), 
                                         Review_Count >= filter_review())
    }
    
    
    #browser()
    if (start){
      leaflet(data = new_data) %>% fitBounds(lng1 = min(new_data$Longitude),lng2 = max(new_data$Longitude),lat1= min(new_data$Latitude), lat2 = max(new_data$Latitude)) %>% 
        addProviderTiles(providers$Wikimedia) %>% 
        addMarkers(~Longitude, ~ Latitude, popup = paste0("<a href='",new_data$URL,"' target='_blank'>",new_data$Name,"</a>:  <br> ",new_data$Categories,"<br>",new_data$Rating," &#9733 &nbsp; &nbsp;",new_data$Price,"<br> ",new_data$Review_Count," reviews"))
      
      
    }else{
      leaflet(data = new_data) %>% flyToBounds(lng1 = min(new_data$Longitude),lng2 = max(new_data$Longitude),lat1= min(new_data$Latitude), lat2 = max(new_data$Latitude)) %>% 
        addProviderTiles(providers$Wikimedia) %>% 
        addMarkers(~Longitude, ~ Latitude, popup = paste0("<a href='",new_data$URL,"' target='_blank'>",new_data$Name,"</a>:  <br> ",new_data$Categories,"<br>",new_data$Rating," &#9733 &nbsp; &nbsp;",new_data$Price,"<br> ",new_data$Review_Count," reviews"))
      
      
      
    }
  })
  
  
}