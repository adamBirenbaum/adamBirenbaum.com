server <- function(input, output,session){
  new_image_path <- NULL
  
  
  convert_to_fraction <- function(x){
    
    x <- as.numeric(x)
    
    whole <- floor(x)

    
    frac <- x %% 1
    if (frac != 0){
      frac <- round(frac, digits = 3)
      
      frac <- switch(as.character(frac),"0.125" = "1/8","0.25" = "1/4", "0.333" = "1/3","0.375" = "3/8","0.5" = "1/2", "0.625" = "5/8",
                     "0.667" = "2/3","0.75" = "3/4","0.875" = "7/8","0.083" = "1/12", "0.167" = "1/6",  "0.417" = "5/12","0.583" = "7/12", 
                      "0.833" = "5/6", "0.917" = "11/12","0.042" = "1/24","0.208" = "5/24", "0.292" = "7/24",  "0.458" = "11/24",  
                     "0.542" = "13/24", "0.708" = "17/24", "0.792" = "19/24", "0.958" = "23/24")
      
    }

    if (whole == 0){
      return(frac)
    }else if (frac == 0){
      return(as.character(whole))
    } else return(paste(whole,frac))
  }
  
  convert_to_decimal <- function(x){
    split_x <- strsplit(x," ")[[1]]
    sapply(split_x,function(x) eval(parse(text = x)),USE.NAMES = F) %>% sum()
  }
  
  new_val <- function(x,incr){
    if (x == "") x <- "0"
    x <- convert_to_decimal(x)
    x <- x + incr
    convert_to_fraction(x)

  }
  
  
  observeEvent(input$add_1_8,{
    updateTextInput(session = session,inputId = "add_amount",value = new_val(input$add_amount,1/8))
    
  })
  
  observeEvent(input$add_1_4,{
    updateTextInput(session = session,inputId = "add_amount",value = new_val(input$add_amount,1/4))
    
  })
  observeEvent(input$add_1_3,{
    updateTextInput(session = session,inputId = "add_amount",value = new_val(input$add_amount,1/3))
    
  })
  
  observeEvent(input$add_1_2,{
    updateTextInput(session = session,inputId = "add_amount",value = new_val(input$add_amount, 1/2))
    
  })
  
  observeEvent(input$add_1,{
    updateTextInput(session = session,inputId = "add_amount",value = new_val(input$add_amount, 1))
    
  })
  
  observeEvent(input$add_2,{
    updateTextInput(session = session,inputId = "add_amount",value = new_val(input$add_amount, 2))
    
  })
  
  observeEvent(input$add_5,{
    updateTextInput(session = session,inputId = "add_amount",value = new_val(input$add_amount, 5))
    
  })
  
  observeEvent(input$add_10,{
    updateTextInput(session = session,inputId = "add_amount",value = new_val(input$add_amount, 10))
    
  })
  

  
  observeEvent(input$add_reset_amount,{
    updateTextInput(session = session,inputId = "add_amount",value = 0)
  })
  
  
  
  
  
  
  observeEvent(input$add_tsp,{
    updateTextInput(session = session,inputId = "add_unit",value = "tsp")
  })
  
  observeEvent(input$add_tab,{
    updateTextInput(session = session,inputId = "add_unit",value = "T")
  })
  
  observeEvent(input$add_c,{
    updateTextInput(session = session,inputId = "add_unit",value = "c.")
  })
  
  observeEvent(input$add_quart,{
    updateTextInput(session = session,inputId = "add_unit",value = "quarts")
  })
  
  observeEvent(input$add_oz,{
    updateTextInput(session = session,inputId = "add_unit",value = "oz.")
  })
  
  observeEvent(input$add_g,{
    updateTextInput(session = session,inputId = "add_unit",value = "g")
  })
  
  observeEvent(input$add_add_ingredient,{
    str_ingr <- paste(input$add_amount,input$add_unit, input$add_ingredient,input$add_preparation)
    if (input$add_area_ingred == ""){
      updateTextAreaInput(session = session, inputId = "add_area_ingred",value = str_ingr)
    }else{
      updateTextAreaInput(session = session, inputId = "add_area_ingred",value = paste0(input$add_area_ingred,"\n",str_ingr))
    }
    
    updateTextInput(session = session,inputId = "add_amount",value = 0)
    updateTextInput(session = session,inputId = "add_unit",value = "")
    updateSelectInput(session = session,inputId = "add_preparation",selected = "")
    updateSelectInput(session = session,inputId = "add_ingredient",selected = "")
    
  })
  
  short <- function(x) paste(x, collapse = ", ")
  
  observeEvent(input$add_make_recipe,{
    

    if (is.null(input$add_img$datapath)){
      str_image <- ""
    }else{
      str_image <- input$add_img$datapath
      
      new_title <- gsub(" ","_",input$add_title,fixed = T)
      new_image_path <<- paste0(path_to_recipe,"img/",new_title,".",file_ext(str_image))

      file.rename(str_image,new_image_path)
      str_image <- paste0("img/",new_title,".",file_ext(str_image))
    } 
    
    
    df <- data.frame(Name = input$add_title, Tag = short(input$add_tag), Image = str_image, Ingredients = input$add_area_ingred, Directions = input$add_directions,
                     Time = input$add_time, Cost = input$add_cheap, stringsAsFactors = F)
      
    old_df <- read.csv(paste0(path_to_recipe,"recipe_database.csv"),stringsAsFactors = F)
    df <- rbind(old_df,df)
    write.csv(df,paste0(path_to_recipe,"recipe_database.csv"),row.names = F)
    rmarkdown::render(paste0(path_to_recipe,"recipe.Rmd"))
    file.rename(paste0(path_to_recipe,"recipe.html"),paste0(path_to_recipe,"recipe_html/",input$add_title,".html"))
    output$add_recipe <- renderUI(
        includeHTML(paste0(path_to_recipe,"recipe_html/",input$add_title,".html"))  
    )
    
    updateTextInput(session = session,inputId = "add_title",value = "")
    updateSelectizeInput(session = session,inputId = "add_tag",selected = NULL)
    updateTextAreaInput(session = session,inputId = "add_area_ingred",value = "")
    updateTextAreaInput(session = session,inputId = "add_directions",value = "")
    

  })
  
  
  make_buttons <- function(FUN, len, id,label,...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i),label = label[i],...))
    }
    inputs
  }
  
  df <- reactiveValues(data = read.csv(paste0(path_to_recipe,"recipe_database.csv"),stringsAsFactors = F)[,c(1,2,6,7)])
  observe(
    
    df$data$Recipes <- make_buttons(actionButton,nrow(df$data),"recipe_button_",df$data$Name,onclick = 'Shiny.setInputValue(\"select_button\",  this.id,{priority: \"event\"})')
    
  )
  

  observeEvent(input$select_button,{
    
    button_index <- as.numeric(gsub("recipe_button_([0-9]+)$","\\1",input$select_button))
    
    output$view_recipe <- renderUI(
      includeHTML(paste0(path_to_recipe,"recipe_html/",df$data$Name[button_index],".html")) 
    )
    
  })
  

  
  output$data <- DT::renderDataTable(
    df$data, server = FALSE, escape = FALSE, selection = 'none',options = list(searching = F,pageLength = 10)
  )
  
  observeEvent(c(input$filter_name,input$filter_tag),{
    new_data <- df$data %>% filter(grepl(input$filter_name,tolower(Name),fixed = T))
    if(!is.null(input$filter_tag)){
   
      tag_list <- strsplit(new_data$Tag,",")
      tag_list <- lapply(tag_list,function(x) gsub("^ (.*$)","\\1",x))
      contains_tag <- sapply(tag_list, function(x) any(input$filter_tag %in% x))
      new_data <- new_data[contains_tag,]
    }

    output$data <- DT::renderDataTable(new_data
      , server = FALSE, escape = FALSE, selection = 'none',options = list(searching = F,pageLength = 10)
    )
    
  })
  
  
  
  
}