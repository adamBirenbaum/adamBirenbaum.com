

is_local <- ifelse(Sys.info()['nodename'] =="adamubuntu",T,F)

path_to_data <- ifelse(is_local, "~/adambirenbaum.com/public/project/run-tracker/", "/var/www/adambirenbaum.com/public/project/run-tracker/")


if (is_local){
  today_date <- Sys.Date()
}else{
  time <- Sys.time()
  time <- time - 3600*5
  today_date <- as.Date(time)
}


server <- function(input,output,session){
  phrases <- c("You Go Girl!","Great Job!","Way to Go!","Hell Ya!","You are amazing!","Woohooo","Yahooo!","Frick Ya!","Nice Work","Well Done!","Super Job!","Holy Cats!","Hod Dog, you are incredible!","Glory Hallelujah you are amaizng",
               "Out of Sight!","Good Golly!","JEEMINY CHRISTMAS!")
  
  
  
  output$plot <- renderPlot({
    miles_df <- read.csv(paste0(path_to_data,"total_miles.csv"),stringsAsFactors = F,colClasses = c("Date","numeric","numeric","numeric"))
    
    miles_df <- miles_df %>% filter(Date <= Sys.Date()) %>% mutate(Bo = cumsum(Bo),
                                                                   Rachael= cumsum(Rachael),
                                                                   Adam = cumsum(Adam)) %>% 
      gather(key = "Person",value = "Miles",Bo,Rachael,Adam)
  
    ggplot(miles_df, aes(x = Date, y = Miles, group = Person, color = Person)) + geom_line(size = 2) + theme_minimal() + ggtitle("Running Progress")+
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18),plot.title = element_text(size = 28),legend.text = element_text(size = 18))
  })
  
  output$table <- renderTable({
    miles_df <- read.csv(paste0(path_to_data,"total_miles.csv"),stringsAsFactors = F,colClasses = c("Date","numeric","numeric","numeric"))
    miles_df <- miles_df %>% filter(Date <= Sys.Date()) 
    num_days <- nrow(miles_df) - 1
    table_df <- data.frame(Runner = c("Bo","Rachael","Adam"), "Total Miles" = c(sum(miles_df$Bo),sum(miles_df$Rachael), sum(miles_df$Adam)))
    table_df <- table_df %>% mutate("Miles/Day" = Total.Miles/num_days)
    table_df[["Longest Run"]] <- c(max(miles_df$Bo),max(miles_df$Rachael), max(miles_df$Adam))
    table_df <- table_df[order(table_df$Total.Miles,decreasing = T),]
    names(table_df)[2] <- "Total Miles"
    table_df
    
  })
  
  
  observeEvent(input$enter,{
    miles_df <- read.csv(paste0(path_to_data,"total_miles.csv"),stringsAsFactors = F,colClasses = c("Date","numeric","numeric","numeric"))
    

    miles_df[[input$person]][miles_df$Date == Sys.Date()] <- input$miles +   miles_df[[input$person]][miles_df$Date == Sys.Date()]
    
    
    write.csv(miles_df,paste0(path_to_data,"total_miles.csv"),row.names = F)
    output$plot <- renderPlot({

      miles_df <- miles_df %>% filter(Date <= Sys.Date()) %>% mutate(Bo = cumsum(Bo),
                                                                     Rachael= cumsum(Rachael),
                                                                     Adam = cumsum(Adam)) %>% 
        gather(key = "Person",value = "Miles",Bo,Rachael,Adam)
      
      ggplot(miles_df, aes(x = Date, y = Miles, group = Person, color = Person)) + geom_line(size = 2) + theme_minimal() + ggtitle("Running Progress")+
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18),plot.title = element_text(size = 28),legend.text = element_text(size = 18))
    })
    
    output$table <- renderTable({
      miles_df <- read.csv(paste0(path_to_data,"total_miles.csv"),stringsAsFactors = F,colClasses = c("Date","numeric","numeric","numeric"))
      miles_df <- miles_df %>% filter(Date <= Sys.Date()) 
      num_days <- nrow(miles_df) - 1
      table_df <- data.frame(Runner = c("Bo","Rachael","Adam"), "Total Miles" = c(sum(miles_df$Bo),sum(miles_df$Rachael), sum(miles_df$Adam)))
      table_df <- table_df %>% mutate("Miles/Day" = Total.Miles/num_days)
      table_df[["Longest Run"]] <- c(max(miles_df$Bo),max(miles_df$Rachael), max(miles_df$Adam))
      table_df <- table_df[order(table_df$Total.Miles,decreasing = T),]
      names(table_df)[2] <- "Total Miles"
      table_df
      
    })
    
    shinyWidgets::sendSweetAlert(session = session, title = NULL,text = tags$span(tags$br(),tags$br(),tags$h2(sample(phrases,size = 1))), type = "success")
    
  })
  
}