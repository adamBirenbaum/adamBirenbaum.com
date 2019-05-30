

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
  phrases <- c("You Go Girl!","Great Job!","Way to Go!","Hell Ya!","You are amazing!","Woohooo","Yahooo!","Holy Moly","Nice Work","Well Done!","Super Job!","Holy Cats!","Hod Dog, you are incredible!","Glory Hallelujah you are amazing!",
               "Out of Sight!","Good Golly!","JEEMINY CHRISTMAS!")
  get_streak <- function(x){
    
    streak <- 0
    current_streak <- 0
    for (i in x){
      
      if (i == 0){
        if (current_streak > streak) streak <- current_streak
        current_streak <- 0
      }else{
        current_streak <- current_streak + 1
      }
    }
    
    if (current_streak > streak) streak <- current_streak
    
    as.integer(streak)
  }
  
  
  output$plot <- renderPlot({
    miles_df <- read.csv(paste0(path_to_data,"total_miles.csv"),stringsAsFactors = F,colClasses = c("Date","numeric","numeric","numeric","numeric","numeric"))
    
    miles_df <- miles_df %>% filter(Date <= today_date) %>% mutate(Bo = cumsum(Bo),
                                                                   Rachael= cumsum(Rachael),
                                                                   Adam = cumsum(Adam),
                                                                   Jo = cumsum(Jo),
                                                                   Kelsey = cumsum(Kelsey)) %>% 
      gather(key = "Person",value = "Miles",Bo,Rachael,Adam,Jo,Kelsey)
 
    # browser()
    # miles_df2 <- miles_df %>% group_by(Person) %>% mutate(txt = c(0,diff(Miles)))
    # miles_df2$x <- 
    ggplot(miles_df, aes(x = Date, y = Miles, group = Person, color = Person)) + geom_line(size = 1) + theme_minimal() + ggtitle("Running Progress")+
      theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18),plot.title = element_text(size = 28),legend.text = element_text(size = 12),
            legend.position = "top") + scale_color_discrete("")
  })
  
  output$table <- renderTable({
    miles_df <- read.csv(paste0(path_to_data,"total_miles.csv"),stringsAsFactors = F,colClasses = c("Date","numeric","numeric","numeric","numeric","numeric"))
    miles_df <- miles_df %>% filter(Date <= today_date) 
    num_days <- nrow(miles_df) - 1
    table_df <- data.frame(Runner = c("Bo","Rachael","Adam","Jo","Kelsey"), "Total Miles" = c(sum(miles_df$Bo),sum(miles_df$Rachael), sum(miles_df$Adam),sum(miles_df$Jo),sum(miles_df$Kelsey)))


    
    table_df <- table_df %>% mutate("Miles/Day" = Total.Miles/num_days)
    table_df[["Longest Run"]] <- c(max(miles_df$Bo),max(miles_df$Rachael), max(miles_df$Adam),max(miles_df$Jo),max(miles_df$Kelsey))
    table_df[["Longest Streak"]] <- sapply(list(miles_df$Bo, miles_df$Rachael, miles_df$Adam, miles_df$Jo,miles_df$Kelsey), get_streak)
    table_df <- table_df[order(table_df$Total.Miles,decreasing = T),]
    names(table_df)[2] <- "Total Miles"
    table_df
    
  })
  
  
  observeEvent(input$enter,{
    miles_df <- read.csv(paste0(path_to_data,"total_miles.csv"),stringsAsFactors = F,colClasses = c("Date","numeric","numeric","numeric","numeric","numeric"))
    

    miles_df[[input$person]][miles_df$Date == today_date] <- input$miles +   miles_df[[input$person]][miles_df$Date == today_date]
    
    
    write.csv(miles_df,paste0(path_to_data,"total_miles.csv"),row.names = F)
    output$plot <- renderPlot({

      miles_df <- miles_df %>% filter(Date <= today_date) %>% mutate(Bo = cumsum(Bo),
                                                                     Rachael= cumsum(Rachael),
                                                                     Adam = cumsum(Adam),
                                                                     Jo = cumsum(Jo),
                                                                     Kelsey = cumsum(Kelsey)) %>% 
        gather(key = "Person",value = "Miles",Bo,Rachael,Adam,Jo,Kelsey)
      
      ggplot(miles_df, aes(x = Date, y = Miles, group = Person, color = Person)) + geom_line(size = 1) + theme_minimal() + ggtitle("Running Progress")+
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18),plot.title = element_text(size = 28),legend.text = element_text(size = 12),
              legend.position = "top")+ scale_color_discrete("")
    })
    
    output$table <- renderTable({
      miles_df <- read.csv(paste0(path_to_data,"total_miles.csv"),stringsAsFactors = F,colClasses = c("Date","numeric","numeric","numeric","numeric","numeric"))
      miles_df <- miles_df %>% filter(Date <= today_date) 
      num_days <- nrow(miles_df) - 1
      table_df <- data.frame(Runner = c("Bo","Rachael","Adam","Jo","Kelsey"), "Total Miles" = c(sum(miles_df$Bo),sum(miles_df$Rachael), sum(miles_df$Adam),sum(miles_df$Jo),sum(miles_df$Kelsey)))
      table_df <- table_df %>% mutate("Miles/Day" = Total.Miles/num_days)
      table_df[["Longest Run"]] <- c(max(miles_df$Bo),max(miles_df$Rachael), max(miles_df$Adam),max(miles_df$Jo),max(miles_df$Kelsey))
      table_df[["Longest Streak"]] <- sapply(list(miles_df$Bo, miles_df$Rachael, miles_df$Adam,miles_df$Jo,miles_df$Kelsey), get_streak)
      table_df <- table_df[order(table_df$Total.Miles,decreasing = T),]
      names(table_df)[2] <- "Total Miles"
      table_df
      
    })
    
    shinyWidgets::sendSweetAlert(session = session, title = NULL,text = tags$span(tags$br(),tags$br(),tags$h2(sample(phrases,size = 1))), type = "success")
    
  })
  
}