zip_coords <- read.csv(paste0(path_to_weather,"zip_coords.csv"),stringsAsFactors = F)

function(input,output,session){
  
  get_icon_list <- function(x,size){
    list(src = paste0(path_to_weather,"/www/",x,".png"),contentType = "image/png",
         width = size, height = size)
    
  }
  
  get_image <- function(x){
    img(src = paste0(x,".png"),width = 40, height = 40,contentType = "image/png")
  }
  
  
  make_icon <- function(x,size = 50){
    renderImage(get_icon_list(x,size),deleteFile = F)
  }
  
  extract_from_list <- function(x,col_name){
    sapply(x$data, function(y) y[[col_name]])
  }
  
  observeEvent(input$city,{
    
    zip <- switch (input$city,
      'Mt Horeb' = 53572,
      'Madison' = 53719,
      'Stoughton'=53589,
      'Plymouth' = 53073
    )
    updateTextInput(session,"zip","Zip Code",value = zip)

  })
  
  make_time_plot <- function(hour,col_name,is_hour = T,first_period = T){
    
    hour_y <- extract_from_list(hour,col_name)
    hour_times <- extract_from_list(hour,"time")
    col_name <- switch(col_name,
                       "temperature"="Temperature",
                       "temperatureHigh" = "High Temperature",
                       "temperatureLow" = "Low Temperature",
                       "precipProbability" ="Chance of Rain",
                       "dewPoint"="Dew Point",
                       "humidity" = "Humidity" ,
                       "pressure" = "Pressure",
                       "windSpeed" = "Wind Speed" ,
                       "cloudCover" = "Cloud Cover")
    nn <- length(hour_times)
    if (is_hour){
      hour_times <- format(as.POSIXct(hour_times,origin = "1970-01-01",tz = "America/Chicago"),format = "%a %I:%M %p")
      blank_seq <- seq(from = 1, to = nn,by = 6)
      title <- "Hourly "
    }else{
      hour_times <- format(as.POSIXct(hour_times,origin = "1970-01-01",tz = "America/Chicago"),format = "%a, %b %d")
      blank_seq <- seq(from = 1, to = nn,by = 1)
      title <- "Daily "
    }
    
    full_times <- hour_times
    hour_times[!(1:nn) %in% blank_seq] <- ""
    df_hour <- data.frame(Time = 1:length(hour_times),y = hour_y)
    names(hour_times) <- 1:length(hour_times)
    
    if (is_hour){
      if (first_period){
        inds <- 1:25
      }else inds <- 26:50
      
      hour_times <- hour_times[inds]
      full_times <- full_times[inds]
      df_hour <- df_hour[inds,]
      inds <- 1:25
      df_hour$Time <- inds
      
    }else{
      inds <- 1:length(hour_times)
    }

    df2 <- make_annotation_df(df_hour,full_times)
   
    if (!is.null(df2)){
      ggplot(df_hour,aes(x = Time, y = y)) + geom_line() +  scale_x_continuous(breaks = inds,label = hour_times) + 
        theme_bw() + ggtitle(paste0(title,col_name)) + ylab(col_name) + geom_text(data = df2, aes(x = x, y = y, label= text),size = 3)
    }else{
      ggplot(df_hour,aes(x = Time, y = y)) + geom_line() + scale_x_continuous(breaks = inds,label = hour_times) + 
        theme_bw() + ggtitle(paste0(title,col_name)) + ylab(col_name)
    }
    
    
  }
  
  make_annotation_df <- function(df,times){
    y <- df$y
    x <- times
    local_max <- local_maxima(y)
    local_min <- local_minima(y)
    
    df <- data.frame(x = numeric(0), y = numeric(0),text = character(0))
    
    scale_factor <- (max(y) - min(y))*.05
    if (length(local_max) > 0){
      df <- rbind(df, data.frame(x = local_max, y = y[local_max] + scale_factor,text = paste0(y[local_max],' \n ',x[local_max])))
    }
    
    if (length(local_min) > 0){
      df <- rbind(df, data.frame(x = local_min, y = y[local_min] - scale_factor,text = paste0(y[local_min]," \n ",x[local_min])))
    }
    
    if (nrow(df) > 0){
      # browser()
      # diff_vec <- diff(df$x)
      # ind_vec <- (2:(length(diff_vec)+1))[diff_vec > 3]
      # df[]
      return(df)
    }else return(NULL)
  }
  
  local_minima<- function(x) {
    # Use -Inf instead if x is numeric (non-integer)
    y <- diff(c(Inf, x)) < 0L
    rle(y)$lengths
    y <- cumsum(rle(y)$lengths)
    y <- y[seq.int(1L, length(y), 2L)]
    if (x[[1]] == x[[2]]) {
      y <- y[-1]
    }
    y[!(y == 1 | y == length(x))]
    
  }
  
  local_maxima <- function(x) {
    # Use -Inf instead if x is numeric (non-integer)
    y <- diff(c(-Inf, x)) > 0L
    rle(y)$lengths
    y <- cumsum(rle(y)$lengths)
    y <- y[seq.int(1L, length(y), 2L)]
    if (x[[1]] == x[[2]]) {
      y <- y[-1]
    }
    
    y[!(y == 1 | y == length(x))]
  }
  
  
  weather_data <- eventReactive(input$enter,{
    zip <- input$zip
    
    zip_id <- grep(zip,zip_coords$Zip)
    if (length(zip_id) == 0) return(NULL)
    lat <- zip_coords$Latitude[zip_id]
    long <- zip_coords$Longitude[zip_id]
    
    key <- "5278dff938af0d98c6290804dacda8d4"
    
    xx <- try(GET(url = paste0("https://api.darksky.net/forecast/",key,"/",lat,",",long)))
    bad <- T
    while(bad){
      if (inherits(xx,"try-error")){
        Sys.sleep(1.5)
      }else bad <- F
      xx <- try(GET(url = paste0("https://api.darksky.net/forecast/",key,"/",lat,",",long)))
    }
    
    xx <- content(xx,"parsed")
    
    
    list(current = xx$currently,
         minute =xx$minutely,
         hour = xx$hourly,
         day = xx$daily)
    
  })
  
  output$ui_hour_button <- renderUI(successActionButton("next_hour_button","-> Next 24 Hours"))
  
  
  observe({
    
    weather <- weather_data()
    
    if (is.null(weather)){
      output$error <- renderUI(h2("Error! Bad zip provided"))
      return(NULL)
    }else{
      output$error <- renderUI(h2(""))
      
      output$current_summary <- renderUI(
          tagList(
          h2(weather$current$summary),
          h2(paste0(weather$current$temperature," °F")),
          h3(paste0("Chance of rain: ",weather$current$precipProbability*100,"%")),
          h3(paste0("Humidity: ",weather$current$humidity*100,"%")),
          h3(weather$minute$summary)
        )
      )
      output$ui_hour_button <- renderUI(successActionButton("next_hour_button","-> Next 24 Hours"))
      output$current_icon <- make_icon(weather$current$icon,200)
      
      hour_plot_option <- switch(input$hourly_plots,
                                 "Temperature" = "temperature",
                                 "Chance of Rain" = "precipProbability",
                                 "Dew Point" = "dewPoint",
                                 "Humidity" = "humidity",
                                 "Pressure" = "pressure",
                                 "Wind Speed" = "windSpeed",
                                 "Cloud Cover" = "cloudCover"
      )
      
      
     
      output$hour_plot <- renderPlot(make_time_plot(weather$hour,hour_plot_option))
      
      
      
      output$hourly_icons <- renderUI({
        
        
        hour_icons <- extract_from_list(weather$hour,"icon")
        hour_times <- extract_from_list(weather$hour,"time")
        hour_times <- format(as.POSIXct(hour_times,origin = "1970-01-01",tz = "America/Chicago"),format = "%a %I:%M %p")
        hour_temp <- extract_from_list(weather$hour,"temperature")
        hour_precip <- paste0(extract_from_list(weather$hour,"precipProbability")*100,"%")
        hour_summary <- paste0(extract_from_list(weather$hour,"summary"))
        
        make_hourly_icons <- function(icon,time,temp,precip,summary){
          tagList(
            
            
            fluidRow(
              column(width = 1,
                     h6(time)
              ),
              column(width = 1,
                     get_image(icon),
                     h6(summary)
              ),
              column(width = 2,
                     h6(paste0(temp," °F")),
                     h6(paste0("Chance of Rain: ",precip))
                     
              )
            ),
            hr()
            
          )
        }
        
        tagList(mapply(FUN = make_hourly_icons,hour_icons,hour_times,hour_temp,hour_precip,hour_summary,SIMPLIFY = F))
        
        
        
      })
      
    }
    
    
    day_plot_option <- switch(input$daily_plots,
                              "High Temperature" = "temperatureHigh",
                              "Low Temperature" = "temperatureLow",
                              "Chance of Rain" = "precipProbability",
                              "Dew Point" = "dewPoint",
                              "Humidity" = "humidity",
                              "Pressure" = "pressure",
                              "Wind Speed" = "windSpeed",
                              "Cloud Cover" = "cloudCover"
    )
    
    output$daily_plot <- renderPlot(make_time_plot(weather$day,day_plot_option,is_hour = F))
    
    output$daily_icons <- renderUI({
      day_icons <- extract_from_list(weather$day,"icon")
      day_times <- extract_from_list(weather$day,"time")
      day_times <- format(as.POSIXct(day_times,origin = "1970-01-01",tz = "America/Chicago"),format = "%a, %b %d")
      day_temp_high <- extract_from_list(weather$day,"temperatureHigh")
      day_temp_low <- extract_from_list(weather$day,"temperatureLow")
      day_precip <- paste0(extract_from_list(weather$day,"precipProbability")*100,"%")
      day_summary<- extract_from_list(weather$day,"summary")
      
      make_day_icons <- function(icon,time,high_temp,low_temp,precip,summary){
        tagList(
          fluidRow(
            column(width = 1,
                   h5(time)
            ),
            column(width = 1,
                   get_image(icon),
                   h6(summary)
            ),
            column(width = 2,
                   h6(paste0("High: ",high_temp," °F")),
                   h6(paste0("Low: ",low_temp," °F")),
                   h6(paste0("Chance of Rain: ",precip))
            )

          ),
          hr()
        )
      }
      
      
      tagList(mapply(FUN = make_day_icons,day_icons,day_times,day_temp_high,day_temp_low,day_precip,day_summary,SIMPLIFY = F))
    })
    
    
  })
  
  observeEvent(input$next_hour_button,{
    
    output$ui_hour_button <-  renderUI(successActionButton("prev_hour_button","<- Prev. 24 Hours"))
    
    weather <- weather_data()
    
    if (is.null(weather)){
      output$error <- renderUI(h2("Error! Bad zip provided"))
      return(NULL)
    }
    
    hour_plot_option <- switch(isolate(input$hourly_plots),
                               "Temperature" = "temperature",
                               "Chance of Rain" = "precipProbability",
                               "Dew Point" = "dewPoint",
                               "Humidity" = "humidity",
                               "Pressure" = "pressure",
                               "Wind Speed" = "windSpeed",
                               "Cloud Cover" = "cloudCover"
    )
    output$hour_plot <- renderPlot(make_time_plot(weather$hour,hour_plot_option,first_period = F))
    
    
  })
  
  observeEvent(input$prev_hour_button,{
    output$ui_hour_button <-  renderUI(successActionButton("next_hour_button","-> Next 24 Hours"))
    
    weather <- weather_data()
    
    if (is.null(weather)){
      output$error <- renderUI(h2("Error! Bad zip provided"))
      return(NULL)
    }
    
    hour_plot_option <- switch(isolate(input$hourly_plots),
                               "Temperature" = "temperature",
                               "Chance of Rain" = "precipProbability",
                               "Dew Point" = "dewPoint",
                               "Humidity" = "humidity",
                               "Pressure" = "pressure",
                               "Wind Speed" = "windSpeed",
                               "Cloud Cover" = "cloudCover"
    )
    output$hour_plot <- renderPlot(make_time_plot(weather$hour,hour_plot_option,first_period = T))
    
  })
  
  # observeEvent(input$hourly_plots,{
  #   
  #   if (!exists("hour")){
  #     good <- weather_data()
  #     
  #     if (!good){
  #       output$error <- renderUI(h2("Error! Bad zip provided"))
  #       return(NULL)
  #     }else{
  #       output$error <- renderUI(h2(""))
  #     }
  #   } 
  #   
  #   
  #   
  #   plot_option <- switch(input$hourly_plots,
  #                         "Temperature" = "temperature",
  #                         "Chance of Rain" = "precipProbability",
  #                         "Dew Point" = "dewPoint",
  #                         "Humidity" = "humidity",
  #                         "Pressure" = "pressure",
  #                         "Wind Speed" = "windSpeed",
  #                         "Cloud Cover" = "cloudCover"
  #   )
  #   
  #   output$hour_plot <- renderPlot(make_time_plot(hour,plot_option))
  # })
  # output$hourly_icons <- renderUI({
  #   
  #   
  #   if (!exists("hour")){
  #     good <- weather_data()
  #     
  #     if (!good){
  #       output$error <- renderUI(h2("Error! Bad zip provided"))
  #       return(NULL)
  #     }else{
  #       output$error <- renderUI(h2(""))
  #     }
  #   } 
  #   
  #   hour_icons <- extract_from_list(hour,"icon")
  #   hour_times <- extract_from_list(hour,"time")
  #   hour_times <- format(as.POSIXct(hour_times,origin = "1970-01-01",tz = "America/Chicago"),format = "%a %I:%M %p")
  #   hour_temp <- extract_from_list(hour,"temperature")
  #   hour_precip <- paste0(extract_from_list(hour,"precipProbability")*100,"%")
  #   
  #   make_hourly_icons <- function(icon,time,temp,precip){
  #     tagList(
  #       
  #       
  #       fluidRow(
  #         column(width = 1,
  #                h6(time)
  #         ),
  #         column(width = 1,
  #                get_image(icon)
  #         ),
  #         column(width = 2,
  #                h6(paste0(temp," °F")),
  #                h6(paste0("Chance of Rain: ",precip))
  #         )
  #       ),
  #       hr()
  #       
  #     )
  #   }
  #   
  #   
  #   tagList(mapply(FUN = make_hourly_icons,hour_icons,hour_times,hour_temp,hour_precip,SIMPLIFY = F))
  #   
  #   
  #   
  # })
  
  # observeEvent(input$daily_plots,{
  #   
  #   if (!exists("hour")){
  #     good <- weather_data()
  #     
  #     if (!good){
  #       output$error <- renderUI(h2("Error! Bad zip provided"))
  #       return(NULL)
  #     }else{
  #       output$error <- renderUI(h2(""))
  #     }
  #   } 
  #   
  #   
  #   
  #   plot_option <- switch(input$daily_plots,
  #                         "High Temperature" = "temperatureHigh",
  #                         "Low Temperature" = "temperatureLow",
  #                         "Chance of Rain" = "precipProbability",
  #                         "Dew Point" = "dewPoint",
  #                         "Humidity" = "humidity",
  #                         "Pressure" = "pressure",
  #                         "Wind Speed" = "windSpeed",
  #                         "Cloud Cover" = "cloudCover"
  #   )
  #   
  #   output$daily_plot <- renderPlot(make_time_plot(day,plot_option,is_hour = F))
  # })
  # 
  # output$daily_icons <- renderUI({
  #   
  #   
  #   if (!exists("hour")){
  #     good <- weather_data()
  #     
  #     if (!good){
  #       output$error <- renderUI(h2("Error! Bad zip provided"))
  #       return(NULL)
  #     }else{
  #       output$error <- renderUI(h2(""))
  #     }
  #   } 
  #   
  #   day_icons <- extract_from_list(day,"icon")
  #   day_times <- extract_from_list(day,"time")
  #   day_times <- format(as.POSIXct(day_times,origin = "1970-01-01",tz = "America/Chicago"),format = "%a, %b %d")
  #   day_temp_high <- extract_from_list(day,"temperatureHigh")
  #   day_temp_low <- extract_from_list(day,"temperatureLow")
  #   day_precip <- paste0(extract_from_list(day,"precipProbability")*100,"%")
  #   day_summary<- extract_from_list(day,"summary")
  #   
  #   make_day_icons <- function(icon,time,high_temp,low_temp,precip,summary){
  #     tagList(
  #       fluidRow(
  #         column(width = 1,
  #                h5(time)
  #         ),
  #         column(width = 1,
  #                get_image(icon)
  #         ),
  #         column(width = 2,
  #                h6(paste0("High: ",high_temp," °F")),
  #                h6(paste0("Low: ",low_temp," °F")),
  #                h6(paste0("Chance of Rain: ",precip))
  #         ),
  #         column(width = 2,
  #                br(),
  #                h6(summary)
  #         )
  #       ),
  #       hr()
  #     )
  #   }
  #   
  #   
  #   tagList(mapply(FUN = make_day_icons,day_icons,day_times,day_temp_high,day_temp_low,day_precip,day_summary,SIMPLIFY = F))
  #   
  #   
  #   
  # })
  
}