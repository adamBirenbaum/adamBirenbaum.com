zip_coords <- read.csv(paste0(path_to_weather,"zip_coords.csv"),stringsAsFactors = F)

function(input,output){
  
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
  
  make_time_plot <- function(hour,col_name,is_hour = T){
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

    hour_times[!(1:nn) %in% blank_seq] <- ""
    df_hour <- data.frame(Time = 1:length(hour_times),y = hour_y)
    names(hour_times) <- 1:length(hour_times)
    ggplot(df_hour,aes(x = Time, y = y)) + geom_line() + scale_x_continuous(breaks = 1:length(hour_times),label = hour_times) + 
      theme_bw() + ggtitle(paste0(title,col_name)) + ylab(col_name)
    
  }
  
 
  get_weather_data <- function(){
    zip <- input$zip
    
    zip_id <- grep(zip,zip_coords$Zip)
    if (length(zip_id) == 0) return(F)
    lat <- zip_coords$Latitude[zip_id]
    long <- zip_coords$Longitude[zip_id]
    
    key <- "5278dff938af0d98c6290804dacda8d4"
  
    xx <- GET(url = paste0("https://api.darksky.net/forecast/",key,"/",lat,",",long))
    xx <- content(xx,"parsed")
    
    
    current <<- xx$currently
    minute <<- xx$minutely
    hour <<- xx$hourly
    day <<- xx$daily
    TRUE
  }
  
  observeEvent(input$enter,{

     good <- get_weather_data()

     if (!good){
       output$error <- renderUI(h2("Error! Bad zip provided"))
       return(NULL)
     }else{
       output$error <- renderUI(h2(""))
     }
    output$current_summary <- renderUI(
      tagList(
        h2(current$summary),
        h2(paste0(current$temperature," °F")),
        h3(paste0("Chance of rain: ",current$precipProbability*100,"%")),
        h3(paste0("Humidity: ",current$humidity*100,"%")),
        h3(minute$summary)
        )
    )
    
    output$current_icon <- make_icon(current$icon,300)
    

    
  })
  
  observeEvent(input$hourly_plots,{

    if (!exists("hour")){
      good <- get_weather_data()
      
      if (!good){
        output$error <- renderUI(h2("Error! Bad zip provided"))
        return(NULL)
      }else{
        output$error <- renderUI(h2(""))
      }
    } 
    
    
    
    plot_option <- switch(input$hourly_plots,
      "Temperature" = "temperature",
      "Chance of Rain" = "precipProbability",
      "Dew Point" = "dewPoint",
      "Humidity" = "humidity",
      "Pressure" = "pressure",
      "Wind Speed" = "windSpeed",
      "Cloud Cover" = "cloudCover"
    )
    
    output$hour_plot <- renderPlot(make_time_plot(hour,plot_option))
  })
    output$hourly_icons <- renderUI({
    
   
      if (!exists("hour")){
        good <- get_weather_data()
        
        if (!good){
          output$error <- renderUI(h2("Error! Bad zip provided"))
          return(NULL)
        }else{
          output$error <- renderUI(h2(""))
        }
      } 
      
      hour_icons <- extract_from_list(hour,"icon")
      hour_times <- extract_from_list(hour,"time")
      hour_times <- format(as.POSIXct(hour_times,origin = "1970-01-01",tz = "America/Chicago"),format = "%a %I:%M %p")
      hour_temp <- extract_from_list(hour,"temperature")
      hour_precip <- paste0(extract_from_list(hour,"precipProbability")*100,"%")

      make_hourly_icons <- function(icon,time,temp,precip){
       tagList(
         
       
         fluidRow(
          column(width = 1,
                 h6(time)
                 ),
          column(width = 1,
                 get_image(icon)
                 ),
          column(width = 2,
                 h6(paste0(temp," °F")),
                 h6(paste0("Chance of Rain: ",precip))
                 )
        ),
        hr()
       
        )
      }
      
   
      tagList(mapply(FUN = make_hourly_icons,hour_icons,hour_times,hour_temp,hour_precip,SIMPLIFY = F))

      
 
  })
    
    observeEvent(input$daily_plots,{
      
      if (!exists("hour")){
        good <- get_weather_data()
        
        if (!good){
          output$error <- renderUI(h2("Error! Bad zip provided"))
          return(NULL)
        }else{
          output$error <- renderUI(h2(""))
        }
      } 
      
      
      
      plot_option <- switch(input$daily_plots,
                            "High Temperature" = "temperatureHigh",
                            "Low Temperature" = "temperatureLow",
                            "Chance of Rain" = "precipProbability",
                            "Dew Point" = "dewPoint",
                            "Humidity" = "humidity",
                            "Pressure" = "pressure",
                            "Wind Speed" = "windSpeed",
                            "Cloud Cover" = "cloudCover"
      )
      
      output$daily_plot <- renderPlot(make_time_plot(day,plot_option,is_hour = F))
    })
    
    output$daily_icons <- renderUI({
      
      
      if (!exists("hour")){
        good <- get_weather_data()
        
        if (!good){
          output$error <- renderUI(h2("Error! Bad zip provided"))
          return(NULL)
        }else{
          output$error <- renderUI(h2(""))
        }
      } 
      
      day_icons <- extract_from_list(day,"icon")
      day_times <- extract_from_list(day,"time")
      day_times <- format(as.POSIXct(day_times,origin = "1970-01-01",tz = "America/Chicago"),format = "%a, %b %d")
      day_temp_high <- extract_from_list(day,"temperatureHigh")
      day_temp_low <- extract_from_list(day,"temperatureLow")
      day_precip <- paste0(extract_from_list(day,"precipProbability")*100,"%")
      day_summary<- extract_from_list(day,"summary")
      
      make_day_icons <- function(icon,time,high_temp,low_temp,precip,summary){
        tagList(
        fluidRow(
          column(width = 1,
                 h5(time)
          ),
          column(width = 1,
                 get_image(icon)
          ),
          column(width = 2,
                 h6(paste0("High: ",high_temp," °F")),
                 h6(paste0("Low: ",low_temp," °F")),
                 h6(paste0("Chance of Rain: ",precip))
          ),
          column(width = 2,
                 br(),
                 h6(summary)
                 )
        ),
        hr()
        )
      }
      
      
      tagList(mapply(FUN = make_day_icons,day_icons,day_times,day_temp_high,day_temp_low,day_precip,day_summary,SIMPLIFY = F))
      
      
      
    })
  
}