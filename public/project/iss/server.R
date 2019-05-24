
df <- data.frame(Latitude = numeric(0),Longitude = numeric(0))
server <- function(input,output,session){
  

  get_lat_and_lon <- function(){

    url <- "http://api.open-notify.org/iss-now.json"

    raw <- readLines(url)
    x <- strsplit(raw,split = '"')[[1]]
    x <- x[!grepl("\\{|:",x)]
    lat_ind <- grep("latitude",x)
    long_ind <- grep("longitude",x)

    as.numeric(c(x[lat_ind+1],x[long_ind + 1]))

  }

 
  #autoInvalidate <- reactiveTimer(2000)
  
  # observe(
  #   autoInvalidate()
  # )
  ISSIcon <- makeIcon(
    iconUrl = "https://i2.wp.com/freepngimages.com/wp-content/uploads/2015/12/international-space-station-transparent-background.png?fit=817%2C325",
    iconWidth = 150, iconHeight = 95,
    iconAnchorX = 94, iconAnchorY = 22

  )
  
  output$map <- renderLeaflet({
    #autoInvalidate()
    invalidateLater(5000,session)

    loc <- get_lat_and_lon()
    df <- rbind(df,data.frame(Latitude = loc[1],Longitude = loc[2]))
    print(nrow(df))
    leaflet(data = df) %>% setView(lng = loc[2],lat = loc[1],zoom = 6) %>%
      addProviderTiles(providers$Wikimedia) %>% 
      addMarkers(~Longitude,~Latitude,icon = ISSIcon)

  })

  
}


  