
server <- function(input,output,session){
  
  
  output$mymap <- renderLeaflet(
    
    leaflet() %>% setView(lng = -89.7452665,lat=43.001617,zoom = 13) %>% 
      addTiles()
  )
  
  
  
  base_url <- "https://api.openrouteservice.org/"
  
  key <- "5b3ce3597851110001cf624898c9c20ea4aa4b609f651a41640132e4"
  
  format_search <- function(x){
    gsub(" ","%20",x,fixed = T)
  }
  
  get_geocode <- function(x){
    re <- try(GET(paste0(base_url,"geocode/search?api_key=",key,"&text=",format_search(x))))
    if (inherits(re,"try-error")){
      print("error")
      Sys.sleep(1)
      re <- try(GET(paste0(base_url,"geocode/search?api_key=",key,"&text=",format_search(x))))
    }
    re <- content(re)
    re  
    
  }
  
  get_avg_coord <- function(re,as_vec = F){
    bbox <- get_bbox(re)
    if (as_vec){
      return(c(mean(c(bbox[1],bbox[3])),mean(c(bbox[2],bbox[4]))))
    }else{
      return(paste0(mean(c(bbox[1],bbox[3])),",",mean(c(bbox[2],bbox[4]))))
    }
    
  }
  
  get_directions <- function(start,end,profile = c("driving","cycling","walking")){
    if (length(profile) > 1){
      prof <- "driving-car"
    }else{
      prof <- switch(profile,
                     'driving' = 'driving-car',
                     'cylcling' = 'cycling-road',
                     'walking' = 'foot-walking','driving-car')
    }
    s <- get_geocode(start)
    s <- get_avg_coord(s)
    e <- get_geocode(end)
    e <- get_avg_coord(e)
    
    re <- GET(paste0(base_url,"v2/directions/",prof,"?api_key=",key,"&start=",s,"&end=",e))
    #content(re)
  }
  
  
  
  get_bbox <- function(re){
    b <- re$bbox
    c(left = b[[1]], bottom  = b[[2]], right = b[[3]],top = b[[4]])
    
  }
  
  get_isochrones <- function(start,time = 300,profile = c("driving","cycling","walking")){
    if (length(profile) > 1){
      prof <- "driving-car"
    }else{
      prof <- switch(profile,
                     'driving' = 'driving-car',
                     'cycling' = 'cycling-road',
                     'walking' = 'foot-walking','driving-car')
    }
 
    s <- get_geocode(start)
    s <- get_avg_coord(s,as_vec = T)
    
    re <- try(POST(url = paste0(base_url,"v2/isochrones/",prof),
                   config = add_headers(Accept="application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8",
                                        Authorization= key),
                   body = toJSON(list(locations= list(s),range = time)),content_type_json()))
    #body = list(locations=paste0("[[",s,"]]"),range=paste0("[",time,"]")))
    
    if (inherits(re,'try-error')){
      print("error")
      Sys.sleep(1)
      POST(url = paste0(base_url,"v2/isochrones/",prof),
           config = add_headers(Accept="application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8",
                                Authorization= key),
           body = toJSON(list(locations= list(s),range = time)),content_type_json())
    }else if(re$status_code != 200){
      return(NULL)
    }
    
    
    re2 <- FROM_GeoJson(content(re,as = "text"))
    re2$features[[1]]$geometry$coordinates
    
  }
  
  observeEvent(input$enter,{
    

    vehicle <- isolate(input$vehicle)
    address <- isolate(input$address)
    
    
    
    
    coords1 <- get_isochrones(address,time = 300,profile = vehicle)
    if (is.null(coords1)){
      shinyWidgets::sendSweetAlert(session = session, title = NULL,text = tags$span(tags$h2("Bad Address!")), type = "error")
      return(NULL)
    }
    Sys.sleep(1)
    coords2 <- get_isochrones(address,time = 600,profile = vehicle)
    Sys.sleep(1)
    coords3 <- get_isochrones(address,time = 1200,profile = vehicle)
    
 
    
    output$mymap <- renderLeaflet(
    leaflet() %>% setView(lng = median(coords1[,1]),lat=median(coords1[,2]),zoom = 11) %>% 
      addTiles() %>%  
      
      addPolygons(lng = coords3[,1],lat = coords3[,2],weight = 2,fillColor = "red",color = "black") %>%  
      addPolygons(lng = coords2[,1],lat = coords2[,2],weight = 2,fillColor = "orange",color = "black",fillOpacity = 0.3) %>% 
      addPolygons(lng = coords1[,1],lat = coords1[,2],weight = 2,fillColor = "yellow",color = "black",fillOpacity = 0.4) %>% 
      addLegend(colors = c("red","orange","yellow"),labels = c("20 min.", "10 min.", "5 min."),title = "Traveling Time",position = "bottomleft")
    
    )
    
    
  })
  
  
}