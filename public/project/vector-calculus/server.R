
server <- function(input, output){
  is_error <- function(x) inherits(x,"try-error")
  
  div_i <- reactive(parse(text = input$div_i))
  div_j <- reactive(parse(text = input$div_j))
  
  output$ui_div_plot <- renderUI({
    if (input$div_plot_type == "Vector Field"){
      plotOutput("div_plot",height = "600px")
    }else{
      plotlyOutput("div_plotly",height = "600px")
    }
    
  })
  
  output$div_plot <- renderPlot({
  
    
    
   
    
    div_fi <- function(x,y){
      eval(parse(text = div_i()))
      
    } 
    
   
    
    div_fj <- function(x,y){
      eval(parse(text = div_j()))
      
    } 
    
    div <- function(x,y){
      eval(D(expr = div_i(), name = "x")) + eval(D(expr = div_j(), name = "y"))  
    }
    
    
  
      div_df <- expand.grid(-10:10,-10:10)
      names(div_df) <- c("x","y")
      error_check <- try(sum(div_fi(div_df$x,div_df$y),div_fj(div_df$x,div_df$y)),silent = T)
      
      
      if (is_error(error_check) | input$div_i == "" | input$div_j == "") return(NULL)
      
      
      
      fixed_amount <- 1
      div_df  <- div_df %>% mutate(xend = x + div_fi(x,y), yend = y + div_fj(x,y),
                                   len = sqrt((xend - x)^2 + (yend - y)^2),
                                   max_len = max(len),
                                   xend = (xend - x)/(len/fixed_amount) + x,
                                   yend = (yend - y)/(len/fixed_amount) + y,
                                   color = log(len)) %>% 
        filter(len > 0) 

   
    if (input$div_gradient){
      a <- seq(from = -12, to = 12, length.out = 100)
      tile_df <- expand.grid(a,a)
      names(tile_df) <- c("x","y")
      
      tile_df <- tile_df %>% mutate(diverg = div(x,y))
      
      #browser()
      
      custom_label <- function(b){
        n <- length(b)
        a <- replicate(n,"")
        if (n == 6){
          a[1] <- "Low"
          a[ceiling(n/2)] <- "0"
          a[5] <- "High"
        }else if (n == 5){
          a[2] <- "Low"
          a[3] <- "0"
          a[4] <- "High"
        }else if (n == 1){
          return(as.character(b))
        }else if (n == 4){
          a[2] <- "Low"
          a[3] <- "High"
        }
        
        a
      }
      
    }
  
    
    
      if (input$div_gradient){
        g <- ggplot(tile_df) + geom_raster(aes(x = x,y = y, fill = diverg),alpha = .9) + 
          geom_segment(data = div_df,aes (x = x, y = y, xend = xend, yend = yend,color = color), arrow = arrow(type = "closed",angle =25,length = unit(.1,"in"))) +
          scale_color_gradientn(colours = rainbow(10)) + 
          theme_minimal()+ scale_fill_gradient2(low = "blue", mid = "white", high = "red",name = "Divergence",labels = custom_label)
      }else{
        g <- ggplot(div_df,aes (x = x, y = y, xend = xend, yend = yend)) + 
          geom_segment(arrow = arrow(type = "closed",angle =25,length = unit(.1,"in"))) +
          theme_minimal()
      }
      
   
    
    # ggplotly(g)
    # 
    # 
    # 
    # browser()
    # ggplot(tile_df) + geom_raster(aes(x = x,y = y, fill = diverg),alpha = .9) + 
    #   geom_segment(data = div_df,aes (x = x, y = y, xend = xend, yend = yend), arrow = arrow(type = "closed",angle =25,length = unit(.1,"in"))) +
    #   theme_minimal()+ scale_fill_gradient2(low = "blue", mid = "white", high = "red",name = "Divergence",labels = custom_label) + 
    #   #geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
    #   NULL
    # 
    # #ggplot(div_df,aes (x = x, y = y, xend = xend, yend = yend)) + geom_segment(arrow = arrow(type = "closed",length = unit(.1,"in")),aes(color = div)) + theme_minimal()+
    #  # xlim(-10,10) + ylim(-10,10) + geom_raster(data = tile_df, aes(x = x,y = y, fill = diverg))
    # 
    
   g
    
  })
    

  output$div_plotly <- renderPlotly({
 
    div_fi <- function(x,y){
      eval(parse(text = div_i()))
      
    } 
    
    
    
    div_fj <- function(x,y){
      eval(parse(text = div_j()))
      
    } 
    
    div <- function(x,y){
      eval(D(expr = div_i(), name = "x")) + eval(D(expr = div_j(), name = "y"))  
    }
    
    
  
    
      grid_vec <- seq(from = -10, to = 10, length.out = 21)
      grid_df <- expand.grid(grid_vec,grid_vec)
      names(grid_df) <- c("x","y")
      
      error_check <- try(sum(div_fi(grid_df$x,grid_df$y),div_fj(grid_df$x,grid_df$y)),silent = T)
      
      
      if (is_error(error_check) | input$div_i == "" | input$div_j == "") return(NULL)
      
      
      
      n_frame <- 30
      n <- nrow(grid_df)
      
      grid_df2 <- data.frame(x = replicate(n_frame*n, 0), y =replicate(n_frame*n, 0))
      grid_df2[1:n,] <- grid_df
      fixed_amount <- .5
   
      for (i in 2:n_frame){
        beg <- (i-1) * n + 1
        end <- beg + n - 1
        grid_df2[beg:end,] <- grid_df2[(beg-n):(end-n),] %>% 
          mutate(x2 = x + div_fi(x,y), y2 = y + div_fj(x,y),frame = i,
                 len = sqrt((x2-x)^2 + (y2 - y)^2), max_len = max(len),
                 x = x + div_fi(x,y)/(len/fixed_amount), y = y + div_fj(x,y)/(len/fixed_amount)) %>%
          select(x,y)
        
      }
      
      grid_df2$frame <- rep(1:n_frame,each = n)
      
      #g <- ggplot(grid_df2,aes(x = x, y = y, frame = frame)) + geom_point() 
    
    
    
    if (input$div_gradient){
   
      animation_max <- max(c(max(abs(grid_df2$x),na.rm = T),max(abs(grid_df2$y),na.rm = T)))
      a <- seq(from = -animation_max, to = animation_max, length.out = 100)
      tile_df <- expand.grid(a,a)
      names(tile_df) <- c("x","y")
      
      tile_df <- tile_df %>% mutate(diverg = div(x,y))
      
      #browser()
      
      custom_label <- function(b){
        n <- length(b)
        a <- replicate(n,"")
        if (n == 6){
          a[1] <- "Low"
          a[ceiling(n/2)] <- "0"
          a[5] <- "High"
        }else if (n == 5){
          a[2] <- "Low"
          a[3] <- "0"
          a[4] <- "High"
        }else if (n == 1){
          return(as.character(b))
        }else if (n == 4){
          a[2] <- "Low"
          a[3] <- "High"
        }
        
        a
      }
      
    }
    
    
  
      if (input$div_gradient){
   
        g <- ggplot(tile_df) + geom_raster(aes(x = x,y = y, fill = diverg),alpha = .9) + 
          geom_point(data = grid_df2,aes(x = x, y = y, frame = frame),size = .5) +
          theme_minimal()+ scale_fill_gradient2(low = "blue", mid = "white", high = "red",name = "Divergence",labels = custom_label)
        g <- ggplotly(g)
      }else{
        g <- ggplot(grid_df2,aes(x = x, y = y, frame = frame)) + geom_point() 
        g <- ggplotly(g)
      }
      
    
    
    # ggplotly(g)
    # 
    # 
    # 
    # browser()
    # ggplot(tile_df) + geom_raster(aes(x = x,y = y, fill = diverg),alpha = .9) + 
    #   geom_segment(data = div_df,aes (x = x, y = y, xend = xend, yend = yend), arrow = arrow(type = "closed",angle =25,length = unit(.1,"in"))) +
    #   theme_minimal()+ scale_fill_gradient2(low = "blue", mid = "white", high = "red",name = "Divergence",labels = custom_label) + 
    #   #geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
    #   NULL
    # 
    # #ggplot(div_df,aes (x = x, y = y, xend = xend, yend = yend)) + geom_segment(arrow = arrow(type = "closed",length = unit(.1,"in")),aes(color = div)) + theme_minimal()+
    #  # xlim(-10,10) + ylim(-10,10) + geom_raster(data = tile_df, aes(x = x,y = y, fill = diverg))
    # 
    
    g
    
  })
}