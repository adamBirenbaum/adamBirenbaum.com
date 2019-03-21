library(ggplot2)


function(input,output){
  
  
  g <- function(){
    ggplot() + theme(line = element_blank(),rect = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),title = element_text(size = 20,face = "bold"),axis.text = element_blank())
  }
  
  make_note <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    df <- data.frame(x = xx, y = yy)
    geom_path(data = df,aes(x=x,y=y),size = 1)
  }
  
  
  make_segments <- function(nsegments = 3){
    n1 <- -1:(-1*nsegments)
    n2 <- 1:nsegments + 10
    yheights <- c(n1,5,n2)
    df <- data.frame(x = 0,xend = 1,y=yheights,yend = yheights)
    geom_segment(data = df, aes(x = x, xend = xend, y = y, yend = yend))
  }
  
  
  
  make_w <- function(key_num,pt = F){
    width = 1
    df <- data.frame(xmin = key_num*width,xmax = (key_num + 1)*width, ymin = 0,ymax = 6.857143)
    
    if (pt){
      list(geom_rect(data = df,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),fill = "white",color = "black"),
           geom_point(data = df,aes(x = xmin + width/2,y = ymax * .1),color = "red"))
    }else{
      geom_rect(data = df,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),fill = "white",color = "black") 
    }
    
    
  }
  
  make_b <- function(key_num,sharp = T,pt = F){
    bwidth = 0.5357
    width = 1
    if (sharp){
      df <- data.frame(xmin = (key_num + 1)*width - bwidth/2,xmax = (key_num + 1)*width + bwidth/2, ymin = 2.357,ymax = 6.857143)
      
    }else{
      df <- data.frame(xmin = key_num *width - bwidth/2,xmax = key_num*width + bwidth/2, ymin = 2.357,ymax = 6.857143)
    }
    
    if (pt){
      list(geom_rect(data = df,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),fill = "black"), 
           geom_point(data = df,aes(x = xmin + bwidth/2,y = (ymax - ymin) * .1 + ymin),color = "red"))
    }else{
      geom_rect(data = df,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),fill = "black") 
    }
    
    
  }
  
  make_octave <- function(shift = 0){
    a <- 7*shift
    list(make_w(1 + a),make_w(2 + a),make_w(3 + a),make_w(4 + a),make_w(5 + a),make_w(6 + a),make_w(7 + a))
    
  }
  
  keys = c("C_M","G_M","D_M","A_M","E_M","B_M","Gb_M","Fs_M","Db_M","Cs_M","Ab_M","Eb_M","Bb_M","F_M")
  make_b_octave <- function(key = "C_M",shift = 0){
    b_keys <- switch(key,
                     C_M = c(1,2,4,5,6),
                     G_M = c(1,2,4,5,7),
                     A_M = c(1,3,4,6,7)
    )
    a <- 7*shift
    b_keys <- b_keys + a
    lapply(b_keys, make_b)
    
  }
  
  make_full <- function(){
    list(make_octave(),make_octave(1),make_octave(2),make_octave(3),make_octave(4),make_octave(5),make_octave(6),make_w(50),make_w(51),make_w(52),
         make_b_octave("A_M"),make_b_octave("A_M",shift = 1),make_b_octave("A_M",shift = 2),make_b_octave("A_M",shift = 3),make_b_octave("A_M",shift = 4),
         make_b_octave("A_M",shift = 5),make_b_octave("A_M",shift = 6),make_b(50))
  }
  
  
  
  
  
  
  output$plot <- renderPlot(
    g() + geom_hline(yintercept = c(0,1,2,3,4,6,7,8,9,10)) + make_segments(input$nseg) + xlim(0,10) + coord_fixed()
  )
 
  output$plot2 <- renderPlot(
    g() + make_full()
  )
  
  observeEvent(input$click,{
    
    y <- input$click$y

    decimal <- y - trunc(y)
    
    if (decimal < .25){
      y <- trunc(y)
    }else if (decimal >= .25 && decimal <= .75){
      y <- trunc(y) + 0.5
    }else{
      y <- trunc(y) + 1
    }
    
    all_key_num <- -23:28
    key_let <- c(rep(c("A","B","C","D","E","F","G"),times = 7),"A","B","C")
    selected_key <- key_let[y*2]

    output$plot <- renderPlot(
    g() + geom_hline(yintercept = c(0,1,2,3,4,6,7,8,9,10)) + make_segments(input$nseg) + xlim(0,10) + make_note(c(0.5,y),diameter = 1) +
      ggtitle(selected_key) + coord_fixed()
    )
    
    key_num <- y * 2 + 14
    pt_df <- data.frame(x = key_num + 0.5,y = 6.857143 * .1)
    
    output$plot2 <- renderPlot(
      g() + make_full() + geom_point(data = pt_df,aes(x =x,y =y ),color = "red") +  ggtitle(selected_key)
    )
    
    
    
  })
  
  observeEvent(input$click2,{

    x <- trunc(input$click2$x)
    y <- (-23:28)[x] / 2 + 5
    key_let <- c(rep(c("A","B","C","D","E","F","G"),times = 7),"A","B","C")
    selected_key <- key_let[x]
    
    output$plot <- renderPlot(
      g() + geom_hline(yintercept = c(0,1,2,3,4,6,7,8,9,10)) + make_segments(input$nseg) + xlim(0,10) + make_note(c(0.5,y),diameter = 1) +
        ggtitle(selected_key) + coord_fixed()
    )
    
    pt_df <- data.frame(x = x + 0.5,y = 6.857143 * .1)
    output$plot2 <- renderPlot(
      g() + make_full() + geom_point(data = pt_df,aes(x =x,y =y ),color = "red") +  ggtitle(selected_key)
    )
    
  

    
  })
  
}

