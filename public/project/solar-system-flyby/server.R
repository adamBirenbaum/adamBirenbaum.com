
server <- function(input, output) {
   
  
  M_sun <- 2E30
  M_mars <- 6.39E23
  M_jupiter <- 1.898E27
  M_saturn <- 5.683E26
  
  
  m <- 1000
  r <- matrix(c(0,1E11),2)
  r_sun <- matrix(c(0,0),2)
  r_mars <- matrix(c(2.228E11,0),2)
  r_jupiter <- matrix(c(7.7785E11,0),2)
  r_saturn <- matrix(c(1.434E12,0),2)

  G = 6.67E-11
  
  mag <- function(x) sqrt(x[1,1]^2 + x[2,1]^2)
  set_lim <- function(x) list(xlim(-x,x),ylim(-x,x))
  planet_geom <- function(r,co,siz,lab){
    list(geom_point(data = data.frame(x = r[1,1],y = 0),aes(x = x, y=y),color = "black",fill = co,pch =21, size = siz),
         annotate("text",x = r[1,1],y = -5E10,label = lab)
         )
    
  } 
  
observeEvent(c(input$velx,input$vely,input$nframes),{

  output$static_trajectory <- renderPlot({
    
    N = isolate(input$nframes)
    dt = 50000
    
  
    
    v <- matrix(c(isolate(input$velx),isolate(input$vely)),2)
    
    df <- data.frame(x = 1:N, y = 0)
    df$x[1] <- r[1,1]
    df$y[1] <- r[2,1]
    
    for (i in 2:N){
      a <- G*M_sun/mag(r)^2
      avec <- -r
      avec <- avec / mag(avec) * a
      
      r_m <- r_mars - r
      a_mars <- G*M_mars/mag(r_m)^2
      a_vec_mars <- r_m/mag(r_m) * a_mars
      
      
      r_jup <- r_jupiter - r
      a_jupiter <- G*M_jupiter/mag(r_jup)^2
      a_vec_jupiter <- r_jup/mag(r_jup) * a_jupiter
      
      r_sat <- r_saturn - r
      a_sat <- G*M_saturn/mag(r_sat)^2
      a_vec_sat <- r_sat/mag(r_sat) * a_sat
      
      avec <- avec + a_vec_mars + a_vec_jupiter + a_vec_sat
      
      v[1,1] <- v[1,1] + avec[1,1]*dt
      v[2,1] <- v[2,1] + avec[2,1]*dt
      
      r[1,1] <- r[1,1] + v[1,1]*dt
      r[2,1] <- r[2,1] + v[2,1]*dt
      
      df$x[i] <- r[1,1]
      df$y[i] <- r[2,1]
      
    }
    
    max_val <- max(df$x,df$y,mag(r_saturn))
    max_y <- max(abs(df$y))
    
    planet_df <- data.frame(x = 2E11,y =0)
    df$t <- 1:nrow(df)
    g <- ggplot(df, aes(x = x, y =y )) + geom_point(size = .5) + planet_geom(r_sun,"yellow",10,"Sun") + planet_geom(r_mars,"red",2,"Mars") + planet_geom(r_jupiter,"orange",5,"Jupiter") + 
      planet_geom(r_saturn,"yellow",4,"Saturn")+ xlab("") + ylab("")+
      theme_bw() + coord_fixed() + ylim(-max_y,max_y) + theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) #+ set_lim(max_val)  
    
    g
  })
  
})


  observeEvent(input$animate,{

    output$animation_gif <- renderImage({
      
      
    
      N = isolate(input$nframes)
      dt = 50000
      
      
      
      v <- matrix(c(isolate(input$velx),isolate(input$vely)),2)
    
    df <- data.frame(x = 1:N, y = 0)
    df$x[1] <- r[1,1]
    df$y[1] <- r[2,1]
    
    for (i in 2:N){
      a <- G*M_sun/mag(r)^2
      avec <- -r
      avec <- avec / mag(avec) * a
      
      r_m <- r_mars - r
      a_mars <- G*M_mars/mag(r_m)^2
      a_vec_mars <- r_m/mag(r_m) * a_mars
      
      
      r_jup <- r_jupiter - r
      a_jupiter <- G*M_jupiter/mag(r_jup)^2
      a_vec_jupiter <- r_jup/mag(r_jup) * a_jupiter
      
      r_sat <- r_saturn - r
      a_sat <- G*M_saturn/mag(r_sat)^2
      a_vec_sat <- r_sat/mag(r_sat) * a_sat
      
      avec <- avec + a_vec_mars + a_vec_jupiter + a_vec_sat
      
      v[1,1] <- v[1,1] + avec[1,1]*dt
      v[2,1] <- v[2,1] + avec[2,1]*dt
      
      r[1,1] <- r[1,1] + v[1,1]*dt
      r[2,1] <- r[2,1] + v[2,1]*dt
      
      df$x[i] <- r[1,1]
      df$y[i] <- r[2,1]
      
    }
    
    max_val <- max(df$x,df$y,mag(r_saturn))
    max_y <- max(abs(df$y))
    
    planet_df <- data.frame(x = 2E11,y =0)
    
   
    df <- df[unique(round(seq(from = 1, to = nrow(df),length.out = 150))),]
    df$t <- 1:nrow(df)
    
    g <- ggplot(df, aes(x = x, y =y )) + geom_point(size = .5) + planet_geom(r_sun,"yellow",10,"Sun") + planet_geom(r_mars,"red",2,"Mars") + planet_geom(r_jupiter,"orange",5,"Jupiter") + 
      planet_geom(r_saturn,"yellow",4,"Saturn")+ xlab("") + ylab("")+
      theme_bw() + coord_fixed() + ylim(-max_y,max_y) + theme(panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank()) +#+ set_lim(max_val)  
      transition_time(t)
    
    anim_gif <- animate(g,nframes = 150,fps = 15)
    temp_file_name <- paste0(tempfile(),".gif")
    anim_save(temp_file_name,anim_gif)
    
    list(src = temp_file_name,width = "100%")
  })
  
  
})
  
}
