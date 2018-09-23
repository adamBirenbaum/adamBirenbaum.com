
server <- function(input, output,session = session) {
  
  is_error <- function(x) inherits(x,"try-error")
  
  # renderPlotly() also understands ggplot2 objects!
  
  output$ui_guess <- renderUI({
    sliderInput("guess","Guess", min = input$xrange[1],max = input$xrange[2],step = 0.25, value = mean(input$xrange))
    
  })
  
  output$ui_guess2 <- renderUI({
    sliderInput("guess2","Guess", min = input$xrange[1],max = input$xrange[2],step = 0.25, value = mean(input$xrange))
    
  })


  math_fun <- c("sin","cos","tan","sqrt","exp","log")
  math_op <- c("+","-","*","/","^")
  math_nums <- seq(from = -10, 10, length.out = 41)[-21]
  possible_nums <- function() sample(c(replicate(60,"x"),math_nums),1)
  
  random_fun <- function(fun_string,total_terms,op = F){
    
    if (!op){
      if (total_terms > 10) return(fun_string)
      
    }
    
    
    
    num_terms <- sample(2:8,size = 1)
    
    if (total_terms < 15){
      for (i in 1:num_terms){
        if (!op){
          if (total_terms > 10) return(fun_string)
          
        }
        
        math_selection <- sample(1:2,1)
        
        if (math_selection == 1){
          fun_string <- c(fun_string, paste0(sample(math_fun,1),"("))
          total_terms <- total_terms + 1
          op = F
          random_fun(fun_string,total_terms,op)
          
        }else{
          math_op <- sample(math_op,1)
          if (math_op == "^"){
            math_op <- paste0(math_op,possible_nums())
            op = F
          } else op = T
          fun_string <- c(fun_string, paste0(possible_nums(),math_op,"("))
          total_terms <- total_terms + 1
          
          random_fun(fun_string,total_terms,op)
        }
        
        
      }
    }
    
    if (op) fun_string <- c(fun_string, possible_nums())
    
    fun_string <- c(fun_string,replicate(num_terms,")"))
    
    fun_string <- paste0(fun_string,collapse = "")
    while(T){
      
      fun_string <- gsub("()","",fun_string,fixed = T)
      fun_string <- gsub("sin)|cos)|tan)|sqrt)|exp)|log)",")",fun_string)
      if (!grepl("()",fun_string,fixed = T) && !grepl("sin)|cos)|tan)|sqrt)|exp)|log)",fun_string)) break
    }
    
    
    fun_string
    
    
    
  }

  observeEvent(input$random_function,{
    updateTextInput(session = session,"roots_function","f(x)",value = random_fun("",0))



    
  })
  
  
  
  
  roots_function <- reactive(input$roots_function)
  integration_function <- reactive(input$integration_function)
  xrange <- reactive(input$xrange)
  integration_xrange <- reactive(input$integration_xrange)
  n <- reactive(input$niter)
  guess <- reactive(input$guess)
  
  
  safely_get_fx_df <- function(f_string,xrange){
    x <- seq(from = xrange[1], to = xrange[2], length.out = 3000)
    error_check <- try(eval(parse(text = f_string)),silent = T)
    error_check2 <- try(as.numeric(error_check),silent = T)
    if (is_error(error_check) | is_error(error_check2)){
      return(NULL)
    }else{
      x <- seq(from = xrange[1], to = xrange[2], length.out = 3000)
      
      
      y <- eval(parse(text = f_string))
      data.frame(x = x, y = y)
    }
    
    
  }
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observeEvent(input$integration_plot_dblclick, {
    brush <- input$integration_plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  


  convert_string_to_latex <- function(x,f){
  #x <- "exp(-x)*cos(x)*sin*(3*x)*sqrt(2)"  
  x <- gsub("exp\\(([^)]+)\\)","e^{\\1}",x) 
  x <- gsub("*","",x,fixed = T)
  x <- paste0(f,"(x) = ",x)
  x
  }
  
  get_y_value_from_x <- function(x,f_string){
    sapply(x,function(x) eval(parse(text = f_string)),USE.NAMES = F )
    
  }
  
  get_newton_output <- function(){
    
    d_function <- safely_get_fx_df(roots_function(),xrange())
    if (!is.null(d_function)){
      
      guess <- guess()
      x <- guess 
      newton_function_string <- roots_function()
      
      x_root_vec <- x
      for (i in 1:n()){

        if(is.null(x)) x <- 0
        y_at_root <- eval(parse(text = newton_function_string))
        deriv_at_root <- eval(D(parse(text = newton_function_string),"x"))
        x_root_vec <- c(x_root_vec,x_root_vec[i] - y_at_root / deriv_at_root)
        x <- x_root_vec[i+1]
      }
      roots_label_df <- data.frame(x = x_root_vec,y = 0,text = 0:(length(x_root_vec)-1))
      
      #browser()
      
      newton_segments_df <- roots_label_df[,c(1,2)]
      newton_segments_df$y <- get_y_value_from_x(newton_segments_df$x,newton_function_string)
      newton_segments_df$xend = newton_segments_df$x
      newton_segments_df$yend = 0
      
      xend_index <- 1:(nrow(newton_segments_df) - 1) + 1
      newton_slope_df <- newton_segments_df
      newton_slope_df$xend <- newton_segments_df$xend[c(xend_index,1)]
      newton_slope_df<- newton_slope_df[-nrow(newton_segments_df),]
      
      newton_segments_df <- rbind(newton_segments_df, newton_slope_df)
      
     
      g <-  ggplot(data = d_function, aes(x = x, y=y)) + geom_line(size = 2) + 
        geom_label(data = roots_label_df, aes(x =x, y =y, label = text,fill = factor(text)),  fontface = "bold",size = 8) + 
        geom_segment(data = newton_segments_df,aes(x = x, y = y, xend = xend, yend = yend),linetype = 2) + 
        scale_fill_brewer(palette ="Blues") + guides(fill=FALSE) + geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+ ylim(input$yrange[1],input$yrange[2]) + 
        coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
        ylab("f(x)") + ggtitle(TeX(convert_string_to_latex(newton_function_string,"f"))) + theme(axis.text = element_text(size = rel(1.5)),
                                                                                             title = element_text(size = rel(3))) 
      
      
      roots_table_df <- roots_label_df[,c(1,2)]
      names(roots_table_df) <- c("x","y")
      roots_table_df$y <- get_y_value_from_x(roots_table_df$x,newton_function_string)
      

      

    }else{
      g <- NULL
      roots_table_df <- NULL
    }
    return(list(plot = g, table = roots_table_df))
    
    
  }
  
  
  
  get_bisection_output <- function(){
    d_function <- safely_get_fx_df(roots_function(),xrange())
    if (!is.null(d_function)){
      
      start <- as.numeric(input$bisect_start)
      end <- as.numeric(input$bisect_end)
      niter <- input$bisect_niter
      #bisection_function_string
      start_vec <- start
      end_vec <- end
      x_root_vec <- c()
      for (i in 1:niter){
        midpoint <- (start + end) / 2
        x_root_vec <- c(x_root_vec, midpoint)
        y_at_midpoint <- get_y_value_from_x(midpoint,roots_function())
        y_at_start <- get_y_value_from_x(start,roots_function())
        if (sign(y_at_start )*sign(y_at_midpoint) == 1){
          start <- midpoint
          start_vec <- c(start_vec,start)
          end_vec <- c(end_vec, end)
        }else if (sign(y_at_start )*sign(y_at_midpoint) == -1){
          end <- midpoint
          start_vec <- c(start_vec,start)
          end_vec <- c(end_vec, end)
        }else if (midpoint == 0){
          break
        }
      }
    
      bisect_arrows_df <- data.frame(x = numeric(0),xend = numeric(0), y = numeric(0), yend = numeric(0))
      for (i in 1:(length(start_vec) - 1)){
        if (start_vec[i + 1] == start_vec[i]){
          yval <- get_y_value_from_x(end_vec[i],roots_function())
          bisect_arrows_df <- rbind(bisect_arrows_df, data.frame(x = end_vec[i], xend = end_vec[i + 1], y = yval, yend = yval))
        }else{
          yval <- get_y_value_from_x(start_vec[i],roots_function())
          bisect_arrows_df <- rbind(bisect_arrows_df, data.frame(x = start_vec[i], xend = start_vec[i + 1], y = yval, yend = yval))
          
        }
        
      }
      
    
      bisect_arrows_df$y2 <- 0
      #bisect_vert_line_df <- bisect_arrows_df
      # for (i in 1:(nrow(bisect_vert_line_df) - 1)){
      #  bisect_vert_line_df$y[i] <-  bisect_vert_line_df$y[i + 1] 
      # }
      # 
      bisect_arrows_df <- bisect_arrows_df[-nrow(bisect_arrows_df),]
      bisect_arrows_df$text <- 1:nrow(bisect_arrows_df)
      bisect_arrows_df$textx <- apply(bisect_arrows_df[,c(1,2)],MARGIN = 1, mean)
      bisect_arrows_df$texty <- bisect_arrows_df$y+sign(bisect_arrows_df$y)*.7
      #bisect_vert_line_df <- bisect_vert_line_df[-nrow(bisect_vert_line_df),]
      
      roots_label_df <- data.frame(x = x_root_vec,y = 0,text = 1:length(x_root_vec))
      start_end_df <- data.frame(x = c(as.numeric(input$bisect_start),as.numeric(input$bisect_end)), y = 0)
      
  
      g <-  ggplot(data = d_function, aes(x = x, y=y)) + geom_line(size = 2) + 
        geom_label(data = roots_label_df, aes(x =x, y =y, label = text,fill = factor(text)),  fontface = "bold",size = 8) +
        geom_segment(data = bisect_arrows_df, aes(x = x, y = y, xend = xend, yend = yend),arrow = arrow(angle = 15),size = 2)+
        geom_text(data = bisect_arrows_df,aes(x = textx,y = texty, label = text),size = 6)+
        geom_segment(data = bisect_arrows_df, aes(x = x, y = y, xend = x, yend = y2), linetype = 2) + 
        geom_point(data = start_end_df, aes(x = x, y = y),size = 4, color = "red" ) + 
        scale_fill_brewer(palette ="Blues",direction = -1) + guides(fill=FALSE) + geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+ ylim(input$yrange[1],input$yrange[2]) + 
        coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
        ylab("f(x)") + ggtitle(TeX(convert_string_to_latex(roots_function(),"f"))) + theme(axis.text = element_text(size = rel(1.5)),
                                                                                             title = element_text(size = rel(3))) 
      
      
      
      roots_table_df <- roots_label_df[,c(1,2)]
      names(roots_table_df) <- c("x","y")
      roots_table_df$y <- get_y_value_from_x(roots_table_df$x,roots_function())
    }else{
      g <- NULL
      roots_table_df <- NULL
    }
    
    return(list(plot = g, table = roots_table_df))
  }
  

  get_fixed_point_output <- function(){
    d_function <- safely_get_fx_df(roots_function(),xrange())
    if (!is.null(d_function)){
      
      gx_string <- paste0(roots_function()," + x")
      gx_df <- safely_get_fx_df(gx_string,xrange())
      x_string <- "x"
      x_df <- safely_get_fx_df(x_string,xrange())
      
  
      initial_guess <- input$guess2
      if (is.null(initial_guess)){
        initial_guess <- 0
      }
      niter <- input$fixed_niter
      
      x_root_vec <- initial_guess
      y_point_vec <- c(get_y_value_from_x(initial_guess,gx_string))
      for (i in 1:niter){
        if (i == 1) next()
        x_root_vec <- c(x_root_vec, get_y_value_from_x(x_root_vec[i-1], gx_string))
        
        
      }
      
      roots_label_df <- data.frame(x = x_root_vec, y = 0,text = 1:length(x_root_vec))
      
      fixed_segment_df <- data.frame(x = initial_guess,xend = initial_guess, y = 0, yend = x_root_vec[2])
      root_index <- 3
      for (i in 1:(2*((niter-1)))){
        if (i == 1) next
        if (i %% 2 == 0){
          fixed_segment_df <- rbind(fixed_segment_df, data.frame(x = fixed_segment_df$x[i - 1], xend = fixed_segment_df$yend[i-1],
                                                                 y = fixed_segment_df$yend[i-1], yend = fixed_segment_df$yend[i-1]))
        }else{
          fixed_segment_df <- rbind(fixed_segment_df, data.frame(x = fixed_segment_df$yend[i-1], xend = fixed_segment_df$yend[i-1],
                                                                 y = fixed_segment_df$yend[i-1], yend = x_root_vec[root_index]))
          root_index <- root_index + 1
        }
        
      }
      

      g <-  ggplot(data = gx_df, aes(x = x, y=y)) + geom_line(size = 2) + 
        geom_label(data = roots_label_df, aes(x =x, y =y, label = text,fill = factor(text)),  fontface = "bold",size = 8) +
        geom_line(data = x_df, aes(x = x, y=y),size = 1)+
        geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+ 
        geom_segment(data = fixed_segment_df, aes(x = x, y=y,xend = xend, yend = yend),linetype = 2,arrow = arrow(length = unit(.3,"cm"),type = "closed"))+
        ylim(input$yrange[1],input$yrange[2]) + scale_fill_brewer(palette ="Blues",direction = -1) + guides(fill=FALSE) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
        ylab("f(x)") + ggtitle(TeX(convert_string_to_latex(gx_string,"g"))) + theme(axis.text = element_text(size = rel(1.5)),
                                                                                       title = element_text(size = rel(3))) 
      
      roots_table_df <- roots_label_df[,c(1,2)]
      names(roots_table_df) <- c("x","y")
      roots_table_df$y <- get_y_value_from_x(roots_table_df$x,roots_function())
      
      }else{
      g <- NULL
      roots_table_df <- NULL
    }
    
    return(list(plot = g, table = roots_table_df))
    
    
  }
  
  
  output$plot <- renderPlot({


    #der <- eval(D(parse(text = y_at_point_string),"x_point"))
    #d$y2 <-der
    if (input$roots_tabset == "Newton-Raphson"){
      
      plot_and_roots_table <- get_newton_output()
      
    }else if (input$roots_tabset == "Bisection"){
      
      plot_and_roots_table <- get_bisection_output()
      
    }else if (input$roots_tabset == "Fixed-Point"){
      
      plot_and_roots_table <- get_fixed_point_output()
      
    }
    
    root_table <- plot_and_roots_table$table
    
    gplot <- plot_and_roots_table$plot
 
    if (!is.null(gplot)){
      output$roots_df <- renderTable(root_table,digits = 4,rownames = T)
      return(gplot)
    }else{
      return(NULL)
    }
    
    
    

    #plot_ly(d, x = ~x, y = ~y,type = 'scatter',mode = "lines") %>% add_trace(x = ~root, y = ~y_root,mode = "markers")
  })
  
  
  ############
  
  string_integrate <- function(f_string, lower,upper, subsets = 1000L){
    eval(parse(text = paste0("f <- function(x) { return(",f_string,") }")))
    
    integral = integrate(f,lower,upper,subdivisions = subsets)
    integral$value
  }
  
  
  
  get_trapezoidal_output <- function(){
    #browser()
    
    d_function <- safely_get_fx_df(integration_function(),integration_xrange())
    if (!is.null(d_function)){
    lower <- as.numeric(input$trapezoidal_lower_bound)
    upper <- as.numeric(input$trapezoidal_upper_bound)
    trapezoid_x_values <- seq(from = lower, to = upper, length.out = input$integration_nsubsets + 1)
    trapezoid_y_values <- sapply(trapezoid_x_values, get_y_value_from_x, f_string = input$integration_function)
    trapezoid_df <- data.frame(x = trapezoid_x_values, y = trapezoid_y_values)
    vline_df <- data.frame(x = trapezoid_x_values, xend = trapezoid_x_values, y = 0, yend = trapezoid_y_values)
    g <- ggplot(trapezoid_df, aes(x = x, y =y )) + geom_point(color = "red") + geom_area(linetype = 2,color = "red",fill = "indianred1",alpha = .6) + geom_line(data = d_function, aes(x = x, y = y),size = 2) +
      geom_segment(data = vline_df, aes(x = x, xend = xend, y =y , yend = yend),linetype = 2, color = "red4",size = 1) +
      geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
      ylim(input$integration_yrange[1],input$integration_yrange[2]) + coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
      ylab("f(x)") + ggtitle(TeX(convert_string_to_latex(integration_function(),"f"))) + theme(axis.text = element_text(size = rel(1.5)),
                                                                                  title = element_text(size = rel(3))) 
    
      g$layers <- g$layers[c(3,1,2,4,5,6)]
      
    delta_x <- (as.numeric(input$trapezoidal_upper_bound) - as.numeric(input$trapezoidal_lower_bound)) / input$integration_nsubsets
    integral_approx <- 0
    for (i in 1:input$integration_nsubsets){
      integral_approx  <- integral_approx + delta_x * ((trapezoid_y_values[i] + trapezoid_y_values[i + 1])/2)
    }

    integral_actual <- string_integrate(integration_function(),lower,upper)
 
    integral_error <- paste0(round(abs((integral_actual - integral_approx)/integral_actual) * 100,2),"%") 
    
    integral_actual <- integral_actual %>% round(4) %>% as.character()
    integral_approx <- integral_approx %>% round(4) %>% as.character()
    summary_df <- data.frame(a = c("Composite Trapezoid Approximation", "Actual Integral","Error"),b = c(integral_approx,integral_actual,integral_error),stringsAsFactors = F)
    
   
    }else{
      g <- NULL
      summary_df <- NULL
    }
    
    return(list(plot = g, table = summary_df))
    
  }
  
  #color_vec <- c("blue","green","red","yellow","violet","turquoise","cyan","gold","orange","navy","springgreen","darkviolet","deeppink","lawngreen","magenta","skyblue","seagreen","wheat","khaki","indianred","olivedrab","tan","cadetblue","plum","firebrick")
  #
  #color_vec <- c("dodgerblue1","green","yellow","deeppink","chartreuse","cyan","red","gold","magenta")
  #dput(rep(color_vec,10))
  color_vec <- c("dodgerblue1", "green", "yellow", "deeppink", "chartreuse", 
                 "cyan", "red", "gold", "magenta", "dodgerblue1", "green", "yellow", 
                 "deeppink", "chartreuse", "cyan", "red", "gold", "magenta", "dodgerblue1", 
                 "green", "yellow", "deeppink", "chartreuse", "cyan", "red", "gold", 
                 "magenta", "dodgerblue1", "green", "yellow", "deeppink", "chartreuse", 
                 "cyan", "red", "gold", "magenta", "dodgerblue1", "green", "yellow", 
                 "deeppink", "chartreuse", "cyan", "red", "gold", "magenta", "dodgerblue1", 
                 "green", "yellow", "deeppink", "chartreuse", "cyan", "red", "gold", 
                 "magenta", "dodgerblue1", "green", "yellow", "deeppink", "chartreuse", 
                 "cyan", "red", "gold", "magenta", "dodgerblue1", "green", "yellow", 
                 "deeppink", "chartreuse", "cyan", "red", "gold", "magenta", "dodgerblue1", 
                 "green", "yellow", "deeppink", "chartreuse", "cyan", "red", "gold", 
                 "magenta", "dodgerblue1", "green", "yellow", "deeppink", "chartreuse", 
                 "cyan", "red", "gold", "magenta")
  get_simpson_parab <- function(x0,x1,x2,f,color_var){
    fx0 <- eval_function_at_x(f,x0)
    fx1 <- eval_function_at_x(f,x1)
    fx2 <- eval_function_at_x(f,x2)
    
    x <- seq(from = x0, to = x2, length.out = 50)
    
    parab_f <- "(x-x1)*(x-x2)/((x0-x1)*(x0-x2))*fx0 + (x-x0)*(x-x2)/((x1-x0)*(x1-x2))*fx1 + (x-x0)*(x-x1)/((x2-x0)*(x2-x1))*fx2"
  y <- eval(parse(text = parab_f))
  d <- data.frame(x = x, y = y)
    geom_area(data = d,aes(x = x, y = y),linetype = "dashed",fill= color_var,alpha = .5)
  }
  
  eval_function_at_x <- function(f,x,...){
    eval(parse(text = f))
  }
  
  get_simpsons_one_third_output <- function(){
    d_function <- safely_get_fx_df(integration_function(),integration_xrange())
    if (!is.null(d_function)){
 
      a <- as.numeric(input$simpson_third_lower_bound)
      b <- as.numeric(input$simpson_third_upper_bound)
      nn <- input$simpson_integration_subsets
      ff <- integration_function()
      
      x_range <- seq(from = a, to = b, length.out = nn + 1)
      
      parab_list <- list()
    
      for (i in 1:nn){
        x0 <- x_range[i]
        x2 <- x_range[i + 1]
        x1 <- (x0 + x2) / 2
        parab_list <- c(parab_list,get_simpson_parab(x0,x1,x2,ff,color_vec[i]))  
        
      }
      
      all_x_points <- seq(from = a, to = b, length.out = 2*nn + 1)
      all_y_points <- sapply(all_x_points,eval_function_at_x, f = ff)
      points_df <- data.frame(x = all_x_points,y = all_y_points)
      
      
      
      
      g <- ggplot(d_function,aes(x = x, y =y)) + geom_line(size = 2) + parab_list + 
        geom_point(data = points_df, aes(x = x, y = y), size = 3)  + geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
        coord_cartesian(xlim = ranges$x, ylim = ranges$y) + 
        ylab("f(x)") + ggtitle(TeX(convert_string_to_latex(ff,"f"))) + theme(axis.text = element_text(size = rel(1.5)),
                                                                                                 title = element_text(size = rel(3))) 
      
      
      
      integral_actual <- string_integrate(ff,a,b)
      middle_vals <- all_x_points[(1:nn) * 2]
      end_vals <- x_range[c(-1,-(nn+1))]
      integral_approx <- (b-a) * (eval_function_at_x(ff,a) + 4 * sum(sapply(middle_vals,eval_function_at_x,f = ff)) + 2 * sum(sapply(end_vals,eval_function_at_x,f = ff)) + eval_function_at_x(ff,b)) / (6*nn)
   
      integral_error <- paste0(round(abs((integral_actual - integral_approx)/integral_actual) * 100,2),"%") 
      
      integral_actual <- integral_actual %>% round(4) %>% as.character()
      integral_approx <- integral_approx %>% round(4) %>% as.character()
      summary_df <- data.frame(a = c("Simpson's 1/3 Approximation", "Actual Integral","Error"),b = c(integral_approx,integral_actual,integral_error),stringsAsFactors = F)

      
    }else{
      g <- NULL
      summary_df <- NULL
    }
    
    return(list(plot = g, table = summary_df))
      
  }
  
  
  output$integration_plot <- renderPlot({
    
    if (input$integration_tabset == "Trapezoidal"){
    
      plot_and_summary_table <- get_trapezoidal_output()
      
      
    }else if (input$integration_tabset == "Simpson's 1/3"){
      
      plot_and_summary_table <- get_simpsons_one_third_output()
    }
    
    
    summary_table <- plot_and_summary_table$table
    
    gplot <- plot_and_summary_table$plot
    
    if (!is.null(gplot)){
      output$integration_summary_df <- renderTable(summary_table,colnames = F)
      return(gplot)
    }else{
      return(NULL)
    }
    
  })
  
  
  
  # h4("Bisection Limits"),
  # textInput("bisect_start","Start"),
  # textInput("bisect_end","End"),
  # textInput("newton_function","f(x)",value = "exp(-x)*sin(x)"),
  # sliderInput("bisect_niter","Iterations",min = 1, max = 8,value = 2),
  # tableOutput("bisect_roots_df")
  # )
  # 
  
}