
server <- function(input, output) {
  
  
  a <- reactive(input$a)
  e <- reactive(input$e)
  om <- reactive(input$om)
  i <- reactive(input$i)
  w <- reactive(input$w)
  
  # renderPlotly() also understands ggplot2 objects!
  output$plot <- renderPlotly({
    b = sqrt(a()^2*(1-e()^2))
    
    x = seq(from = 0, to = a(), length.out = 100)
    y = sqrt(b^2*(1 - x^2/a()^2))
    xx = c(x,rev(x),-(x),-rev(x))
    yy = c(y,-rev(y),-y,rev(y))
    
    
    d <- data.frame(x = xx, y =yy, z = 0)
    
    w = w() * pi / 180
    om = om() * pi / 180
    i = i() * pi / 180
    x1 = cos(om)*cos(w)-sin(om)*cos(i)*sin(w)
    x2 = sin(om)*cos(w)+cos(om)*cos(i)*sin(w)
    x3 = sin(i)*sin(w)
    
    y1 = -cos(om)*sin(w)-sin(om)*cos(i)*cos(w)
    y2 = -sin(om)*sin(w)+cos(om)*cos(i)*cos(w)
    y3 = sin(i)*cos(w)
    
    z1 = sin(i)*sin(om)
    z2 = -sin(i)*cos(om)
    z3 = cos(i)
    A = matrix(c(x1,x2,x3,y1,y2,y3,z1,z2,z3),3,3)
    
    d <- t(d)
    aa <- A%*%d
    ab <- t(aa)
    dd <- data.frame(ab)
    names(dd) <- c("x","y","z")
    
    p <- plot_ly(dd,x = ~x, y = ~y, z = ~z, type ="scatter3d", mode = 'lines')
    
    
    p
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}

