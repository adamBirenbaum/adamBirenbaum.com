library(shiny)

pageWithSidebar(
  headerPanel(''),
  sidebarPanel(
    sliderInput("nseg","Seg",min = 1, max = 7,value = 3,step = 1)
  ),
  mainPanel(
    fluidRow(
      plotOutput('plot',click = "click")
    ),
    fluidRow(
      plotOutput('plot2',click = "click2",width = "100%")
    )
    
  )
)