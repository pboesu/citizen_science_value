#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  pageWithSidebar(
    headerPanel('Where to record?'),
    sidebarPanel(
      selectInput('yearcol', 'Year', unique(AllBirds$year)),
      selectInput('valcol', 'Value Type', names(AllBirds[c(2,6)]))),
    mainPanel(
      plotOutput('plot1')
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #load("Birds.RData")
  
  # subset by year
  Birds <- reactive({subset(AllBirds,AllBirds$year == as.numeric(as.character(input$yearcol)))})
    
  # get the value column
  colscheme <- reactive({
      Birds() %>% mutate(color = pull(Birds(),input$valcol))
    })
  
    
    output$plot1 <- renderPlot({
      
      par(mar = c(5.1, 4.1, 0, 1))
      ggplot(colscheme(), aes(y=LATITUDE, x=LONGITUDE,
           col = color)) + geom_point() + coord_cartesian(xlim = range(AllBirds$LONGITUDE), ylim=range(AllBirds$LATITUDE))
    })
    
  }
# Run the application 
shinyApp(ui = ui, server = server)

