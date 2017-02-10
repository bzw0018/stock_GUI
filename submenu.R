library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "salam"),
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Operating Software", icon = icon("bar-chart-o"),
               # Input directly under menuItem
               selectInput("inputTest", "Input Test",
                           choices = c("a", "b", "c", "d"), 
                           width = '98%'),
               
               # Input inside of menuSubItem
               menuSubItem(icon = NULL,
                           sliderInput("inputTest2", "Input test 2", min=0, max=10, value=5,
                                       width = '95%')
               )
      )
    )
  ),
  dashboardBody()
  
)

server <- function(input, output) {}

shinyApp(ui, server)
