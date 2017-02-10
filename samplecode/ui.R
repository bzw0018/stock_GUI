shinyUI(fluidPage(
  headerPanel("Power Calculator"),
  fluidRow(

    column(4,
           wellPanel(
             actionButton("go", "Go"),
             actionButton("gog", "Gog"),
             selectInput("alpha", "Significance Level",c("1", "2","3"),selected = "1")
            
           )),

  column(12,
         wellPanel(
           plotOutput("plot"),
           textOutput("texti")
         ))
)
))