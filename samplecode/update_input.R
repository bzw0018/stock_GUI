library(shiny)

ui <- fluidPage(
  sidebarLayout(sidebarPanel(
  h2('The uploaded file data'),
  dataTableOutput('mytable'),
  fileInput('file', 'Choose info-file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )
  ),
  # Taken from: http://shiny.rstudio.com/gallery/file-upload.html
  tags$hr(),
  checkboxInput('header', 'Header', TRUE),
  
  radioButtons('sep', 'Separator',
               c(Comma=',',
                 Semicolon=';',
                 Tab='\t'),
               ','),
  radioButtons('quote', 'Quote',
               c(None='',
                 'Double Quote'='"',
                 'Single Quote'="'"),
               '"'),

  ################################################################
  
 # actionButton("choice", "incorporate external information"),
  
  selectInput("columns", "Select Columns", choices = NULL)), 
  
 mainPanel(
   
   conditionalPanel(
     condition = "output.fileUploaded",
     
     h2("Hamidreza Ahady Dolatsara, Instructor", 
        tags$br(),
        "Fadel Megahed, Assistant Professor",
        tags$br(),
        "Bin Weng, PhD Candidate",
        tags$hr(),
        
        tags$img(src='https://upload.wikimedia.org/wikipedia/en/thumb/7/7f/Auburn_University_seal.svg/480px-Auburn_University_seal.svg.png', heigth=200, width=200))
   ),
   
   
   conditionalPanel(
     condition = "!output.fileUploaded",
     tabsetPanel(tabPanel("Dependant Variable",checkboxGroupInput("inRadioButtGroup","RadioButt group input:",choices = "")),
                 tabPanel("Independant Variables",checkboxGroupInput("incheckboxGroupInput","Checkbox group input",choices = "")),
                 tabPanel("Data",tableOutput("table")),
                 tabPanel("About", verbatimTextOutput("summarys"))
     ))
   
   
   
   
 )#mainPanel

  
))

server <- function(input, output, session) { # added session for updateSelectInput
  
  getData <- reactive({
    if(is.null(input$file)) return(NULL)
    input$file
  })
  output$fileUploaded <- reactive({
    return(is.null(getData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  input_object<-reactiveValues()
  
  observe({
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    
    # Changes in read.table 
    input_object$data<- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(input_object$data)
    # Update select input immediately after clicking on the action button. 
    updateCheckboxGroupInput(session, "inRadioButtGroup","RadioButt group input:", choices = vars)
    updateCheckboxGroupInput(session, "incheckboxGroupInput", "Checkbox group input", choices = vars)
    updateSelectInput(session, "columns","Select Columns", choices = vars)
    
    
    
 } )
  output$summarys<-renderPrint({
  input$incheckboxGroupInput
    
  })
  
  
  output$table <- renderTable({
    if(is.null(input_object$data)){return ()}
    head(input_object$data)
  })
  
  
}
shinyApp(ui, server)