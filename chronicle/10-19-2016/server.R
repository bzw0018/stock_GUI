
#shiny part starts here
library(shiny)
source("functions.R")
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input,output,session){
  output$contents <- renderText({
  assign('platform',input$platform)
  file_input<-input$file
  stk_comp<-file_input$name
  trg<-input$RadioButtGroup
  })
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=',', header = TRUE, stringsAsFactors = TRUE, nrows=20)
    
  })
  
  
  # this reactive output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  #=================Selection module
  observe({
    dsnames <- names(data())
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    updateRadioButtons(session, "RadioButtGroup",
                             label = "Select the Dependant Variable",
                             choices = cb_options,
                             selected = "")
    updateCheckboxGroupInput(session, "inCheckboxGroup",
                             label = "Check Box Group",
                             choices = cb_options,
                             selected = dsnames)
  })
  
  output$choose_dataset <- renderUI({
    selectInput("data", "Data set", as.list(data_sets))
  })
  
  # Check boxes
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    
    colnames <- names(contents)
    
  })

  #=================End of the Selection module
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h2("Hamidreza Ahady Dolatsara, Instructor", 
         tags$br(),
      "Fadel Megahed, Assistant Professor",
      tags$br(),
      "Bin Weng, PhD Candidate",
      tags$hr(),
      
         tags$img(src='https://upload.wikimedia.org/wikipedia/en/thumb/7/7f/Auburn_University_seal.svg/480px-Auburn_University_seal.svg.png', heigth=200, width=200))
    else
      tabsetPanel(tabPanel("Dependant Variable",checkboxGroupInput("RadioButtGroup","RadioButt group input:",choices = "")),
                  tabPanel("Independant Variables",checkboxGroupInput("inCheckboxGroup","Checkbox group input:",choices = "")),
                  tabPanel("Data", tableOutput("table")),tabPanel("About file", tableOutput("filedf"))
                  )
  })
})
#Shiny part ends here