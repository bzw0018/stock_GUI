
#shiny part starts here
library(shiny)
source("functions.R")
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input,output,session){
  
  getData <- reactive({
    if(is.null(input$file)) return(NULL)
    input$file
  })
  output$fileUploaded <- reactive({
    return(is.null(getData()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$contents <- renderText({
  assign('platform',input$platform)
  file_input<-input$file
  stk_comp<-file_input$name
  trg<-input$RadioButtGroup
  })
  
  # This reactive function will take the inputs from UI.R and use them for read.table() to read the data from the file. It returns the dataset in the form of a dataframe.
  # file$datapath -> gives the path of the file
  input_object<-reactiveValues()
  parameters<-reactiveValues()
 
  observe({
    file1 <- input$file
    if(is.null(file1)){return()}
    input_object$data_base<-read.table(file=file1$datapath, sep=',', header = TRUE, stringsAsFactors = TRUE, nrows=20)
    input_object$data<-input_object$data_base[,input_object$parameters$uservars]
    input_object$parameters$target<-input$inRadioButtGroup
    input_object$parameters$uservars<-input$incheckboxGroupInput
    input_object$parameters$lit_vars<-input$lit_vars
    input_object$parameters$file_name<-input$file$name
    input_object$parameters$tuning<-input$tuning

    input_object$parameters$Multi_Bino<-input$Multi_Bino
    input_object$parameters$platform<-input$platform
    input_object$parameters$FS_Method<-input$FS_Method

    input_object$parameters$RF_run<-input$RF_run
    input_object$parameters$Lasso_run<-input$Lasso_run
    input_object$parameters$FCBF_run<-input$FCBF_run
    input_object$parameters$itself_run<-input$itself_run
    input_object$parameters$lit_run<-input$lit_run

    input_object$parameters$conso_feat<-input$conso_feat
    input_object$parameters$n_folds<-input$n_folds
    input_object$parameters$balanc_proc<-input$balanc_proc
    input_object$parameters$B_alg<-input$B_alg
    input_object$parameters$H_por_low<-input$H_por_low
    input_object$parameters$H_por_high<-input$H_por_high
    input_object$parameters$email<-input$email

  })
  

  output$summarys<-renderPrint({
    initialization(input_object)
    
    })
  
  # this reactive output contains the summary of the da.0taset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(input_object$data)){return ()}
    input$file
  })
 
  #This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(input_object$data)){return ()}
    input_object$data
  })
 
  #=================Selection module
  observe({
    
    inFile <- input$file
    
    req(inFile)

    vars <- names(input_object$data_base)
   
    updateRadioButtons(session, "inRadioButtGroup","RadioButt group input:", choices = vars)
    updateCheckboxGroupInput(session, "incheckboxGroupInput", "Checkbox group input", choices = vars, selected = vars)
    updateSelectInput(session, "lit_vars","Select the variables", choices = vars)

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
  
  #=================functions part
  
  # randomValss <- eventReactive(input$runit, {
  #   
  #   
  #   input_object<-initialization(input_object)
  #   return(summary(input_object$settings))
  #   
  # })

})
