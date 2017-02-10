
#shiny part starts here
library(shiny)
library(shinyFiles)
library(tcltk2)
library(svDialogs)

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
  
  parameters<-reactiveValues()
 
  observe({
    file1 <- input$file
    if(is.null(file1)){return()}
    parameters$data_base<-read.table(file=file1$datapath, sep=',', header = TRUE, stringsAsFactors = TRUE)
    parameters$data<-parameters$data_base[,parameters$uservars]
    parameters$data_ui<-head(parameters$data,n=10)
    parameters$target<-input$inRadioButtGroup
    parameters$uservars<-input$incheckboxGroupInput
    parameters$lit_vars<-input$lit_vars
    parameters$file_name<-input$file$name
    parameters$tuning<-input$tuning
    
    parameters$Multi_Bino<-input$Multi_Bino
    parameters$platform<-input$platform
    parameters$FS_Method<-input$FS_Method
    parameters$method_fs<-paste(parameters$FS_Method, collapse = "_")

    parameters$RF_run<-input$RF_run
    parameters$Lasso_run<-input$Lasso_run
    parameters$FCBF_run<-input$FCBF_run
    parameters$itself_run<-input$itself_run
    parameters$lit_run<-input$lit_run

    parameters$conso_feat<-input$conso_feat
    parameters$n_folds<-input$n_folds
    parameters$balanc_proc<-input$balanc_proc
    parameters$B_alg<-input$B_alg
    parameters$H_por_low<-input$H_por_low
    parameters$H_por_high<-input$H_por_high
    parameters$email<-input$email
    parameters$data_base<-read.table(file=file1$datapath, sep=',', header = TRUE, stringsAsFactors = TRUE)
    parameters$dataset<-parameters$data_base[,parameters$uservars]
    parameters$target<-input$inRadioButtGroup
    parameters$uservars<-input$incheckboxGroupInput
    parameters$lit_vars<-input$lit_vars
    parameters$file_name<-input$file$name
    parameters$tuning_input<-input$tuning_input

  })
  

  output$summarys<-renderPrint({
    itself(FFS(Lasso_Binomial(Random_Forrest(ADP((data_cleaning(initialization(reading(parameters)))))))))
    #FFS(Lasso_Binomial(Random_Forrest(ADP((data_cleaning(initialization(reading(parameters))))))))
    #Lasso_Binomial(ADP((data_cleaning(initialization(reading(parameters))))))
    #str(initialization(reading(parameters)))
    #data_cleaning(initialization(reading(parameters)))
    #initialization(reading(parameters))
    
    })
  
  # this reactive output contains the summary of the da.0taset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(parameters$data)){return ()}
    input$file
  })
 
  #This reactive output contains the dataset and display the dataset in table format
  output$table <- renderTable({
    if(is.null(parameters$data_ui)){return ()}
    parameters$data
  })
 
  #=================Selection module
  observe({
    
    inFile <- input$file
    
    req(inFile)

    vars <- names(parameters$data_base)
   
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
  
  observeEvent(input$folder, {
    
   user <- dlgMessage("Other the subfolders would be assigned under the major folder.", type="ok")
   parameters$root_folder_input<-tk_choose.dir(default = "", caption = "Select directory")
   
    
  })

})
