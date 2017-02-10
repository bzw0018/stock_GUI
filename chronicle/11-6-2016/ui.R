library(shiny)
shinyUI(fluidPage(
  fluidRow(
  titlePanel("Stock Price Machine Learning App."),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file",accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')), 
      tags$hr(),
      selectInput(inputId="platform" , label = "Operating System", choices = c("MAC OS","Windows","LINUX")),
      tags$hr(),
      selectInput(inputId="Multi_Bino" , label = "Type of the Dependent Variable", choices = c("Multinomial","Binomial")),
      tags$hr(),
      checkboxGroupInput(inputId="FS_Method" , label = "Feature Selection Method", choices = c("Random Forrest","Lasso","Fast Selection","itself","Literature")),
      
      
      conditionalPanel(
        condition = "input.FS_Method.indexOf ('Random Forrest')>-1",
        selectInput("RF_run", "Random Forrest is performed?", selected = NULL,
                    list("","Yes", "No"))),
      conditionalPanel(
        condition = "input.FS_Method.indexOf ('Lasso')>-1",
        selectInput("Lasso_run", "Lasso is performed?", selected = NULL,
                    list("","Yes", "No"))),
      conditionalPanel(
        condition = "input.FS_Method.indexOf ('Fast Selection')>-1",
        selectInput("FCBF_run", "Fast Selection is performed?", selected = NULL,
                    list("","Yes", "No"))),
      conditionalPanel(
        condition = "input.FS_Method.indexOf ('itself')>-1",
        selectInput("itself_run", "itself is performed?", selected = NULL,
                    list("","Yes", "No"))),
      conditionalPanel(
        condition = "input.FS_Method.indexOf ('Literature')>-1",
        selectInput("lit_vars", "Select the variables", selected = NULL,
                    choices = NULL, multiple = TRUE)),
      checkboxGroupInput(inputId="conso_feat" , label = "Consolidating features from all the Feature Selection Method", choices = c("Yes","No")),
      tags$hr(),
      selectInput(inputId="tuning" , label = "Do you want to tune the learning algorithms?", choices = c("Yes","No"), selected = "No"),
      tags$hr(),
      textInput(inputId="n_folds" , label = "No. of cross validation folds"),
      tags$hr(),
      selectInput(inputId="balanc_proc" , label = "Applying sample balancing (SMOTE, RUS, Hybrid)?", choices = c("Yes","No"), selected = "No"),
      conditionalPanel(
        condition = "input.balanc_proc.indexOf ('Yes')>-1",
        selectInput("B_alg", "Name of the sample balancing?", selected = NULL,
                    list("","SMOTE", "RUS","Hybrid"))),
      conditionalPanel(
        condition = "input.B_alg.indexOf ('Hybrid')>-1",
        textInput(inputId="H_por_low" , label = "Proportion of diffrences for increasing the minority?")),
      conditionalPanel(
        condition = "input.B_alg.indexOf ('Hybrid')>-1",
        textInput(inputId="H_por_high" , label = "Proportion of diffrences for increasing the minority?")),
      tags$hr(),
      textInput(inputId="email" , label = "Email address for reporting the results"),
      actionButton("runit","Run")
      
    ),
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
        tabsetPanel(tabPanel("Dependant Variable",radioButtons("inRadioButtGroup","RadioButt group input:",choices = "")),
                    tabPanel("Independant Variables",checkboxGroupInput("incheckboxGroupInput","Checkbox group input",choices = "")),
                    tabPanel("Data",tableOutput("table")),
                    tabPanel("About", verbatimTextOutput("summarys"))
        ))
      
      #uiOutput("tb")
      
    )
    
  ))
))