library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage( #theme = shinytheme("cyborg"), 

 titlePanel("Data ExploreR")
  ,
  sidebarLayout(
    sidebarPanel(width=3,
      fileInput("file","Choose a CSV File", accept=c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv')),
      
      tags$hr(),
      
#      checkboxInput("header","Header",TRUE),
      radioButtons("sep","Separator",c(Comma=',',Semicolon=';',Tab='\t')),

      tags$hr(),
      
      numericInput("num","Number of observations to view:",10),
      
      tags$hr(),
      
      uiOutput("columnNames") 
      
      ),
    
    # Show a tabset that includes the table view and summary of data
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Documentation", htmlOutput("help")),
        tabPanel(p(textOutput("dim")),title="Data", tableOutput("data")),
        tabPanel("Structure", verbatimTextOutput("structure")),
        tabPanel("Decision Tree", plotOutput("plotDTree")), 
        tabPanel("Model Accuracy", verbatimTextOutput("modelAcc"))
        
      )
    )
  )
))
