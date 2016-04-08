library(shiny)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(AppliedPredictiveModeling)
library(gplots)
# Not for shiny io server
suppressMessages(library(rattle))
library(RColorBrewer)

set.seed(174)

# By default, the file size limit is 5MB. Here I've set the limit to 1MB.
options(shiny.maxRequestSize = 1*1024^2)

factorColumns <- c()

dataTrain <- NULL
dataTest <- NULL

trainModel <- function(dataIn, predColumn) {
    df <- data.frame(dataIn())
    selectedColumn <- predColumn
    allOtherColumns <- colnames(df[, colnames(df) != selectedColumn])
    allOtherColumns <- paste(allOtherColumns, sep = ",", collapse = "+")
    formula <- as.formula(paste(selectedColumn, '~', allOtherColumns))
    fit <- train(formula, method = "rpart", data = df)
    fit$finalModel
}

shinyServer(function(input,output){
    
    # get the uploaded dataset
    # idea from shiny site: http://shiny.rstudio.com/gallery/upload-file.html
    dataUpload <- reactive({
        inFile <- input$file
        if(is.null(inFile))
        return(NULL)
        #fileData <- read.csv(inFile$datapath, header=input$header, sep=input$sep, stringsAsFactors=FALSE, na.strings=c("NA", "", " "))
        fileData <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, stringsAsFactors=FALSE, na.strings=c("NA", "", " "))
        
        # keep only complete records
        fileData <- fileData[complete.cases(fileData),]
        
        # factor columns search: if count of levels <= 16 then it can be a factor. 
        factorCols <- colnames(fileData[sapply(fileData, function(x) nlevels(as.factor(x)) < 16)])
        factorColumns <<- c(factorColumns, factorCols)
        
        # convert those columns to factors
        fileData[, factorCols] <- lapply(fileData[, factorCols], as.factor)
        
        # TODO : the non-numeric factor thing for the labels
        #f = "Survived"
        #levels <- unique(c(dataset[[f]]))
        #dataset[[f]] <- factor(dataset[[f]],labels=make.names(levels))

        fileData
    })

    currentModelTrained <- reactive({
        trainModel(dataUpload, input$columnNames)
    })
    
    output$columnNames <- renderUI({
        df <- dataUpload()
        if (is.null(df)) return(NULL)
        selectInput("columnNames", "Select Feature Column", factorColumns)
    })
    
    # dimensions of the dataset
    output$dim<-renderText({
    	paste("This dataset has:",dim(dataUpload())[1]," complete observations over ",dim(dataUpload())[2]," variables.")
    	
    })
    # generate an HTML table view of the data
    output$data <- renderTable({
        head(dataUpload(),n=input$num)
    })
    
    # generate the structure of the dataset
    output$structure <- renderPrint({
         str(dataUpload())
    })
    
    # generate a summary of the data
    output$summary <- renderPrint({
        dataset <- dataUpload()
        summary(dataset)
    })
        
    # generate the feature tree
    output$plotDTree <- renderPlot({
        
#         df <- data.frame(dataUpload())
#         selectedColumn <- input$columnNames
#         allOtherColumns <- colnames(df[, colnames(df) != selectedColumn])
#         allOtherColumns <- paste(allOtherColumns, sep = ",", collapse = "+")
#         formula <- as.formula(paste(selectedColumn, '~', allOtherColumns))
#         fit <- train(formula, method = "rpart", data = df)
#             
#         # fancy plot as part of the rattle package does deploy on shinyapps.io
#         #fancyRpartPlot(fit$finalModel)
#         trainedModel <<- fit$finalModel
#         tree <- fit$finalModel
#         prp(tree, extra = 1, box.col=c("orange", "lightseagreen")[tree$frame$yval])
        
#        prp(currentModelTrained(), extra = 1, box.col=c("orange", "lightseagreen")[currentModelTrained()$frame$yval])
        fancyRpartPlot(fit$finalModel)

    })
    
    output$modelAcc <- renderPrint ({
        printcp(fit$finalModel)
    })
    
    output$help <- renderUI ({
        HTML("<H2>Help</H2>
             </br>1. How to use this app.
             </br>2. Limitations")
    })
	
})