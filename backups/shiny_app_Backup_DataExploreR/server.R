library(shiny)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(AppliedPredictiveModeling)
#suppressMessages(library(rattle))

# By default, the file size limit is 5MB. Here I've raised the limit to 10MB.
options(shiny.maxRequestSize = 10*1024^2)

factorColumns <- c()

shinyServer(function(input,output){
    
    # get the uploaded dataset
    dataUpload <- reactive({
        inFile <- input$file
        if(is.null(inFile))
        return(NULL)
        fileData <- read.csv(inFile$datapath, header=input$header, sep=input$sep, stringsAsFactors=FALSE, na.strings=c("NA", "", " "))
        
        # keep only complete records
        fileData <- fileData[complete.cases(fileData),]
        
        # factor columns search: if count of levels <= 16 then it can be a factor. 
        factorCols <- colnames(fileData[sapply(fileData, function(x) nlevels(as.factor(x)) < 16)])
        factorColumns <<- c(factorColumns, factorCols)
        
        # convert those columns to factors
        fileData[, factorCols] <- lapply(fileData[, factorCols], as.factor)

        fileData
    })
    
    output$columnNames <- renderUI({
        df <- dataUpload()
        if (is.null(df)) return(NULL)
        selectInput("columnNames", "Select Feature Column", factorColumns)
    })
    
    # dimensions of the dataset
    output$dim<-renderText({
    	paste("This dataset has:",dim(dataUpload())[1],"complete observations over ",dim(dataUpload())[2],"variables.")
    	
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
    output$plot <- renderPlot({
        df <- data.frame(dataUpload())
        selectedColumn <- input$columnNames
        allOtherColumns <- colnames(df[, colnames(df) != selectedColumn])
        allOtherColumns <- paste(allOtherColumns, sep = ",", collapse = "+")
        formula <- as.formula(paste(selectedColumn, '~', allOtherColumns))
        print(formula)
        fit <- train(formula, method = "rpart", data = df)
        #fancyRpartPlot(fit$finalModel)
        
        tree <- fit$finalModel
        prp(tree, extra = 1, box.col=c("orange", "lightseagreen")[tree$frame$yval])
    })
    
	
})