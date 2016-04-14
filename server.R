library(shiny)
library(rpart)
library(rpart.plot)
library(caret)
#library(dplyr)
#library(AppliedPredictiveModeling)
library(gplots)
#library(e1071)
# Not for shiny io server
suppressMessages(library(rattle))
#library(RColorBrewer)

set.seed(174)

# By default, the file size limit is 5MB. Here I've set the limit to 10MB.
options(shiny.maxRequestSize = 10*1024^2)

# global variables (TODO: rework)
factorColumns <- c()
selectedColumn <- c()
allOtherColumns <- c()

# TODO: check accuracy tests with test and train data for cross reference
#dataTrain <- NULL
#dataTest <- NULL

loadData <- reactive({
    function(dataIn, predColumn) {
        df <- data.frame(dataIn())
        selectedColumn <<- predColumn
        allOtherColumns <<- colnames(df[, colnames(df) != selectedColumn])
        allOtherColumns <<- paste(allOtherColumns, sep = ",", collapse = "+")
    }
})

trainModel <- reactive ({
    function() {
        formula <- as.formula(paste(selectedColumn, '~', allOtherColumns))
        fit <- train(formula, method = "rpart", data = df)
        fit$finalModel
    }
})

shinyServer(function(input,output){
    
    # get the uploaded dataset
    # idea from shiny site: http://shiny.rstudio.com/gallery/upload-file.html
    dataUpload <- reactive({
        inFile <- input$file
        if(is.null(inFile))
        return(NULL)
        fileData <- read.csv(inFile$datapath, header=input$header, sep=input$sep, stringsAsFactors=FALSE, na.strings=c("NA", "", " "))
        #fileData <- read.csv(inFile$datapath, header=TRUE, sep=input$sep, stringsAsFactors=FALSE, na.strings=c("NA", "", " "))
        
        # keep only complete records
        fileData <- fileData[complete.cases(fileData),]
        
        # factor columns search: if count of levels <= 16 then it can be a factor. 
        factorCols <- colnames(fileData[sapply(fileData, function(x) nlevels(as.factor(x)) < 6)])
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
        loadData(dataUpload, input$columnNames)
        trainModel()
    #    trainModel(dataUpload, input$columnNames)
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
        fancyRpartPlot(fit$finalModel)
    })
    
    # generate model accuracy
    output$modelAcc <- renderPrint ({
        printcp(fit$finalModel)
        #printcp(trainModel)
    })
    
    output$help <- renderUI ({
        HTML("<H2>Help</H2></br>
            <ol>
                <li>About this app: 
                    </br>It's purpose is to visualize an optimal decision tree for a selected target field, based on the data available. 
                    </br>Several types of preprocessing happens to make this possible. (see limitations for details)
                </li>
                </br>
                <li>Use:   
                    <ul>
                        <li>Upload a csv file</li>
                        <li>Select a column from the drop down</li>
                        <li>Click the 'decision tree' tab to draw the plot</li>
                    </ul>
                </li>
                </br>
                <li>Tabs: 
                    <ul>
                        <li>Data: html table displaying top x rows from the file. (X is selectable from the 'nr of observations') </li>
                        <li>Structure: overview of the data types in the file. </li>
                        <li>Decision tree: the plot of the learned decision tree from the data, for selected column </li>
                        <li>Accuracy: shows the accuracy metrics for the learned tree model (needs improvements, a nice plot) </li>
                        <li>Help: this information. </li>
                    </ul>
                </li>
                </br>
                <li>Limitations & remarks:
                    <ul>
                        <li>Only files with header will work. </li>
                        <li>File must be less than 1MB in size. </li>
                        <li>Only complete rows are used, other with missing values are filtered out. 
                        <li>Only columns with a maximum of 5 distinct values can be used in the selection drop down. </li>
                        <li>Because of time limitation it is a simple app. </li>
                        <li>Due to missing rattle package on shinyapps.io, the plot is not 'fancy'. </li>
                    </ul>
                </li>
                </br>
                <li>Future extensions & improvements: 
                    <ul>
                        <li>Create prediction input form, and predict outcome with the model build from the training set (import file). </li>
                        <li>Add Accuracy measures for the model, with a nice plot to visualize. </li>
                    </ul>
                </li>
                </br>

            </ol>
             ")
    })
	
})