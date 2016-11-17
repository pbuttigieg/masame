library(shiny)
library(vegan)
library(RColorBrewer)

#create objects during the preprocessing and use if.exists to continue with bg and cex

data(dune)
data(dune.env)

shinyServer(function(input, output){
  
  # Handle uploaded response data...
  datasetInput <- reactive({		
    input$dataset
  })
  
  datasetFile <- reactive({
    if (input$useExampleData == TRUE) {
      dune
    } else if (input$useExampleData == FALSE) {
      inFile <- datasetInput()
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(
        file = inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        row.names = if(input$rownames == 0){NULL} else{input$rownames}
      )
    }
  })

  
  #introduce an option where files containing coloring factors can be uploaded
 
  datasetColorInput <- reactive({		
    input$colorfile
  })
  
  datasetColor <- reactive({
    if (input$useExampleDataColor == TRUE) {
      dune.env
    } else if (input$useExampleDataColor == FALSE) {
      inColorFile <- datasetColorInput()
      
      if (is.null(inColorFile))
        return(NULL)
    
    read.csv(
      file = inColorFile$datapath,
      header = input$header,
      sep = input$sep,
      row.names = if(input$rownames2 == 0){NULL} else{input$rownames}
    ) #ends reading of csv file
    
    } #ends false of useExampleDataColor
    })  #ends datasetColor
     
  
  # Create UI element for selection of colouring variable
  output$colorVariable <- renderUI({    
    selectInput(
    inputId = "colorVariable", 
    label = "Select a factor, according to which the objects are going to be colored",
    choices = names(datasetColor()),
    selected = NULL
    
    ) 
  })
    
  #create a file in which factors that determine point size are going to be selected
  
  datasetSizeInput <- reactive({		
    input$sizefile
  })
  
  datasetSize <- reactive({
    if (input$useExampleDataSize == TRUE) {
      dune.env
    } else if (input$useExampleDataSize == FALSE) {
      inSizeFile <- datasetSizeInput()
      
      if (is.null(inSizeFile))
        return(NULL)
      
      read.csv(
        file = inSizeFile$datapath,
        header = input$header,
        sep = input$sep,
        row.names = if(input$rownames3 == 0){NULL} else{input$rownames}
      ) #ends reading of csv file
      
    } #ends false of useExampleDataSize
  })  #ends datasetSize 
  
  # Create UI element for selection of sizing variable
  output$scalingVariable <- renderUI({    
    selectInput(
      inputId = "scalingVariable", 
      label = "Select a numeric variable, according to which the objects are going to be scaled",
      choices = names(datasetSize()),
      selected = NULL
      
    ) 
  })
  
  # Transform data if requested...
  transData <- reactive({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return()
    
    if(
      !is.numeric(as.matrix(datasetFile())) &
      input$transform != 'none'
    )
      stop("Non-numeric values detected! Transformation invalid.")
    
    if (input$transform == 'none'){
      transData <- datasetFile()
    } else {
      decostand(
        datasetFile(),
        method = input$transform
      )
    }
    
  })
  
  # Perform PCA analysis
  pca <- reactive({ 
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return()
    rda(
      transData()
    )
  })
  
  # Prepare output...
  
 # output$plot <- renderPlot({
    
 #   if (is.null(input$dataset) & input$useExampleData == FALSE)
#      return()
    
#    biplot(
#      pca(),
#      type = input$labels,
#      scaling = as.numeric(input$scaling),
#      col = c("red", "blue")
#    )
#  })
  
  output$eigenvals <- renderPrint({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return(print("Please upload data"))
    
    eigenvals(pca())
  })
  
  output$print <- renderPrint({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return(print("Please upload data"))
    
    print(pca())
  })
  
  output$objectScores <- renderPrint({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return(print("Please upload data"))
    
    print(pca()$CA$u.eig)
  })
  
  output$variableScores <- renderPrint({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return(print("Please upload data"))
    
    print(pca()$CA$v.eig)
  })
  
  
  
  
    output$plot <- renderPlot({
      par(mfrow = c(1, 2), mai = c(1, 1, 1, 1))
      
      if (is.null(input$dataset) & input$useExampleData == FALSE)
        return()
      
     # if (is.null(datasetColor()) & input$useExampleDataColor == FALSE & is.null(datasetSize()) & input$useExampleDataSize == FALSE)
    #  {
     #   biplot(
    #      pca(),
    #      scaling = as.numeric(input$scaling)
    #    )
    #  }
      
      if (!is.null(datasetColor()) || input$useExampleDataColor == TRUE){
        
        if(nrow(datasetColor()) != nrow(datasetFile()))
          stop("The number of rows in your colour-  data set is not the same as that of your response data set. Please make sure both data sets have the same number of objects.")
        
        if(!is.numeric(datasetColor()[, input$colorVariable]) & input$factorType == "Numeric")
          stop("The color variable is not numeric. Please specify that the variable is a factor in the alternatives below.")
    
        if(is.numeric(datasetColor()[, input$colorVariable]) & input$factorType == "Factor") 
          stop("The color variable is not a factor. Please specify that the variable is numeric in the alternatives below.")
        
        if (input$factorType == "Numeric"){
      
          quantiles <- quantile(
            datasetColor()[, input$colorVariable],
            probs = seq(0, 1, 0.2)
          )
      
          transparency <- decostand(
            quantiles,
            method = "max"
          )
      
          colN <- 1
          par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
          
        } else if (input$factorType == "Factor") {
      
            cols <- rainbow(length(levels(as.factor(datasetColor()[, input$colorVariable]))))
            palette(cols)
            colF <- 1
          
            par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
        } else {
          stop("Error: colouring variable must be numeric or factor")
        }
      }
      
    if (!is.null((datasetSize())) || input$useExampleDataSize == TRUE){
      
        if(nrow(datasetSize()) != nrow(datasetFile()))
          stop("The number of rows in your size-  data set is not the same as that of your response data set. Please make sure both data sets have the same number of objects.")

      if(is.numeric(as.numeric(datasetSize()[, input$scalingVariable]))) {
  
          scalingRange <- decostand(
              as.numeric(datasetSize()[, input$scalingVariable]),
              method = "max"
              )
          
          quantilesSize <- quantile(
            datasetSize()[, input$scalingVariable],
            probs = seq(0, 1, 0.2)
          )
  
          scaleT <- 1
          par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
      }
      else {
        stop("Please make sure the scaling variable is numeric")}
    }
      
   
      
    if (!is.null(input$dataset) || input$useExampleData == TRUE)
    {
      biplot(
        pca(),
        scaling = as.numeric(input$scaling),
        type = "n",
        main = if(exists("colN") & exists("scaleT")) {paste("The color of the", input$colorVariable, "corresponds to the", input$scalingVariable, "levels")}
              else if(exists("colF") & exists("scaleT")) {paste("The color of the", input$colorVariable, "corresponds to the", input$scalingVariable, "levels")},
        sub = if(exists("scaleT")){"If the size- numeric factor is labelled as X1,
          no column name exists in the size- data set uploaded."}
        
      )
  
      
      if(input$showPoint == TRUE) {points(
        pca(),
        display = "sites",
        scaling = as.numeric(input$scaling),
        pch = 21,
        col = "black",
        bg = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)} else if (exists("colF")){datasetColor()[, input$colorVariable]} else {"red"},
        cex = if(exists("scaleT")){3*scalingRange} else {1}
      )}
      
      if(input$graphType == "Spider"){
      ordispider(
       pca(),
       groups = datasetColor()[, input$colorVariable],
       col = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)} else if (exists("colF")){datasetColor()[, input$colorVariable]} else {"grey"}
      
        )
       }
      
    if(input$labelColor == TRUE) {
      text(
        pca(),
        display = "species",
        scaling = as.numeric(input$scaling),
        col = "black"
      )
    }
      
  

      
    }  #ends biplot condition block
          
    plot(
      dune.env$A1,
      dune.env$A1,
      type = "n",
      bty = "n",
      col.axis = "white",
      col.lab = "white",
      col = "white",
      tck = 0,
      at = 0)
    
    if((exists("colN") || exists("colF")) & input$showPoint == TRUE){
      legend("right",
             legend =  if(exists("colN")) {quantiles} else if (exists("colF")) {levels(datasetColor()[, input$colorVariable])},
             pt.bg = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)}else if (exists("colF")){as.factor(levels(as.factor(datasetColor()[, input$colorVariable])))}else{"grey"},
             pch = 21,
             pt.cex = 1,
             title = input$colorVariable,
             border = "white"
      )}
    
    if (exists("scaleT") & input$showPoint == TRUE){
      legend("left",  
             legend =  quantilesSize,
             pt.cex = 3*decostand(quantilesSize, "max"),
             pt.bg = "grey",
             pch = 21,
             title = input$scalingVariable,
             border = "white"
      )}  
    #ends legend function
      
    })  #ends renderPlot
  

  
  
  # Prepare downloads
  
  output$downloadData.plot <- downloadHandler(
    filename <- function() {
      paste('PCA_plot-', Sys.Date(), '.tiff', sep='')
    },
    
    content <- function(file) {
      
      
      tiff(
        file,
        width = 2000,
        height = 2000,
        units = "px",
        pointsize = 12,
        res = 300
      )
      
      par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
      
      biplot(
        pca(),
        type = input$labels,
        scaling = as.numeric(input$scaling),
        col = c("red", "blue")
      )
      
      legend("topright", 
             legend = names(datasetColor()),
             text.col = c("red", "blue"),
             pt.bg = c("red", "blue"),
             inset = c(-0.1, 0)
      )
      
      dev.off()
    },
    contentType = 'image/png'
  )
  
  
  output$downloadData.objectScores <- downloadHandler(
    filename <- function() {
      paste('Object_scores-', Sys.Date(), '.csv', sep='')
    },
    content <- function(file) {
      write.csv(pca()$CA$u.eig, file)
    },
    contentType = 'text/csv'
  )
  
  output$downloadData.variableScores <- downloadHandler(
    filename <- function() {
      paste('Variable_scores-', Sys.Date(), '.csv', sep='')
    },
      content <- function(file) {
      write.csv(pca()$CA$v.eig, file)
    },
    contentType = 'text/csv'
  )
  
})
#TODO: legend, vector tips, text