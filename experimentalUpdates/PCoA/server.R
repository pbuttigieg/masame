#9G_IXBE5PUN4XSpncTw7oN0l
#runApp('C:\\Users\\pbuttigi\\Documents\\Revolution\\EATME\\PCoA', launch.browser = FALSE)
library(shiny)
library(vegan)
library(RColorBrewer)
data(varechem)

shinyServer(function(input, output){
  
  datasetInput <- reactive({		
    input$dataset
  })
  
  datasetFile <- reactive({
    if (input$useExampleData == TRUE) {
      varechem
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
      varechem
    } else if (input$useExampleDataColor == FALSE) {
      inColorFile <- datasetColorInput()
      
      if (is.null(inColorFile))
        return(NULL)
      
      read.csv(
        file = inColorFile$datapath,
        header = input$header2,
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
      varechem
    } else if (input$useExampleDataSize == FALSE) {
      inSizeFile <- datasetSizeInput()
      
      if (is.null(inSizeFile))
        return(NULL)
      
      read.csv(
        file = inSizeFile$datapath,
        header = input$header3,
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
  
  
  # Use metaMDSdist if stepacross transformation is to be used, just vegdist
  # otherwise
  
  # TODO: Odd behaviour at times, seems not to react to changes in 
  # input$autoTransform occasionally. Could be a machine-specific 
  # issue.
  dissMat <- reactive({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return()
    
    if(
      !is.numeric(as.matrix(datasetFile()))
    )
      stop("Non-numeric values detected! Please upload only numeric data.")
    
    
    if (input$autoTransform == TRUE){
      metaMDSdist(
        datasetFile(),
        distance = input$dissim # vegdist is used here
      )
    } else {
      vegdist(
        datasetFile(),
        method = input$dissim,
        binary = ifelse(input$dissim == 'jaccard', TRUE, input$presAbs)
      )
      
    }
  })
  
  
  pcoa <- reactive({ 
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return()
    
    capscale(
      dissMat() ~ 1,
      comm = datasetFile(),
      add = input$correctionMethod2
      
    )
  })
  
  output$plot <- renderPlot({
    par(mfrow = c(1, 2), mai = c(1, 1, 1, 1))
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return()
    
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
        
        cols <- datasetColor()[, input$colorVariable]
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
    
    plot(pcoa(), type = "n")
    
    if(input$showPoint == TRUE) {points(
      pcoa(),
      pch = 21,
      col = "black",
      bg = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)} else if (exists("colF")){cols} else {"grey"},
      cex = if(exists("scaleT")){3*scalingRange} else {1}
      
    )}
    
    plot(
      varechem$Mo,
      varechem$N,
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
    
    if( input$labels == TRUE){
      ordilabel(pcoa(), fill = "white", col = "darkred", font = 2)
    }
  })
  
  
  output$print <- renderPrint({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return(print("Please upload data"))
    
    print(pcoa())
  })
  
  # Prepare DOWNLOADS
  
  output$downloadData.plot <- downloadHandler(
    filename <- function() {
      paste('PCoA_plot-', Sys.Date(), '.tiff', sep='')
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
      
      plot(pcoa(), type = "n")
      points(pcoa(), pch = 16, col = "red")
      if( input$labels == TRUE){
        ordilabel(pcoa(), fill = "white", col = "darkred", font = 2)
      }
      
      dev.off()
    },
    contentType = 'image/png'
  )
  
  
  output$downloadData.dissMat <- downloadHandler(
    filename <- function() {
      paste('Dissimilarity_matrix-', Sys.Date(), '.csv', sep='')
    },
    content <- function(file) {
      write.csv(as.matrix(dissMat()), file)
    },
    contentType = 'text/csv'
  )
  
  output$downloadData.objectCoordinates <- downloadHandler(
    filename <- function() {
      paste('Object_coordinates-', Sys.Date(), '.csv', sep='')
    },
    content <- function(file) {
      write.csv(pcoa()$CA$u, file)
    },
    contentType = 'text/csv'
  )
  
  output$downloadData.variableCoordinates <- downloadHandler(
    filename <- function() {
      paste('Variable_coordinates-', Sys.Date(), '.csv', sep='')
    },
    content <- function(file) {
      write.csv(pcoa()$CA$v, file)
    },
    contentType = 'text/csv'
  )
  
})

