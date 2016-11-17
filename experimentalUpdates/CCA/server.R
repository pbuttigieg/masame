#8g5ziit1CB
#runApp('C:\\Users\\pbuttigi\\Documents\\Revolution\\EATME\\CCA', launch.browser = FALSE)

library(shiny)
library(vegan)
library(RColorBrewer)
data(mite)
data(mite.env)

shinyServer(function(input, output){
  
  # Handle uploaded response data...
  datasetInput <- reactive({
    input$dataset
  })
  
  
  datasetFile <- reactive({
    if (input$useExampleData == TRUE) {
      mite
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
      mite.env
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
      mite.env
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
  
  
  # Handle uploaded explanatory data...
  explanatoryInput <- reactive({		
    input$explanatoryVars		
  })
  
  explanatoryFile <- reactive({
    if (input$useExampleData == TRUE) {
      mite.env
    } else if (input$useExampleData == FALSE) {
      
      exFile <- explanatoryInput()
      
      if (is.null(exFile))
        return(NULL)
      
      read.csv(
        file = exFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        row.names = if(input$rownames == 0){NULL} else{input$rownames}
      )	
      
    }
  })
  
  # Handle uploaded conditioning variables...
  conditioningInput <- reactive({		
    input$conditioningVars
  })
  
  conditioningFile <- reactive({
    if (input$useExampleData == TRUE) {
      mite.env
    } else if (input$useExampleData == FALSE) {
      
      conFile <- conditioningInput()
      
      if (is.null(conFile))
        return(NULL)
      
      read.csv(
        file = conFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        row.names = if(input$rownames == 0){NULL} else{input$rownames}
      )	
    }
  })
  
  # Generate UI element to select which conditioning variables should be used...
  #reactive ({
  
  
  output$whichCondVarsUI <- renderUI({
    
    if (is.null(input$conditioningVars) & input$useExampleData == FALSE)
      return()
    
    checkboxGroupInput(
      inputId = "whichCondVars", 
      label = "Select at least one of your conditioning variables:",
      choices = names(conditioningFile()),
      selected = NULL
    )
  })
  #}
  #})
  
  # Handle uploaded strata data...
  strataInput <- reactive({		
    input$strata
  })
  
  strataFile <- reactive({
    strFile <- strataInput()
    
    if (is.null(strFile))
      return(NULL)
    
    read.csv(
      file = strFile$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote,
      row.names = if(input$rownames == 0){NULL} else{input$rownames}
    )	
  })
  
  # Transform response data if requested...
  transData <- reactive({
    
    if(is.null(input$dataset) & input$useExampleData == FALSE)
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
  
  # Transform explanatory data if requested...
  transExpData <- reactive({
    
    if(is.null(input$explanatoryVars) & input$useExampleData == FALSE)
      return()
    
    if(
      !is.numeric(as.matrix(explanatoryFile())) &
      input$expTransform != 'none'
    )
      stop("Non-numeric values detected! Transformation invalid.")
    # A useful future enhancement: allow users to select variables to
    # transform.
    
    if (input$expTransform == 'none'){
      transExpData <- explanatoryFile()
    } else {
      decostand(
        explanatoryFile(),
        method = input$expTransform
      )
    }
    
  })
  
  # Transform conditioning data if requested...
  transCondData <- reactive({
    
    if (is.null(input$conditioningVars) & input$useExampleData == FALSE)
      return()
    
    if(
      !is.numeric(as.matrix(conditioningFile()[, input$whichCondVars])) &
      input$condTransform != 'none'
    )
      stop("Non-numeric values detected! Transformation invalid.")
    # The controls above work in general, but fail if there is only one
    # conditioning variable. TODO: Figure out why and how to fix.
    
    if (input$condTransform == 'none'){
      transCondData <- conditioningFile()
    } else {
      decostand(
        conditioningFile(),
        method = input$condTransform
      )
    }
    
  })
  
  
  
  # TODO: Priority: nice to have
  # Add textInput to ui allowing users to select columns of the explanatoryFile
  
  # Calculate CCA solution...
  ccaSol <- reactive({
    
    if ((is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE)
      return()
    
    if (is.null(transCondData()) | is.null(input$whichCondVars)){
      cca(
        transData() ~ .,
        data = transExpData()
      )
    } else {
      cca(
        as.formula(
          paste(
            "transData() ~ . + Condition(",
            paste(
              'transCondData()[,"', 
              sapply(input$whichCondVars, FUN = paste0),
              '"]',
              sep = "",
              collapse = " + "
            ),
            ")"
          )
        ),
        data = transExpData()
      )
      
    }
    
  })
  
  # Test significance of model
  anova <- reactive({
    
    if ((is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE)
      return()
    
    if(is.null(strataFile())){
      anova.cca(
        ccaSol()
      )
    } else {
      anova.cca(
        ccaSol(),
        strata = strataFile()
      )
    }
  })
  
  
  # Prepare output
  
  # Generate plot...
  output$plot <- renderPlot({
    par(mfrow = c(1, 2), mai = c(1, 1, 1, 1))
    
    if ((is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE)
      return()
    
    #conditions for introducing a coloring and sizing option for CCA results 
    
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
    
    if (input$display == "both") {
      ordiplot(
        ccaSol(),
        type = input$labels,
        scaling = as.numeric(input$scaling)
      )
      
      if(input$showPoint == TRUE) {points(
        ccaSol(),
        scaling = as.numeric(input$scaling),
        pch = 21,
        col = "black",
        bg = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)} else if (exists("colF")){datasetColor()[, input$colorVariable]} else {"grey"},
        cex = if(exists("scaleT")){3*scalingRange} else {1}
      )}
      
      
    } else {
      ordiplot(
        ccaSol(),
        type = input$labels,
        scaling = as.numeric(input$scaling),
        col = c("red", "blue"),
        display = input$display
      )
      if(input$showPoint == TRUE) {points(
        ccaSol(),
        scaling = as.numeric(input$scaling),
        pch = 21,
        col = "black",
        bg = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)} else if (exists("colF")){datasetColor()[, input$colorVariable]} else {"grey"},
        cex = if(exists("scaleT")){3*scalingRange} else {1}
      )}  
    }
    
    plot(
      mite.env$SubsDens,
      mite.env$WatrCont,
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
    
  })
  
  # Generate summary of CCA solution...
  output$print <- renderPrint({
    
    if ((is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE)
      print("Please upload data for analysis")
    
    print(summary(ccaSol()))
  })
  
  
  # Generate output of significance testing
  output$printSig <- renderPrint({
    
    ((is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE)
    print("Please upload data for analysis")
    
    print(anova())
  })
  
  
  # Prepare downloads
  
  output$downloadData.plot <- downloadHandler(
    filename <- function() {
      paste('CCA_plot-', Sys.Date(), '.tiff', sep='')
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
      
      if (input$display == "both") {
        ordiplot(
          ccaSol(),
          type = input$labels,
          scaling = as.numeric(input$scaling),
          col = c("red", "blue")
        )
      } else {
        ordiplot(
          ccaSol(),
          type = input$labels,
          scaling = as.numeric(input$scaling),
          col = c("red", "blue"),
          display = input$display
        )	
      }
      
      dev.off()
    },
    contentType = 'image/png'
  )
  
  # DOWNLOAD object coordinates
  output$downloadData.objectCoordinates <- downloadHandler(
    filename <- function() {
      paste('Object_coordinates-', Sys.Date(), '.csv', sep='')
    },
    content <- function(file) {
      write.csv(ccaSol()$CA$u, file)
    },
    contentType = 'text/csv'
  )
  
  # Download variable coordinates
  output$downloadData.variableCoordinates <- downloadHandler(
    filename <- function() {
      paste('Variable_coordinates-', Sys.Date(), '.csv', sep='')
    },
    content <- function(file) {
      write.csv(ccaSol()$CA$v, file)
    },
    contentType = 'text/csv'
  )
  
})

