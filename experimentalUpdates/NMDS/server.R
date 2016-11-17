#505AdZKpufSmOJG1Hh6l3OT
#runApp('C:\\Users\\pbuttigi\\Documents\\Revolution\\EATME\\NMDS', launch.browser = FALSE)

library(shiny)
library(vegan)
library(RColorBrewer)
data(dune)
data("dune.env")

shinyServer(function(input, output){
  
  # Handle uploaded dataset
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
      dune.env
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
  
  # Transform response data if requested...
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
  dissMat <- reactive({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return()
    
    vegdist(
      transData(),
      method = input$dissim,
      binary = ifelse(input$dissim == 'jaccard', TRUE, input$presAbs)
    )
    
  })
  
  nmds <- reactive({ 
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return()
    
    metaMDS(
      dissMat(),
      k = as.numeric(input$dimNum)
    )
  })
  
  output$plot <- renderPlot({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return()
  
      #conditions for introducing a coloring and sizing option for NMDS results 
    
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
    
    # Multiplot for 3 dimensions
    if (input$dimNum == 3) {
      par(mfrow = c(2,2), mai = c(1, 1, 1, 1))
      
      plot(
        nmds(),
        choices = c(1,2),
        type = "n",
        xlab = "NMDS 1",
        ylab = "NMDS 2"
      )
      
      if(input$showPoint == TRUE) {points(
        nmds(),
        choices = c(1,2),
        pch = 21,
        col = "black",
        bg = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)} else if (exists("colF")){datasetColor()[, input$colorVariable]} else {"grey"},
        cex = if(exists("scaleT")){3*scalingRange} else {1}
      )}
      
      if(input$Spider == TRUE & input$factorType == "Factor"){
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
          ordispider(
            nmds(),
            choices = c(1,2),
            groups = as.factor(datasetColor()[, input$colorVariable]),
            display = "sites",
            show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
            col = i,
            label = F
            
          )
        }
      }
      
      if(input$Hull == TRUE & input$factorType == "Factor"){
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
          ordihull(
            nmds(),
            choices = c(1,2),
            groups = as.factor(datasetColor()[, input$colorVariable]),
            show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
            col = i,
            display = "sites",
            draw = "lines",
            lty = "dotted"
            
            
          )
        }
      }
      
      if(input$Ellipse == TRUE & input$factorType == "Factor"){
        
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
          ordiellipse( 
            nmds(),
            choices = c(1,2),
            groups = as.factor(datasetColor()[, input$colorVariable]),
            show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
            col = i,
            display = "sites",
            draw = "polygon",
            alpha = 127
            
          )
        }
      }
      
      if( input$labels == TRUE){
        ordilabel(nmds(), choices = c(1,2), fill = "white", col = "darkred", font = 2)
      }
      
      plot(
        nmds(),
        choices = c(1,3),
        type = "n",
        xlab = "NMDS 1",
        ylab = "NMDS 3"
      )
      
      if(input$showPoint == TRUE) {points(
        nmds(),
        choices = c(1,3),
        pch = 21,
        col = "black",
        bg = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)} else if (exists("colF")){datasetColor()[, input$colorVariable]} else {"grey"},
        cex = if(exists("scaleT")){3*scalingRange} else {1}
      )}
      
      if(input$Spider == TRUE & input$factorType == "Factor"){
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
          ordispider(
            nmds(),
            choices = c(1,3),
            groups = as.factor(datasetColor()[, input$colorVariable]),
            display = "sites",
            show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
            col = i,
            label = F
            
          )
        }
      }
      
      if(input$Hull == TRUE & input$factorType == "Factor"){
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
          ordihull(
            nmds(),
            choices = c(1,3),
            groups = as.factor(datasetColor()[, input$colorVariable]),
            show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
            col = i,
            display = "sites",
            draw = "lines",
            lty = "dotted"
            
            
          )
        }
      }
      
      if(input$Ellipse == TRUE & input$factorType == "Factor"){
        
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
          ordiellipse( # TODO: use semi-transparent polygon rather than lines
            nmds(),
            choices = c(1,3),
            groups = as.factor(datasetColor()[, input$colorVariable]),
            show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
            col = i,
            display = "sites",
            draw = "polygon",
            alpha = 127
            
          )
        }
      }
      
      if( input$labels == TRUE){
        ordilabel(nmds(), choices = c(1,3), fill = "white", col = "darkred", font = 2)
      }
      
      plot(
        nmds(),
        choices = c(2,3),
        type = "n",
        xlab = "NMDS 2",
        ylab = "NMDS 3"
      )
      
      if(input$showPoint == TRUE) {points(
        nmds(),
        choices = c(2,3),
        pch = 21,
        col = "black",
        bg = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)} else if (exists("colF")){datasetColor()[, input$colorVariable]} else {"grey"},
        cex = if(exists("scaleT")){3*scalingRange} else {1}
      )}
      
      if(input$Spider == TRUE & input$factorType == "Factor"){
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
          ordispider(
            nmds(),
            choices = c(2,3),
            groups = as.factor(datasetColor()[, input$colorVariable]),
            display = "sites",
            show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
            col = i,
            label = F
            
          )
        }
      }
      
      if(input$Hull == TRUE & input$factorType == "Factor"){
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
          ordihull(
            nmds(),
            choices = c(2,3),
            groups = as.factor(datasetColor()[, input$colorVariable]),
            show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
            col = i,
            display = "sites",
            draw = "lines",
            lty = "dotted"
            
            
          )
        }
      }
      
      if(input$Ellipse == TRUE & input$factorType == "Factor"){
        
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
          ordiellipse( # TODO: use semi-transparent polygon rather than lines
            nmds(),
            choices = c(2,3),
            groups = as.factor(datasetColor()[, input$colorVariable]),
            show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
            col = i,
            display = "sites",
            draw = "polygon",
            alpha = 127
            
          )
        }
      }
      
      if( input$labels == TRUE){
        ordilabel(nmds(), choices = c(2,3), fill = "white", col = "darkred", font = 2)
      }
      
      plot(
        nmds(),
        choices = c(2,3),
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
      
      
      # 2 Dimensional plot	
    } else if (input$dimNum == 2) {
      
      
      par(mfrow = c(1, 2), mai = c(1, 1, 1, 1))
      
      mat <- matrix(c(1, 2), 1)
      layout(mat, width = c(1.8, 1))
      
      
      plot(
        nmds(),
        type = "n",
        xlab = "NMDS 1",
        ylab = "NMDS 2"
      )
      
      #TODO: move if showPoint to bg
      if(input$showPoint == TRUE) {points(
        nmds(),
        pch = 21,
        col = "black",
        bg = if (exists("colN")){rgb(0, 0, 1, alpha = transparency)} else if (exists("colF")){datasetColor()[, input$colorVariable]} else {"grey"},
        cex = if(exists("scaleT")){3*scalingRange} else {1}
      )}
      
      if(input$Spider == TRUE & input$factorType == "Factor"){
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
        ordispider(
          nmds(),
          groups = as.factor(datasetColor()[, input$colorVariable]),
          display = "sites",
          show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
          col = i,
          label = F
          
        )
      }
      }
      
      if(input$Hull == TRUE & input$factorType == "Factor"){
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
        ordihull(
          nmds(),
          groups = as.factor(datasetColor()[, input$colorVariable]),
          show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
          col = i,
          display = "sites",
          draw = "lines",
          lty = "dotted"
          
          
        )
        }
      }
      
      if(input$Ellipse == TRUE & input$factorType == "Factor"){
        
        for (i in 1:length(levels(as.factor(datasetColor()[, input$colorVariable])))){
        ordiellipse( # TODO: use semi-transparent polygon rather than lines
          nmds(),
          groups = as.factor(datasetColor()[, input$colorVariable]),
          show.groups = levels(as.factor(datasetColor()[, input$colorVariable]))[i],
          col = i,
          display = "sites",
          draw = "polygon",
          alpha = 127
          
        )
        }
      }
      
      
      if( input$labels == TRUE){
        ordilabel(nmds(), fill = "white", col = "darkred", font = 2)
      }
            
      
      plot(
        nmds(),
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
      
    } 
  }) # End NMDS plot function
  
  output$stressplot <- renderPlot({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return()
    
    stressplot(nmds())
  })
  
  output$print <- renderPrint({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return("Please upload data")
    
    print(nmds())
  })
  
  output$objectCoordinates <- renderTable({
    
    if (is.null(input$dataset) & input$useExampleData == FALSE)
      return("Please upload data")
    
    as.table(nmds()$points)
  })
  
  
  
  # Prepare DOWNLOADS
  
  output$downloadData.plot <- downloadHandler(
    filename <- function() {
      paste('NMDS_plot-', Sys.Date(), '.tiff', sep='')
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
      
     
      
      dev.off()
    },
    contentType = 'image/png'
  )
  
  output$downloadData.stressplot <- downloadHandler(
    filename <- function() {
      paste('NMDS_stress_plot-', Sys.Date(), '.tiff', sep='')
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
      
      stressplot(nmds())
      
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
      write.csv(nmds()$points, file)
    },
    contentType = 'text/csv'
  )
  
})

