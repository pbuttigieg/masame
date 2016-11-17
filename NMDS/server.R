#runApp('./NMDS', launch.browser = FALSE)

library(shiny)
library(vegan)
data(dune)

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
				method = input$transform,
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
		
		# Multiplot for 3 dimensions
		if (input$dimNum == 3) {
			par(mfrow = c(2,2))
			
			plot(
				nmds()$points[,1],
 				nmds()$points[,2],
 				type = "n",
				xlab = "NMDS 1",
				ylab = "NMDS 2"
				)
			
			points(
				nmds()$points[,1],
 				nmds()$points[,2],
 				pch = 16,
 				col = "red"
				)
			
			if( input$labels == TRUE){
				ordilabel(nmds(), choices = c(1,2), fill = "white", col = "darkred", font = 2)
			}
			
			plot(
				nmds()$points[,1],
 				nmds()$points[,3],
 				type = "n",
				xlab = "NMDS 1",
				ylab = "NMDS 3"
				)
			
			points(
				nmds()$points[,1],
 				nmds()$points[,3],
 				pch = 16,
 				col = "red"
				)
			
			if( input$labels == TRUE){
				ordilabel(nmds(), choices = c(1,3), fill = "white", col = "darkred", font = 2)
			}
		
			plot(
				nmds()$points[,2],
 				nmds()$points[,3],
 				type = "n",
				xlab = "NMDS 2",
				ylab = "NMDS 3"
				)
			
			points(
				nmds()$points[,2],
 				nmds()$points[,3],
 				pch = 16,
 				col = "red"
				)
			
			if( input$labels == TRUE){
				ordilabel(nmds(), choices = c(2,3), fill = "white", col = "darkred", font = 2)
			}
		
		# 2 Dimensional plot	
		} else if (input$dimNum == 2) {
		
		plot(
			nmds(),
 			type = "n",
			xlab = "NMDS 1",
			ylab = "NMDS 2"
			)
			points(
				nmds(),
 				pch = 16,
 				col = "red"
				)
			
			if( input$labels == TRUE){
				ordilabel(nmds(), fill = "white", col = "darkred", font = 2)
			}
		
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



# Prepare downloads

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
			
		# Multiplot for 3 dimensions
		if (input$dimNum == 3) {
			par(mfrow = c(2,2))
			
			plot(
				nmds()$points[,1],
 				nmds()$points[,2],
 				type = "n",
				xlab = "NMDS 1",
				ylab = "NMDS 2"
			)
			
			points(
				nmds()$points[,1],
 				nmds()$points[,2],
 				pch = 16,
 				col = "red"
			)
			
			if( input$labels == TRUE){
				ordilabel(nmds(), choices = c(1,2), fill = "white", col = "darkred", font = 2)
			}
			
			plot(
				nmds()$points[,1],
 				nmds()$points[,3],
 				type = "n",
				xlab = "NMDS 1",
				ylab = "NMDS 3"
			)
			
			points(
				nmds()$points[,1],
 				nmds()$points[,3],
 				pch = 16,
 				col = "red"
			)
			
			if( input$labels == TRUE){
				ordilabel(nmds(), choices = c(1,3), fill = "white", col = "darkred", font = 2)
			}
		
			plot(
				nmds()$points[,2],
 				nmds()$points[,3],
 				type = "n",
				xlab = "NMDS 2",
				ylab = "NMDS 3"
			)
			
			points(
				nmds()$points[,2],
 				nmds()$points[,3],
 				pch = 16,
 				col = "red"
			)
			
			if( input$labels == TRUE){
				ordilabel(nmds(), choices = c(2,3), fill = "white", col = "darkred", font = 2)
			} # End 3 dimensional plot definition
		
		# 2 Dimensional plot	
		} else if (input$dimNum == 2) {
		
			plot(
				nmds(),
 				type = "n",
				xlab = "NMDS 1",
				ylab = "NMDS 2"
			)
		
			points(
				nmds(),
				pch = 16,
				col = "red"
			)
				
		if( input$labels == TRUE){
			ordilabel(nmds(), fill = "white", col = "darkred", font = 2)
		}
		
		} # End 2 dimensional plot definition 
			
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