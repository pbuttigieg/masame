#runApp('./PCoA', launch.browser = FALSE)
library(shiny)
library(vegan)
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
		
		if (is.null(input$dataset) & input$useExampleData == FALSE)
					return()
					
		plot(pcoa(), type = "n")
		points(pcoa(), pch = 16, col = "red")
		if( input$labels == TRUE){
			ordilabel(pcoa(), fill = "white", col = "darkred", font = 2)
			}
	})


	output$print <- renderPrint({
		
		if (is.null(input$dataset) & input$useExampleData == FALSE)
					return(print("Please upload data"))
					
		print(pcoa())
	})

# Prepare downloads

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