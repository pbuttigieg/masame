#runApp('./ANOSIM', launch.browser = FALSE)

library(shiny)
library(vegan)
data(mite)
data(mite.env)
miteGroups <- mite.env$Shrub

shinyServer(function (input, output){
	
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

# Handle uploaded grouping data...
	groupInput <- reactive({		
		input$groups
	})

	groupFile <- reactive({
		if (input$useExampleData == TRUE) {
			as.vector(miteGroups)
		} else if (input$useExampleData == FALSE) {
		
		grFile <- groupInput()
	
		if (is.null(grFile))
				return(NULL)
				
		read.csv(
			file = grFile$datapath,
			header = input$header,
			sep = input$sep,
			quote = input$quote,
			row.names = if(input$rownames == 0){NULL} else{input$rownames}
			)	
		}
	})


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

# Transform data if requested...

		transData <- reactive({
		
			if(is.null(input$dataset) & input$useExampleData == FALSE)
				return()
				
			if(
				!is.numeric(as.matrix(datasetFile())) &
 				input$transform != 'none'
			)
				stop("Non-numeric values detected! Transformation invalid.")
		
			if (input$transform == 'none' | is.null(input$transform)){
				datasetFile()
			} else if (input$transform == 'wisconsin') {
				wisconsin(datasetFile())	
			} else if (input$transform == 'square.root') {
				sqrt(datasetFile())
 			} else if (
				input$transformRorC == 0 |
 				input$transform == 'hellinger' |
 				input$transform == 'pa'
				) {
				decostand(
					datasetFile(),
					method = input$transform,
				)
			} else {
				decostand(
					datasetFile(),
					method = input$transform,
					MARGIN = as.numeric(input$transformRorC)
				)
			}
			
		})

	dissMat <- reactive({
		
		if (is.null(input$dataset) & input$useExampleData == FALSE)
			return()
		
		vegdist(
				transData(),
				method = input$dissim,
				binary = ifelse(
					input$dissim == 'jaccard' |
 					input$dissim == 'raup' |
 					input$dissim == 'mountford',
 					TRUE, 
 					input$presAbs)
		)
	})

# Calculate ANOSIM solution
	anosimSol <- reactive({ 
		
		if (is.null(input$dataset) & input$useExampleData == FALSE)
			return()
		
		if (is.null(input$strata)) {
			
			anosim(
				dat = dissMat(),
				grouping = unlist(groupFile()),
				permutations = input$numPermute
			)
		
		} else {
			anosim(
				dat = dissMat(),
				grouping = unlist(groupFile()),
				permutations = input$numPermute,
				strata = strataFile()
			)
		}
			
	})




# Prepare output

	output$plot <- renderPlot({
		
		if (
			(is.null(input$dataset) |
			is.null(input$groups)) & input$useExampleData == FALSE
		)
			return()
		
		plot(
			anosimSol(),
			ylab = "Dissimilarity ranks between and within classes"
			)	
	})


	output$print <- renderPrint({
		
		if (
			(is.null(input$dataset) |
			is.null(input$groups)) & input$useExampleData == FALSE
		)
			return(print("Please upload data"))
			
		print(summary(anosimSol()))
		})

# Prepare downloads

	output$downloadData.plot <- downloadHandler(
	  filename <- function() {
		paste('ANOSIM_plot-', Sys.Date(), '.tiff', sep='')
	  },
	  content <- function(file) {
		tiff(
			file,
			width = 4000,
			height = 2000,
			units = "px",
			pointsize = 12,
			res = 300
			)
			
		plot(anosimSol(), ylab = "Dissimilarity ranks between and within classes")
			
		dev.off()
	  },
	  contentType = 'image/png'
	)



# Download dissimilarity matrix
	output$downloadData.dissMat <- downloadHandler(
	  filename <- function() {
		paste('Dissimilarity_matrix-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(as.matrix(dissMat()), file)
	  },
	  contentType = 'text/csv'
	)

# Download transformed data
	output$downloadData.transData <- downloadHandler(
	  filename <- function() {
		paste('Transformed_data_set-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(transData(), file)
	  },
	  contentType = 'text/csv'
	)
		
	}) # End shiny server