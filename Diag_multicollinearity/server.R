#runApp('./Diag_multicollinearity', launch.browser = FALSE)

library(shiny)
library(vegan)
library(Hmisc)
data(varechem)

shinyServer(
	function(input, output){
		
# Handle uploaded response data...
	datasetInput <- reactive({		
		input$dataset
	})

	datasetFile <- reactive({
		if (input$useExampleData == TRUE) {
			as.matrix(varechem)
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


		
# Transform data if requested...

		transData <- reactive({
		
			if(is.null(input$dataset) & input$useExampleData == FALSE)
				return()
				
			if(
				!is.numeric(as.matrix(datasetFile()))
			)
				stop("Non-numeric values detected! Calculations aborted.")
		
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

		
		# Generate a correlation/P/n matrix list
		corrMat <- reactive({ 
				
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
						
			if (is.null(input$corrType) | input$corrType == "pearson"){
				
				rcorr(
					as.matrix(transData()),
					type = c("pearson")
				) # requires library Hmisc
			
			} else if (input$corrType == "spearman") {
			
				rcorr(
					as.matrix(transData()),
					type = c("spearman")
				) # requires library Hmisc
			}
		
		})

		# Find out coords of vars that are correlated above corrThreshold
 		# with p-values below pThreshold 
		
		corrCoords <- reactive ({
			which(
				corrMat()$r > input$corrThreshold &
				corrMat()$P < input$pThreshold,
				arr.ind = TRUE
			)
		})

	
		
		# Define UI element for box plots
		output$boxPlotRangeUI <- renderUI({
		
		if(is.null(input$dataset) & input$useExampleData == FALSE)
				return()	
				
			sliderInput(
				inputId = 'boxPlotRange',
 				label = "Which columns would you like to plot?",
 				min = 1,
 				max = dim(transData())[2],
 				value = c(1,3),
 				step = 1,
				round = FALSE,
 				format = "#,##0.#####",
				ticks = TRUE,
 				animate = FALSE
			)
		})
		
	# Define UI element for pair plots
	output$scatterPlotRangeUI <- renderUI({
		
		if(is.null(input$dataset) & input$useExampleData == FALSE)
				return()
				
		
		# Create list of correlated vars to choose from
		pairList <- colnames(
			transData()[,c(sort(unique(corrCoords()[,1])))]
			)
			
			checkboxGroupInput(
				inputId = "scatterPlotRange",
 				label = "Which correlated variables would you like to plot? Select at least two:",
				choices = c(pairList),
				selected = NULL					
			)
	})
		
# Generate output
	
	# Create list of variables with correlations above thresholds
	output$corrVars <- renderPrint ({ 
			
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return(print("Please upload data"))
			
			rowColNames <- rownames(corrMat()$r) # same as colnames
			
			df <- as.data.frame(
				sort(
					paste(
						rowColNames[corrCoords()[,1]],
 						rowColNames[corrCoords()[,2]]
					)
				)
			)
		
			colnames(df) <- c("Correlated pairs")
			df			
		
		})

		# State number of correlations higher than threshold
		output$numCorrVars <- renderPrint ({ 
			
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return(print("Please upload data"))
			
			paste(
				dim(corrCoords())[1]/2,
				"pairs of variables out of a set of",
				dim(transData())[2],
				"variables were found to have a correlation greater than",
				input$corrThreshold,
				"and a p-value less than",
				input$pThreshold,
				sep = " "
			)
		})
		
		# Pair plot variables identified in corrVars
		output$scatterPlots <- renderPlot({
				
				if(
					(is.null(input$dataset) & input$useExampleData == FALSE) | 
					dim(as.data.frame(transData()[ ,input$scatterPlotRange]))[2] < 2
				)
						return()
					
				
				# Define panel.cor function after in ?pairs
				# some code modified to set a minimum size of text output
				panel.cor <- function(
					x,
 					y,
 					digits = 2,
 					prefix = "",
 					cex.cor,
 					...
					){
						usr <- par("usr"); on.exit(par(usr))
						par(usr = c(0, 1, 0, 1))
						r <- abs(cor(x, y))
						txt <- format(c(r, 0.123456789), digits = digits)[1]
						txt <- paste(prefix, txt, sep = "")
						if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
						text(0.5, 0.5, txt, cex = max(c(2, cex.cor * r)))
					}

				# Adds a pair plot with correlations in the upper panel.
				pairs(
					transData()[, input$scatterPlotRange],
					upper.panel = panel.cor,
					pch = 16,
					col = "blue"
				)
			}) # End scatter plot output
		
		
		# Create boxplots
		output$boxPlots <- renderPlot({
		
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
			
			if (is.null(input$boxPlotRange)) {
				boxplot(
					as.matrix(transData()[,1:3]),
 					use.cols = TRUE,
					las = 3 # Lables perpendicular to axis
				)
			} else {
				boxplot(
					as.matrix(transData()[, input$boxPlotRange[1]:input$boxPlotRange[2]]),
 					use.cols = TRUE,
					las = 3 # Lables perpendicular to axis
				)
			}
		})

# Create a heatmap? Cool, but may cause issues with larger data sets. Defer to
# user community.

# Prepare downloads? If users want it, but not really sensible for a 
# screening tool. Perhaps just option to download transformed data set
# and correlation matrix.

# Download transformed data
	output$downloadData.transformedData <- downloadHandler(
	  filename <- function() {
		paste('Transformed_data_set-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(transData(), file)
	  },
	  contentType = 'text/csv'
	)

# Download correlation matrix
	output$downloadData.corrMatR <- downloadHandler(
	  filename <- function() {
		paste('Correlation_coefficient_matrix-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(corrMat()$r, file)
	  },
	  contentType = 'text/csv'
	)

# Download p-values associated with correlation matrix
	output$downloadData.corrMatP <- downloadHandler(
	  filename <- function() {
		paste('Correlation_p-value_matrix-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(corrMat()$P, file)
	  },
	  contentType = 'text/csv'
	)


	
}
)