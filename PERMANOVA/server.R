#runApp('./PERMANOVA', launch.browser = FALSE)

library(shiny)
library(vegan)
data(dune)
data(dune.env)

shinyServer(function (input, output){
	
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

# Handle uploaded grouping data...
	explanatoryInput <- reactive({		
		input$explanatoryVars
	})

	explanatoryFile <- reactive({
		if (input$useExampleData == TRUE) {
			dune.env
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
			
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
				
			if(
				!is.numeric(as.matrix(datasetFile())) &
 				input$transform != 'none'
			) {
				stop("Non-numeric values detected! Transformation invalid.")
				}
		
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
					method = input$transform
				)
			} else {
				decostand(
					datasetFile(),
					method = input$transform,
					MARGIN = as.numeric(input$transformRorC)
				)
			}
			
		})

# Transform explanatory data if requested...
	transExpData <- reactive({
		
			if(
				is.null(input$explanatoryVars) & input$useExampleData == FALSE
			  )
				return()
				
			if(
				!is.numeric(as.matrix(explanatoryFile())) &
 				input$expTransform != 'none'
			)
				stop("Non-numeric values detected! Transformation invalid.")
		
			if (input$expTransform == 'none' | is.null(input$expTransform)){
				explanatoryFile()
			} else if (input$expTransform == 'wisconsin') {
				wisconsin(explanatoryFile())	
			} else if (input$expTransform == 'square.root') {
				sqrt(explanatoryFile())
 			} else if (
				input$expTransformRorC == 0 |
 				input$expTransform == 'hellinger' |
 				input$expTransform == 'pa'
				) {
				decostand(
					explanatoryFile(),
					method = input$expTransform
				)
			} else {
				decostand(
					explanatoryFile(),
					method = input$expTransform,
					MARGIN = as.numeric(input$expTransformRorC)
				)
			}
			
		})


# Calculate dissimilarity matrix
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

# Calculate ADONIS solution
	adonisSol <- reactive({ 
		
		if (
			(is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE
		    )
			return()
		
		if (is.null(input$strata)) {
			
			adonis(
				formula = as.formula(
					paste(
						'dissMat() ~ ',
						ifelse(
							is.null(input$customFormula),
 							".",
 							input$customFormula
 						)
					)
				),
				data = transExpData(),
				permutations = input$numPermute
			)
		
		} else {
			adonis(
					formula = as.formula(
					paste(
						'dissMat() ~ ',
						ifelse(
							is.null(input$customFormula),
 							".",
 							input$customFormula
 						)
					)
				),
				data = transExpData(),
				permutations = input$numPermute,
				strata = strataFile()
			)
		} 
	})

	output$print <- renderPrint({
		
		if (
			(is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE
		   )
			return(print("Please upload data"))
			
		if (sum(grepl("condition", class(adonisSol()))) > 0)
			return(print("There was an error. Please check your input and try again."))
			
		print(adonisSol())
		})



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