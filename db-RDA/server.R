#runApp('./db-RDA', launch.browser = FALSE)

library(shiny)
library(vegan)
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
			as.matrix(mite.env)
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
		output$whichCondVarsUI <- renderUI({
			
			if (is.null(conditioningFile()))
					return()
					
				checkboxGroupInput(
					inputId = "whichCondVars", 
					label = "Select at least one of your conditioning variables to perform a partial analysis:",
					choices = colnames(conditioningFile()),
					selected = NULL
					)
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

# Transform explanatory data if requested...
	transExpData <- reactive({
		
			if(
				(is.null(input$dataset) | 
				is.null(input$explanatoryVars))  & input$useExampleData == FALSE
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


# Transform conditioning data if requested...
	transCondData <- reactive({
		
		if(is.null(input$conditioningVars) & input$useExampleData == FALSE)
			return()
			
		if(
			!is.numeric(as.matrix(conditioningFile()[, input$whichCondVars])) &
 			input$condTransform != 'none'
		)
			stop("Non-numeric values detected! Transformation invalid.")
		# The controls above work in general, but fail if there is only one
		# conditioning variable. TODO: Figure out why and how to fix.
	
		if (input$condTransform == 'none' | is.null(input$condTransform)){
			conditioningFile()
		} else {
		
			selectedVars <- which(
				colnames(conditioningFile())
 				%in%
				input$whichCondVars
			)
			
			# Store solution to apply colnames
			
			if (input$condTransform == 'wisconsin') {
				temp <- wisconsin(conditioningFile()[ , selectedVars])	
			} else if (input$condTransform == 'square.root') {
				temp <- sqrt(conditioningFile()[ , selectedVars])
 			} else if (
				input$condTransformRorC == 0 |
 				input$condTransform == 'hellinger' |
 				input$condTransform == 'pa'
				) {
				temp <- decostand(
					as.data.frame(
						conditioningFile()[ , selectedVars]
					),
				method = input$condTransform
			)
			} else {
				decostand(
					as.data.frame(
						conditioningFile()[ , selectedVars]
					),
					method = input$condTransform,
					MARGIN = as.numeric(input$condTransformRorC)
				)
			}
		
		
		
			# Attempt to conserve colnames should only 1 var be selected.
			colnames(temp) <- colnames(conditioningFile())[selectedVars]
		
		temp
		
		}
			
	})
	
# Use metaMDSdist if stepacross transformation is to be used, just vegdist
# otherwise

			dissMat <- reactive({
				
				if(is.null(input$dataset) & input$useExampleData == FALSE)
					return()
				
				if (input$autoTransform == TRUE){
					metaMDSdist(
						transData(),
						distance = input$dissim # vegdist is used here
						)
				} else {
					vegdist(
						transData(),
						method = input$dissim,
						binary = ifelse(input$dissim == 'jaccard', TRUE, input$presAbs)
						)
						
				}
			})

# TODO:
# capscale() does not perform constraint aliasing when running in the App, but
# does this when running the R console. This changes the output! Must figure out why...

	dbrda <- reactive({ 
		
		if ((is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE)
					return()
		
		if 	(
			(!is.null(input$conditioningVars) |  input$useExampleData == TRUE) &
			!is.null(input$whichCondVars)
			){	
				
		# capscale() is unable to handle formulae like rda()
		# and uses some sort of subenvironment for its formulae
		# This results in an inability to find reactive objects
		# defined in the shinyServer to subset (as in RDA) and
		# errors like:
		#	Error in eval(expr, envir, enclos) : 
		#	could not find function "transExpData"
		# are returned. To work around this (rather than tackling
		# environments) the following code will bind the explanatory
		# and conditional variables and use the resulting object as
		# the input for capscale()'s data argument. The variables
		# are then pasted into a string by name. The string is 
		# then fed into capscale() as a formula.
			
			condExpData <- cbind(transExpData(), transCondData())
			expVars <- names(transExpData())
			
			f.text <- paste(
							"dissMat() ~ ",
							paste(
									sapply(expVars, FUN = paste0),
									sep = "",
									collapse = " + "
									),
							"+ Condition(",
								paste(
									sapply(input$whichCondVars, FUN = paste0),
									sep = "",
									collapse = " + "
									),
							")"
						)
					
					
				capscale(
					formula = as.formula(f.text),
					data = condExpData,
					comm = transData(),
					add = input$correctionMethod2
				)
		
		} else {
			
		
		capscale(
				dissMat() ~ .,
				data = transExpData(),
				comm = transData(),
				add = input$correctionMethod2
			)

		}
		
	}) # End dbrda definition
 
# Test significance of model
anova <- reactive({
	
	if((is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE)
		return()
	
	if(is.null(strataFile())){
 			anova.cca(
				dbrda()
				)
	} else {
 			anova.cca(
				dbrda(),
				strata = strataFile()
				)
		}
	})


# Prepare output

	output$plot <- renderPlot({
		
		if((is.null(input$dataset) | is.null(input$explanatoryVars))  & input$useExampleData == FALSE)
			return()
		
		if (input$display == "both") {
		ordiplot(
			dbrda(),
 			type = input$labels,
			scaling = as.numeric(input$scaling),
			col = c("red", "blue")
			)
		} else {
		ordiplot(
			dbrda(),
 			type = input$labels,
			scaling = as.numeric(input$scaling),
			col = c("red", "blue"),
			display = input$display
			)	
		}
	})


# Print dbrda summary
	output$print <- renderPrint({
		
		if((is.null(input$dataset) | is.null(input$explanatoryVars))  & input$useExampleData == FALSE)
			print("Please upload data")
		
		print(summary(dbrda()))
	})

# Print results of anova.cca()
	output$printSig <- renderPrint({
		
		if((is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE)
			print("Please upload data")
			
		print(anova())
	})



# Prepare downloads

	output$downloadData.plot <- downloadHandler(
	  filename <- function() {
		paste('dbRDA_plot-', Sys.Date(), '.tiff', sep='')
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
			
		ordiplot(
			dbrda(),
 			type = input$labels,
			scaling = as.numeric(input$scaling),
			col = c("red", "blue"),
			)
			
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

# Download object coordinates
	output$downloadData.objectCoordinates <- downloadHandler(
	  filename <- function() {
		paste('Object_coordinates-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(dbrda()$CA$u, file)
	  },
	  contentType = 'text/csv'
	)

# Download variable coordinates
output$downloadData.variableCoordinates <- downloadHandler(
	  filename <- function() {
		paste('Variable_coordinates-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(dbrda()$CA$v, file)
	  },
	  contentType = 'text/csv'
	)
		
})