#runApp('./CCA', launch.browser = FALSE)

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
				method = input$transform,
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
				method = input$expTransform,
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
		
		if ((is.null(input$dataset) | is.null(input$explanatoryVars)) & input$useExampleData == FALSE)
			return()
		
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

# Download object coordinates
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