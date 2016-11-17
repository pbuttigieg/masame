#runApp('./PCNM', launch.browser = FALSE)

library(shiny)
library(vegan)
data(mite.xy)

shinyServer(function(input, output){
	
	distanceInput <- reactive({	
		
		if (is.null(input$distance))
				return(NULL)	
		
		input$distance
	})

	distanceFile <- reactive({
		if (input$useExampleData == TRUE) {
			mite.xy
		} else if (input$useExampleData == FALSE) {
		inFile <- distanceInput()
	
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

	weightsInput <- reactive({		
			
			if (is.null(input$weights))
				return(NULL)
			
			input$weights
		})

		weightFile <- reactive({
			inFile <- weightsInput()
		
			if (is.null(inFile))
					return(NULL)
					
			as.numeric(as.matrix( # formatting voodoo to get csv working in pcnm()
				read.csv(
					file = inFile$datapath,
					header = input$header,
					sep = input$sep,
					quote = input$quote,
					row.names = if(input$rownames == 0){NULL} else{input$rownames}
				)
			))	
	})

	## NOT IMPLEMENTED, needed for ccaSol, below.
	## Will consider this for a future enhancement.
		#datasetInput <- reactive({		
			#input$dataset
		#})
	#
		#datasetFile <- reactive({
			#inFile <- datasetInput()
		#
			#if (is.null(inFile))
					#return(NULL)
					#
			#read.csv(
				#file = inFile$datapath,
				#header = input$header,
				#sep = input$sep,
				#quote = input$quote
				#)	
	#})


	# Create UIs that require input processing
	# Create UI for thresholdValue, limiting to the max distance in the distance matrix
	
	# Calculate Euclidean distance matrix for pcnm() and UIs
	distanceMatrix <- reactive({
		
		if (is.null(distanceFile()) & input$useExampleData == FALSE)
				return()
				
		dist(distanceFile())
		
		})
	
	
		output$pcnmThresholdValueUI  <- renderUI({
			
			if((is.null(input$distance) | input$threshold == FALSE) &  input$useExampleData == FALSE)
				return()				
			
			numericInput(
				inputId = 'thresholdValue',
				value = 0,
				min = 0,
				max = max(distanceMatrix()),
				label = 'Enter your custom threshold value. A value of "0" triggers the default setting. Note, if your truncation distance is less than the distance required to ensure all objects are connected, a group of eigenfunctions will be created for each group of interconnected points. Large thresholds will "smear out" fine scale variation.'
			)
		})
	
	# Create UI for surface plot
	output$pcnmSurfChoiUI  <- renderUI({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
						
		numericInput(
			inputId = 'pcnmSurfChoi',
			label = 'Which PCNM axis would you like to visualise as a surface?',
			value = 1,
			min = 1,
			step = 1,
			max = dim(pcnmSol()$vectors)[2]
		)
	})

	# Create selection UI for PCNM visualisation...
	output$pcnmPlotChoiUI1  <- renderUI({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
		
		numericInput(
			inputId = 'pcnmPlot1',
			label = "Select the first PCNM you'd like to plot",
			value = 1,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})

	output$pcnmPlotChoiUI2  <- renderUI({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
				
		numericInput(
			inputId = 'pcnmPlot2',
			label = "Select the second PCNM you'd like to plot",
			value = 2,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})

	output$pcnmPlotChoiUI3  <- renderUI({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
				
		numericInput(
			inputId = 'pcnmPlot3',
			label = "Select the third PCNM you'd like to plot",
			value = 3,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})

	output$pcnmPlotChoiUI4  <- renderUI({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
				
		numericInput(
			inputId = 'pcnmPlot4',
			label = "Select the fourth PCNM you'd like to plot",
			value = 4,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})

	output$pcnmPlotChoiUI5  <- renderUI({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
				
		numericInput(
			inputId = 'pcnmPlot5',
			label = "Select the fifth PCNM you'd like to plot",
			value = 5,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})

	output$pcnmPlotChoiUI6  <- renderUI({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
				
		numericInput(
			inputId = 'pcnmPlot6',
			label = "Select the sixth PCNM you'd like to plot",
			value = 6,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})


	# Create selection UI for vsPlot axes...
	output$pcnmVsPlotChoiUI1  <- renderUI({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
				
		numericInput(
			inputId = 'vsPlot1',
			label = "Select the first axis you'd like to plot",
			value = 1,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})

	output$pcnmVsPlotChoiUI2  <- renderUI({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
				
		numericInput(
			inputId = 'vsPlot2',
			label = "Select the second axis you'd like to plot",
			value = 2,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})

	output$pcnmVsPlotChoiUI3  <- renderUI({		
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
				
		numericInput(
			inputId = 'vsPlot3',
			label = "Select the third axis you'd like to plot",
			value = 3,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})

	output$pcnmVsPlotChoiUI4  <- renderUI({		
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()	
				
		numericInput(
			inputId = 'vsPlot4',
			label = "Select the fourth axis you'd like to plot",
			value = 4,
			min = 1,
			max = dim(pcnmSol()$vectors)[2],
			step = 1
		)
	})

	
	# Perform PCNM and create solution object
	pcnmSol <- reactive({ 
		
		if(is.null(distanceMatrix()))
		return(NULL)
		
		
		if (
			is.null(weightFile()) & 
			(is.null(input$threshold) | is.null(input$thresholdValue))
			) {
			
			#print("unweighted, default threshold")
			pcnm(
				distanceMatrix(),				
				dist.ret = TRUE								
			)
		} else if (
			!is.null(weightFile()) & 
			(is.null(input$threshold) | is.null(input$thresholdValue))
			) {
			
			#print("weighted, default threshold")
			pcnm(
				distanceMatrix(),
				w = weightFile(),				
				dist.ret = TRUE								
			)
		} else	if (
			is.null(weightFile()) & 
			(is.null(input$threshold) | is.null(input$thresholdValue) | input$threshold == FALSE | input$thresholdValue == 0)
			) {
			
			#print("unweighted, default threshold")
			pcnm(
				distanceMatrix(),				
				dist.ret = TRUE								
				)
		
		} else if (
			!is.null(weightFile()) &
			(input$threshold == FALSE | input$thresholdValue == 0)
			
			) {
			
			# print("weighted, default threshold")
			pcnm(
				distanceMatrix(),	 
				w = weightFile(),						
				dist.ret = TRUE								
				)
			
		} else if (
			is.null(weightFile()) & 
			(input$threshold == TRUE & input$thresholdValue != 0)	
			)  {
			
			#print("unweighted, custom threshold")
			pcnm(
				distanceMatrix(),	 
				threshold = input$thresholdValue,						
				dist.ret = TRUE								
				)
		
		} else if (
			!is.null(weightFile()) &
			(input$threshold == TRUE & input$thresholdValue != 0)	
			)  {
			#print("weighted, custom threshold")
			pcnm(
				distanceMatrix(),
	 			w = weightFile(),	
				threshold = input$thresholdValue,						
				dist.ret = TRUE								
				)
		}
	})

## Weighted PCNM with CCA ## NOT IMPLEMENTED. APPROACH FROM VEGAN, WILL
						  ## CONSIDER THIS FOR A FUTURE ENHANCEMENT
#
	#standardisedRowSums <- reactive({
		#rowSums(datasetFile())/sum(datasetFile())
	#})
	#
	#weightedPcnmSol <- reactive({
		#pcnm(
			#dist(distanceFile()), 
			#w = stadardisedRowSums
			#)	
	#})
	#
		#
	#ccaSol <- reactive({
		#cca(
			#datasetFile() ~ scores(weightedPcnmSol)
			#)
	#})

# Prepare output...

# Create PCNM multiplot
	output$pcnmPlot <- renderPlot({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()
			
		par(mfrow = c(3,2))
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot1)){input$pcnmPlot1} else (1)],
 			type = "b",
			main = if (!is.null(input$pcnmPlot1)){paste("PCNM", input$pcnmPlot1, sep = " ")} else ("PCNM 1"),
			ylab = "",
			xlab = ""
		)
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot2)){input$pcnmPlot2} else (2)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot2)){paste("PCNM", input$pcnmPlot2, sep = " ")} else ("PCNM 2"),
			ylab = "",
			xlab = ""
		)
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot3)){input$pcnmPlot3} else (3)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot3)){paste("PCNM", input$pcnmPlot3, sep = " ")} else ("PCNM 3"),
			ylab = "",
			xlab = ""
		)
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot4)){input$pcnmPlot4} else (4)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot4)){paste("PCNM", input$pcnmPlot4, sep = " ")} else ("PCNM 4"),
			ylab = "",
			xlab = ""
		)
		plot(pcnmSol()$vectors[, if (!is.null(input$pcnmPlot5)){input$pcnmPlot5} else (5)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot5)){paste("PCNM", input$pcnmPlot5, sep = " ")} else ("PCNM 5"),
			ylab = "",
			xlab = ""
		)
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot6)){input$pcnmPlot6} else (6)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot6)){paste("PCNM", input$pcnmPlot6, sep = " ")} else ("PCNM 6"),
			ylab = "",
			xlab = ""
		)
	}) # End PCNM multiplot creation

# Create surface plot

	output$pcnmSurf <- renderPlot({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()
				
		if (!is.null(input$pcnmSurfChoi)){
			ordisurf(
				distanceFile(),
				scores(pcnmSol(), choi = as.numeric(input$pcnmSurfChoi)),
				bubble = 4,
				main = paste("Surface plot of PCNM", input$pcnmSurfChoi)
			)
		} else {
			ordisurf(
				distanceFile(),
				scores(pcnmSol(), choi = 1),
				bubble = 4,
				main = "Surface plot of PCNM 1"
			)
		}
		
	})


# Create PCNM vs PCNM plot
	output$vsPlot <- renderPlot({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return()
			
		x <- cbind(
			pcnmSol()$vector[, if (!is.null(input$vsPlot1)){input$vsPlot1} else (1)],
 			pcnmSol()$vector[, if (!is.null(input$vsPlot2)){input$vsPlot2} else (2)],
			pcnmSol()$vector[, if (!is.null(input$vsPlot3)){input$vsPlot3} else (3)], 
			pcnmSol()$vector[, if (!is.null(input$vsPlot4)){input$vsPlot4} else (4)]
			)
		
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
			x,
			labels = c(
				if (!is.null(input$vsPlot1)){paste("PCNM",input$vsPlot1)} else (paste("PCNM 1")), 
				if (!is.null(input$vsPlot2)){paste("PCNM",input$vsPlot2)} else (paste("PCNM 2")),  
				if (!is.null(input$vsPlot3)){paste("PCNM",input$vsPlot3)} else (paste("PCNM 3")), 
				if (!is.null(input$vsPlot4)){paste("PCNM",input$vsPlot4)} else (paste("PCNM 4")) 
			),
			upper.panel = panel.cor,
			pch = 16,
			col = "blue"
		)
	}) # End vsPlot output


# Printed output

	output$vectorsPrint <- renderPrint({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return("Please upload data")
			
		print(pcnmSol()$vectors)
	})

	output$valuesPrint <- renderPrint({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return("Please upload data")
			
		print(pcnmSol()$values)
	})

	output$truncDist <- renderPrint({
		
		if(is.null(input$distance) &  input$useExampleData == FALSE)
				return("Please upload data")
			
		print(pcnmSol()$dist)
	})



# Prepare downloads

output$downloadData.multiPlot <- downloadHandler(
	  filename <- function() {
		paste('PCNM_multipanel_plot-', Sys.Date(), '.tiff', sep='')
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
		par(mfrow = c(3,2))
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot1)){input$pcnmPlot1} else (1)],
 			type = "b",
			main = if (!is.null(input$pcnmPlot1)){paste("PCNM", input$pcnmPlot1, sep = " ")} else ("PCNM 1"),
			ylab = "",
			xlab = ""
		)
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot2)){input$pcnmPlot2} else (2)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot2)){paste("PCNM", input$pcnmPlot2, sep = " ")} else ("PCNM 2"),
			ylab = "",
			xlab = ""
		)
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot3)){input$pcnmPlot3} else (3)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot3)){paste("PCNM", input$pcnmPlot3, sep = " ")} else ("PCNM 3"),
			ylab = "",
			xlab = ""
		)
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot4)){input$pcnmPlot4} else (4)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot4)){paste("PCNM", input$pcnmPlot4, sep = " ")} else ("PCNM 4"),
			ylab = "",
			xlab = ""
		)
		plot(pcnmSol()$vectors[, if (!is.null(input$pcnmPlot5)){input$pcnmPlot5} else (5)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot5)){paste("PCNM", input$pcnmPlot5, sep = " ")} else ("PCNM 5"),
			ylab = "",
			xlab = ""
		)
		plot(
			pcnmSol()$vectors[, if (!is.null(input$pcnmPlot6)){input$pcnmPlot6} else (6)], 
			type = "b",
			main = if (!is.null(input$pcnmPlot6)){paste("PCNM", input$pcnmPlot6, sep = " ")} else ("PCNM 6"),
			ylab = "",
			xlab = ""
		)
	dev.off()
	  },
	  contentType = 'image/png'
	)

	output$downloadData.surfPlot <- downloadHandler(
	  filename <- function() {
		paste('PCNM_surface_plot-', Sys.Date(), '.tiff', sep='')
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
			
		if (!is.null(input$pcnmSurfChoi)){
				ordisurf(
					distanceFile(),
					scores(pcnmSol(), choi = as.numeric(input$pcnmSurfChoi)),
					bubble = 4,
					main = paste("Surface plot of PCNM", input$pcnmSurfChoi)
				)
			} else {
				ordisurf(
					distanceFile(),
					scores(pcnmSol(), choi = 1),
					bubble = 4,
					main = "Surface plot of PCNM 1"
				)
			}
			
		dev.off()
	  },
	  contentType = 'image/png'
	)

	# Download PCNM vs PCNM plot
	output$downloadData.vsPlot <- downloadHandler(
	  filename <- function() {
		paste('PCNMvsPCNM_plot-', Sys.Date(), '.tiff', sep='')
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
		
			x <- cbind(
				pcnmSol()$vector[, if (!is.null(input$vsPlot1)){input$vsPlot1} else (1)],
 				pcnmSol()$vector[, if (!is.null(input$vsPlot2)){input$vsPlot2} else (2)],
				pcnmSol()$vector[, if (!is.null(input$vsPlot3)){input$vsPlot3} else (3)], 
				pcnmSol()$vector[, if (!is.null(input$vsPlot4)){input$vsPlot4} else (4)]
				)
			
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
				x,
				labels = c(
					if (!is.null(input$vsPlot1)){paste("PCNM",input$vsPlot1)} else (paste("PCNM 1")), 
					if (!is.null(input$vsPlot2)){paste("PCNM",input$vsPlot2)} else (paste("PCNM 2")),  
					if (!is.null(input$vsPlot3)){paste("PCNM",input$vsPlot3)} else (paste("PCNM 3")), 
					if (!is.null(input$vsPlot4)){paste("PCNM",input$vsPlot4)} else (paste("PCNM 4")) 
				),
				upper.panel = panel.cor,
				pch = 16,
				col = "blue"
			)
		
		dev.off()
	  },
	  contentType = 'image/png'
	)

	# Download PCNM axes
	output$downloadData.vectors <- downloadHandler(
	  filename <- function() {
		paste('PCNM_eigenvectors-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(pcnmSol()$vectors, file)
	  },
	  contentType = 'text/csv'
	)

	# Download eigenvalues
	output$downloadData.values <- downloadHandler(
	  filename <- function() {
		paste('PCNM_eigenvalues-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(pcnmSol()$values, file)
	  },
	  contentType = 'text/csv'
	)

	# Download truncated distance matrix
	output$downloadData.distanceMatrix <- downloadHandler(
	  filename <- function() {
		paste('PCNM_truncated_distance_matrix-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		write.csv(as.matrix(pcnmSol()$dist), file)
	  },
	  contentType = 'text/csv'
	)
	
})