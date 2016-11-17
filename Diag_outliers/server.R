#runApp('./Diag_outliers', launch.browser = FALSE)

library(shiny)
library(vegan)
library(mvoutlier)
# library(outliers)
data(varechem)

shinyServer(
	function(input, output){
		
# Handle uploaded response data...
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


		
# Transform data if requested...

		transData <- reactive({
		
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
		
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

		# Create boxplot object (without plot) for outlier extraction, plot 
		# objects created below.
		
		boxPlot <- reactive({ 
				
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
				
				boxPlotTemp <- NULL
				boxPlotTemp <- try(
					boxplot(
						transData(),
						range = input$rangeThreshold,
						plot = FALSE
					),
				silent = TRUE
				)		
				
				if (class(boxPlotTemp) != "list"){
					c("FAIL!")
				} else {
				boxPlotTemp
				}
				
		})
		
		
		# Create a matrix indicating which values have been labelled as
		# univariate outliers in which variables
		uniOutliers <- reactive({
			
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
				
			tempMat <- rbind(boxPlot()$names[boxPlot()$group], boxPlot()$out)
			rownames(tempMat) <- c("Variable","Outlier value(s)")
			tempMat <- as.data.frame(t(tempMat))
			
			
		})


		# Calculate pcout() solution, requires mvoutlier
		pcoutSol <- reactive({
			
			tempSol <- NULL
			tempSol <- try(
				pcout(
					transData(),
 					makeplot = FALSE,
 					# TODO: future enhancement
					# These args are set to the defaults, but
					# are good candidates for adding interactivity
					explvar = 0.99,
 					crit.M1 = 1/3,
 					crit.c1 = 2.5,
 					crit.M2 = 1/4,
 					crit.c2 = 0.99,
 					cs = 0.25,
 					outbound = 0.25
				),
			silent = TRUE
			)
		
			if (class(tempSol) != "list"){
					c("FAIL!")
				} else {
				tempSol
				}
		
		})

		# Names of objects which are multivariate outliers according
 		# to the pcout() method
		
		outlierIdsPcout <- reactive({
  			try(
				names(which(pcoutSol()$wfinal01 == 0)),
				silent = TRUE
			)
		})
		
		
		# Calculate sign1() solution, requires mvoutlier
		sign1Sol <- reactive({
			
			tempSol <- NULL
			tempSol <- try(
				sign1(
					transData(),
 					makeplot = FALSE,
 					qcrit = 0.975 # This parameter can be made interactive
				),
			silent = TRUE
			)
		
			if (class(tempSol) != "list"){
					c("FAIL!")
				} else {
				tempSol
				}
		
		})
		
		
		# Names of objects which are multivariate outliers according
 		# to the sign1() method
		
		outlierIdsSign1 <- reactive({
  			try(
				names(which(sign1Sol()$wfinal01 == 0)),
				silent = TRUE
			)
		})
	
		# Calculate sign2() solution, requires mvoutlier
		sign2Sol <- reactive({
			
			tempSol <- NULL
			tempSol <- try(
				sign2(
					transData(),
 					makeplot = FALSE,
					explvar = 0.99, # This parameter can be made interactive
 					qcrit = 0.975 # This parameter can be made interactive
				),
			silent = TRUE
			)
		
			if (class(tempSol) != "list"){
					c("FAIL!")
				} else {
					tempSol
				}
		})
	
	
		# Names of objects which are multivariate outliers according
 		# to the sign2() method
		
			outlierIdsSign2 <- reactive({
				# In contrast to sign1, the value of sign2's $wfinal01 has no names
				# Thus a different approach: the $x.dist component of sign2's output
				# should have the right names...
  			try(
				names(
					sign2Sol()$x.dist[which(sign2Sol()$wfinal01 == 0)] 
				),
			silent = TRUE
			)
		})
		
		# Create boxplots for specific ranges
		
		# Define UI element
		output$boxPlotRangeUI <- renderUI({
		
		if (is.null(input$dataset) & input$useExampleData == FALSE)
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
		
# Histograms of specific ranges
		
		# Define UI element
		output$histPlotRangeUI <- renderUI({
		
		if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()	
				
			sliderInput(
				inputId = 'histPlotRange',
 				label = "Use the slider to navigate through your variables:",
 				min = 1,
 				max = dim(transData())[2] - 3,
 				value = 1,
 				step = 1,
				round = FALSE,
 				ticks = TRUE,
 				animate = FALSE
			)
		})
		
# Generate output

		# Create boxplots
		output$boxPlots <- renderPlot({
		
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
			
			if (is.null(input$boxPlotRange)) {
				boxplot(
					as.matrix(transData()[1:3]),
 					use.cols = TRUE,
					range = input$rangeThreshold,
					las = 3 # Lables perpendicular to axis
				)
			} else {
				boxplot(
					as.matrix(transData()[input$boxPlotRange[1]:input$boxPlotRange[2]]),
 					use.cols = TRUE,
					range = input$rangeThreshold,
					las = 3 # Lables perpendicular to axis
				)
			}
		})


		# Create histograms
		output$histPlots <- renderPlot({
		
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
			
			if (is.null(input$histPlotRange)) {
				
				par(mfrow = c(2,2))
				for (i in 1:min(c(4, dim(transData())[2]))) { 
					hist(
						as.matrix(transData()[i]),
						main = paste("Variable", i),
						xlab = colnames(transData())[i]
					)
			}
        
			} else {
						
				par(mfrow = c(2,2))
				for (i in (input$histPlotRange):(input$histPlotRange + 3)) { 
					hist(
						as.matrix(transData()[i]),
						main = paste("Variable", i),
						xlab = colnames(transData())[i]
					)
				}
			}
		})
		
		
# Printed output

		output$boxPlotResults <- renderPrint({
		
			if (is.null(input$dataset) & input$useExampleData == FALSE)
					return("Please upload data")
				
			cat(			
				
				# results of univariate outlier screen using boxplot()
				paste(
					"A total of",
					length(boxPlot()$out),
					"outliers were detected across the",
 					dim(transData())[2],
					"variables screened at a range threshold of",
 					paste0(input$rangeThreshold, ".") ,
					"\n",
					sep = " "
				)
			)
		})


# results of multivariate outlier screens
		output$pcoutResults <- renderPrint({
			
			if (is.null(input$dataset) & input$useExampleData == FALSE)
					return("Please upload data")
			
			# As tryCatch() returns odd results, some logic to return 
			# custom error if pcout() fails		
			if (class(pcoutSol()) != "list"){
					print("The PCOut method has failed. Your data may not be suitable for this form of screening (e.g. it has too many identical values in one or more variables).")
				} else {					
					
					cat(	
						paste(
							"Under its default settings, the PCOut method detected",
							length(outlierIdsPcout()),
 							"multivariate outliers.\n",
							"These are objects:"
						),
						
						paste(
 							outlierIdsPcout()
						)
					)
				}
		})

output$sign1Results <- renderPrint({
			
			if (is.null(input$dataset) & input$useExampleData == FALSE)
					return("Please upload data")
			
			# As tryCatch() returns odd results, some logic to return 
			# custom error if sign1() fails		
			if (class(sign1Sol()) != "list"){
					print("The simple spatial signs (Sign1) method has failed. Your data may not be suitable for this form of screening (e.g. it has too many identical values in one or more variables).")
				} else {					
					cat(	
						paste(
							"Under its default settings, the simple spatial signs method (sign1) method detected",
							length(outlierIdsSign1()),
 							"multivariate outliers.\nThese are objects:"
							),
 							
						paste(	
							outlierIdsSign1()
						)
					)
				}
		})

output$sign2Results <- renderPrint({
			
			if (is.null(input$dataset) & input$useExampleData == FALSE)
					return("Please upload data")
			
			# As tryCatch() returns odd results, some logic to return 
			# custom error if pcout() fails		
			if (class(sign2Sol()) != "list"){
					print("The sophisticated spatial signs method (Sign2) has failed. Your data may not be suitable for this form of screening (e.g. it has too many identical values in one or more variables).")
				} else {					
					cat(	
						paste(
							"Under its default settings, the sophisticated spatial signs method (sign2) detected",
							length(outlierIdsSign2()),
 							"multivariate outliers.\nThese are objects:"
							),
 							
						paste(	
							outlierIdsSign2()
						)
					)
				}
		})
		
# Prepare downloads

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


# Download univariate outlier IDs
	output$downloadData.uniOutliers <- downloadHandler(
	  filename <- function() {
		paste('Univariate_outlier_values_and_variables-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		 uO <- as.matrix(uniOutliers())
		write.csv(uO, file)
	  },
	  contentType = 'text/csv'
	)	

# Download multivariate outlier IDs
	output$downloadData.multiOutliers <- downloadHandler(
	  filename <- function() {
		paste('Multivariate_outlier_IDs-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		 uO <- c(
				paste("PCOut method:\n"),
				paste(outlierIdsPcout()),
				paste("\n"),
				paste("Simple spatial sign method:\n"),
				paste(outlierIdsSign1()),
				paste("\n"),
				paste("Sophisticated spatial sign method:\n"),
				paste(outlierIdsSign2())
			)
			
		write(uO, file)
	  },
	  contentType = 'text/csv'
	)

})		

# Interesting functions to explore for future enhancement.
#locoutNeighbor	# Diagnostic plot for identifying local outliers with varying size of
				## neighborhood
				#
				#
#locoutSort # Interactive diagnostic plot for identifying local outliers
#
#map.plot # Plot Multivariate Outliers in a Map
#uni.plot # Univariate plots
#mvoutlier.CoDa # Interpreting multivatiate outliers of Compositional Data
#plot.mvoutlierCoDa # Plots for interpreting multivatiate outliers of CoDa
#
#pcout # PCOut Method for Outlier Identification in High Dimensions
#
#sign1	# Sign Method for Outlier Identiflication in High Dimensions - Simple
		## Version
		#
#sign2	# Sign Method for Outlier Identification in High Dimensions - Sophisticated
		## Version	