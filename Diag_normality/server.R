#runApp('./Diag_normality', launch.browser = FALSE)

library(shiny)
library(vegan)
library(mvnormtest)
data(mite)
# library(car) # needed if Box Cox and related power transformations will be used

# For testing purposes...
# Generate multivariate normal distribution:
#library(mnormt)
#mu <- c(1,12,2) # Define vector of means
#Sigma <- matrix(c(1,2,0,2,5,0.5,0,0.5,3), 3, 3) # Define matrix of standard deviations
#rmnormMat <- rmnorm(10, mu, Sigma)

shinyServer(
	function(input, output){
		
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


		
# Transform data if requested...

		transData <- reactive({
		
			if(is.null(input$dataset) & input$useExampleData == FALSE)
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

		# Future enhancement:
		# Add powerTransform {car} to this. will suggest power needed to 
		# transform a matrix to multivariate normality. 
		# e.g.:
 		# powerTransform(cycles ~ len + amp + load, Wool)
		# the "family" parameter can select Box Cox or Yeo-Johnson methods
		# compare with the max loglikelihood of:
		# boxCox(
			#cycles ~ len + amp + load, 
			#data = Wool,
			#lambda = seq(-0.25, 0.25, length = 10)
		#)


		# Test with multivariate Shapiro-Wilk statistic
		mshapiroTest <- reactive({ 
				
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
				
				mShapTemp <- NULL
				mShapTemp <- try(
					mshapiro.test(as.matrix(transData())), # requires library mvnormtest
					silent = TRUE
					)		
				
				if (class(mShapTemp) != "htest"){
					c("FAIL!")
				} else {
				mShapTemp
				}
				
		})

		# Test each individual parameter
		
		# Little helper function to handle htest objects for sapply()
		extractPval <- function(X) {
			return(X$p.value)
			}
		
		# Perform tests and store their p-values as a vector
		shapiroTests <- reactive({
			shapiroList <- apply(transData(), MARGIN = 2, FUN = shapiro.test)
			sapply(shapiroList, FUN = extractPval)			
		})


		# Output number of passed/failed tests given some input$threshold
		
		numPassedShapTests <- reactive({
  			length(which(shapiroTests() > input$shapThreshold))
		})
		
		
		# Boxplots of specific ranges
		
		# Define UI element
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
		
		# QQ plots of specific ranges
		
		# Define UI element
		output$qqPlotRangeUI <- renderUI({
		
		if(is.null(input$dataset) & input$useExampleData == FALSE)
				return()	
				
			sliderInput(
				inputId = 'qqPlotRange',
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

# Histograms of specific ranges
		
		# Define UI element
		output$histPlotRangeUI <- renderUI({
		
		if(is.null(input$dataset) & input$useExampleData == FALSE)
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
					as.matrix(transData()[,1:3]),
 					use.cols = TRUE,
					las = 3 # Lables perpendicular to axis
				)
			} else {
				boxplot(
					as.matrix(transData()[,input$boxPlotRange[1]:input$boxPlotRange[2]]),
 					use.cols = TRUE,
					las = 3 # Lables perpendicular to axis
				)
			}
		})

		# Create QQ plots
		output$qqPlots <- renderPlot({
		
			if (is.null(input$dataset) & input$useExampleData == FALSE)
				return()
			
			if (is.null(input$qqPlotRange)) {
				
				par(mfrow = c(2,2))
				for (i in 1:min(c(4, dim(transData())[2]))) { 
					qqnorm(
						as.matrix(transData()[i]),
						main = paste("Variable", i)
					)
			}
        
			} else {
						
				par(mfrow = c(2,2))
				for (i in (input$qqPlotRange):(input$qqPlotRange + 3)) { 
					qqnorm(
						as.matrix(transData()[i]),
						main = paste("Variable", i)
					)
				}
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

		output$univarTestResults <- renderPrint({
		
			if(is.null(input$dataset) & input$useExampleData == FALSE)
					return("Please upload data")
				
			cat(
				
				# Blurb
				paste(
					"The null hypothesis is: the population is normally distributed.\n",
					"variables / data sets pass the test when p-values are greater than the threshold specified.\n\n"
					),
				
				
				# results of univariate shapiro tests
				paste(
					numPassedShapTests(),
					"out of",
 					dim(transData())[2],
					"variables passed the Shapiro-Wilk test at a p-value threshold of",
 					input$shapThreshold,
					"\n",
					sep = " "
				)
			)
		})


# results of multivariate shapiro test
		output$multivarTestResults <- renderPrint({
			
			if(is.null(input$dataset) & input$useExampleData == FALSE)
					return("Please upload data")
			
			# As tryCatch() returns odd results, some logic to return 
			# custom error if mshapiro.test fails		
			if (class(mshapiroTest()) != "htest"){
					c("Multivariate Shapiro-Wilk test failed to complete. Your data may be unsuitable for calculation (e.g. lead to a computationally singular state).")
				} else {					
					cat(	
						paste(
							"\nA multivariate Shapiro-Wilk test returned a p-value of:",
							mshapiroTest()$p.value,
 							"Your p-value threshold was:",
 							input$mshapThreshold,
							"\n",
							sep = " "
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

# download 
# Download univariate Shapiro-Wilk test p-values
	output$downloadData.shapiroTestsPvals <- downloadHandler(
	  filename <- function() {
		paste('Univariate_Shapiro-Wilk_test_p-values-', Sys.Date(), '.csv', sep='')
	  },
	  content <- function(file) {
		pValDf <- as.data.frame(shapiroTests())
		names(pValDf) <- "(uncorrected) p-values of univariate Shapiro-Wilk tests"
		write.csv(pValDf, file)
	  },
	  contentType = 'text/csv'
	)
		
		
		# Add alternate normality tests? like: mvnorm.etest {energy}? or
		# adtest {robCompositions}. Durbin-Watson statistic
		
}
)