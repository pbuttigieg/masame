#runApp('./ClusterAnalysis', launch.browser = FALSE)

library(shiny)
library(vegan)
data(mite)

shinyServer(function(input, output){
	
	# Handle uploaded dataset
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

	dissMat <- reactive({
		
		if (is.null(input$dataset) & input$useExampleData == FALSE)
			return()
		
		if (
			input$dissim == 'pearson' |
			input$dissim == 'spearman'
		) {
			
			library(Hmisc)
			corrMat <- rcorr(as.matrix(datasetFile()), type = c(input$dissim))
			rMat <- as.dist(corrMat$r) # convert coefs (r) to dist object
			rMat <- rMat + abs(min(rMat)) # no negative values
			rMat <- rMat/max(rMat) # Scale to have a max of 1.
			dMat <- 1 - rMat # Convert to 'dissimilarity'-like values
			dMat # return dist object of correlation-derived values
			
		} else {
			
			vegdist(
				datasetFile(),
				method = input$dissim,
				binary = ifelse(
					input$dissim == 'jaccard' |
 					input$dissim == 'raup' |
 					input$dissim == 'mountford',
 					TRUE, 
 					input$presAbs)
			)
		}
	})



# Calculate cluster analysis solution...
	clustSol <- reactive({ 
				
		if (is.null(input$dataset) & input$useExampleData == FALSE)
			return()
				
		hclust(
			dissMat(),
			method = input$clustMethod
		)
	})

	output$plot <- renderPlot({
					
		if (is.null(input$dataset) & input$useExampleData == FALSE)
			return()
		
		plot(
			clustSol(),
			main =  "",
			ylab = "Dissimilarity",
			xlab = paste(
				"Solution based on",
				switch(
					input$dissim,
					'euclidean' = 'Euclidean',
					'manhattan' = 'Manhattan (city-block)',
					'bray' = 'Bray-Curtis',
					'jaccard' = 'Jaccard', # This will set presAbs to TRUE
					'raup' = 'Raup-Crick',
					'kulczynski' = 'Kulczynski',
					'canberra' = 'Canberra',
					'gower' = 'Gower',
					'altGower' = 'Alternative Gower',
					'morisita' = 'Morisita',
					'horn' = 'Horn-Morisita',
					'mountford' = 'Mountford',
					'binomial' = 'Binomial',
					'cao' = 'Cao (using natural logarithms)',
					'chao' = 'Chao',
					'pearson' = 'Pearson-derived',
					'spearman' = 'Spearman-derived'
				),
				"dissimilarities and the",
				switch(
					input$clustMethod,
					'complete' = 'complete',
					'single' = 'single',
					'average' = 'average',
					'median' = 'median',
					'centroid' = 'centroid',
					"ward" = "Ward's",
					"mcquitty" = "McQuitty's"
					),
				"clustering method.",
				sep = " "
				), # End paste for xlab
				sub = ''
			
		) # End plot
		
	}) # End plot function


	output$print <- renderPrint({
					
		if (is.null(input$dataset) & input$useExampleData == FALSE)
			return("Please upload data")
				
		print(clustSol())
	})


# Prepare downloads

	output$downloadData.plot <- downloadHandler(
	  filename <- function() {
		paste('Cluster_Analysis_plot-', Sys.Date(), '.tiff', sep='')
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
		
		if (is.null(input$dataset))
			return()
		
		plot(
			clustSol(),
			main =  "",
			ylab = "Dissimilarity",
			xlab = paste(
				"Solution based on",
				switch(
					input$dissim,
					'euclidean' = 'Euclidean',
					'manhattan' = 'Manhattan (city-block)',
					'bray' = 'Bray-Curtis',
					'jaccard' = 'Jaccard', # This will set presAbs to TRUE
					'raup' = 'Raup-Crick',
					'kulczynski' = 'Kulczynski',
					'canberra' = 'Canberra',
					'gower' = 'Gower',
					'altGower' = 'Alternative Gower',
					'morisita' = 'Morisita',
					'horn' = 'Horn-Morisita',
					'mountford' = 'Mountford',
					'binomial' = 'Binomial',
					'cao' = 'Cao (using natural logarithms)',
					'chao' = 'Chao',
					'pearson' = 'Pearson-derived',
					'spearman' = 'Spearman-derived'
				),
				"dissimilarities and the",
				switch(
					input$clustMethod,
					'complete' = 'complete',
					'single' = 'single',
					'average' = 'average',
					'median' = 'median',
					'centroid' = 'centroid',
					"ward" = "Ward's",
					"mcquitty" = "McQuitty's"
					),
				"clustering method.",
				sep = " "
				), # End paste for xlab
				sub = ''
			
		) # End plot	
		
			
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
		
	})