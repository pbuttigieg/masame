#runApp('./ClusterAnalysis', launch.browser = FALSE)

library(shiny)

## ui.R


shinyUI(
	pageWithSidebar(
		
		# Header defintion
		headerPanel("Perform a hierarchical cluster analysis..."),
		
		# Sidebar defintion
		sidebarPanel(
		tabsetPanel(
			tabPanel("Data upload",
 				h5("Description"),
				p("This App will perform a hierarchical cluster analysis using the hclust() function from the stats package for R. Transformations are performed by decostand() and wisconsin() {vegan} as well as some standard functions. Dissimilarity is calculated by vegdist() {vegan} and correlations by rcorr() {Hmisc}."),
				h5("CSV parameters"),
				p("Note that these parameters apply to all files uploaded. If your files are not correctly formatted, errors will result."),
				
				
				# TODO: See how this can be done
				#fileInput(
					#'metadata', 
					#'If you would like to use additional data for modifying your plot (e.g. colouring points) upload a single column CSV file here...',
					#accept = c('text/csv','text/comma-separated-values','.csv')
					#),
				
				# Parameters for read.csv...
				h5("Example data"),
				p("Tick the box below if you'd like to use the 'mite' dataset included in the vegan package as an example."),
				checkboxInput('useExampleData', 'Use an example dataset', FALSE),
				p(),
				
				checkboxInput('header', 'Header', TRUE),
				
				numericInput(
					inputId = 'rownames',
					value = 1,
					min = 0,
 					label = 'Which column contains row lables (enter "0" if there is no such column)?'
					),
				
				radioButtons(
					inputId = 'sep',
 					label = 'Separator',
					choices = c(
						Comma = ',',
						Semicolon = ';',
						Tab = '\t'
						)
					),
				
				radioButtons(
					inputId = 'quote',
					label = 'Quote',
					choices = c(
						'Double quotes' = '"',
						'Single quotes' = "'",
						'None' = ''
						)
					),
				
				fileInput(
					inputId = 'dataset', 
					label = 'Select a CSV file to upload for analysis...',
					accept = c('text/csv','text/comma-separated-values','.csv')
					)
				),
			
			
			tabPanel(
				"Clustering parameters...",
				# Parameters for hclust()...
				# Select dissimilarity measure
				h5("Dissimilarity measure"),
				
				selectInput(
					inputId = 'dissim',
					label = 'Select a (dis)similarity or association measure. Note, the Jaccard measure is only valid for presence absence data. Most measures below are Q-mode, which apply to objects. R-mode measures apply to variables.',
					choices = c(
						'Euclidean' = 'euclidean',
						'Manhattan (city-block)' = 'manhattan',
						'Bray-Curtis' = 'bray',
						'Jaccard (presence/absence data)' = 'jaccard', # This will set presAbs to TRUE
						'Raup-Crick (probabilistic, presence/absence data)' = 'raup',
						'Kulczynski' = 'kulczynski',
						'Canberra' = 'canberra',
						'Gower' = 'gower',
						'Alternative Gower' = 'altGower',
						'Morisita (only suitable for integer data)' = 'morisita',
						'Horn-Morisita' = 'horn',
						'Mountford' = 'mountford',
						'Binomial' = 'binomial',
						'Cao using natural logarithms' = 'cao',
						'Chao' = 'chao',						
						'Dissimilarities derived from Pearson correlations (R-mode)*' = 'pearson',
						'Dissimilarities derived from Spearman correlations (R-mode)*' = 'spearman'
						)
				),
				br(),
				HTML("* When using correlations, a constant, equal to the absolute value of the most negative correlation is added to all correlations before they are divided by the maximum correlation value. The resulting values (scaled from 0 to 1) are subtracted from one to deliver a dissimilarity-like value. <b>These results are for exploratory purposes only and interpretation should be restrained.</b>"),
				br(),
				br(),
				# Presence absence or abundance?
				radioButtons(
					inputId = 'presAbs',
					label = 'Do you have abundance (or other count data) or presence/absence data? Only applicable to Q-mode measures.',
					choices = c(
						# Logicals fed into 'binary = ' arg of vegdist()
						'Abundance' = 'FALSE', 
						'Presence / Absence' = 'TRUE'
						)
					),
			
				# Clustering method to use
				h5("Clustering algorithm"),
				radioButtons(
					inputId = 'clustMethod',
					label = 'Which clustering method should be used?',
					choices = c(
						'Complete clustering' = 'complete',
						'Single-linkage clustering' = 'single',
						'Average-linkage clustering' = 'average',
						'Median clustering' = 'median',
						'Centroid clustering' = 'centroid',
						"Ward's method" = 'ward',
						"McQuitty's method" = 'mcquitty'
					)
				)
			),
		
			# Download panel
			tabPanel(
				"Download results...",
				downloadButton('downloadData.dissMat', 'Download dissimilarity matrix...'),
				br(),
				downloadButton('downloadData.plot', 'Download dendrogram...')
			)
		) # End tabSetPanel()
		), # End sideBarPanel()

			# Main panel defintion
			mainPanel(
				tabsetPanel(
					tabPanel("Plot", plotOutput("plot")),
					tabPanel("Summary", verbatimTextOutput("print"))
					)
			) # End mainPanel()

	) # End pageWithSidebar()
) # End shinyUI()