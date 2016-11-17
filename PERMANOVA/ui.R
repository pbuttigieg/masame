#runApp('./PERMANOVA', launch.browser = FALSE)

library(shiny)
options(shiny.error=FALSE)
options(shiny.try = FALSE)
## ui.R

shinyUI(
	
	pageWithSidebar(
		
		# Header defintion
		headerPanel("Perform permutational multivariate analysis of variance using distance matrices..."),
		
		# Sidebar defintion
		sidebarPanel(
			tabsetPanel(
			tabPanel("Data upload", 
				
				h5("Description"),
				p("This App will perform a permutational multivariate analysis of variance using distance matrices via the adonis() function from the vegan package for R. Transformations are performed by decostand() {vegan}, wisconsin() {vegan}, and several standard functions. Distances are calculated using vegdist() {vegan}."),
				br(),
				h5("Example data"),
				p("Tick the box below if you'd like to use the 'dune' dataset included in the vegan package as an example. The associated environmental variables in 'dune.env' constitute the explanatory matrix."),
				checkboxInput('useExampleData', 'Use an example dataset', FALSE),
				br(),
				h5("CSV parameters"),
				p("Note that these parameters apply to all files uploaded. If your files are not correctly formatted, errors will result."),
				
				# Parameters for read.csv...
				checkboxInput('header', 'Header', TRUE),
				
				numericInput(
					inputId = 'rownames',
					value = 1,
					min = 0,
 					label = 'In each file, which column contains row lables (enter "0" if there are no such columns)?'
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
				
				# Upload files...
				h5("Response data"),
				fileInput(
					inputId = 'dataset', 
					label = 'Select a CSV file with a table of objects (sites, samples, etc) as rows and response variables as columns.',
					accept = c('text/csv','text/comma-separated-values','.csv')
				),
			
				h5("Explanatory / grouping data"),
				fileInput(
					inputId = 'explanatoryVars', 
					label = 'Select a CSV file with explanatory variables. These may be factors or continuous variables.',
					accept = c('text/csv','text/comma-separated-values','.csv')
				),
			
				h5("Stratification data"),
				fileInput(
					inputId = 'strata', 
					label = 'If your objects are stratified (e.g. nested), select the CSV file which specifes which rows belong to each stratum. Strata should be represented by integers.',
					accept = c('text/csv','text/comma-separated-values','.csv')
				)
			),
			
			
			tabPanel("Transformations",
				
				p("If needed, select a transformation to apply to your data."),
				strong("Note that many of these transformation will fail and produce errors if there are negative or non-numeric values in your data!"),
				br(),
				br(),
				HTML("Click <b>recalculate</b> after you've set new parameters."),
				br(),
				
				h5("Response data"),
				# Should the data be transformed? Input for decostand()
				selectInput(
					inputId = 'transformRorC',
					label = 'Would you like to transform the rows or columns of your response data set?',
					choices = c(
						'Method default' = 0,
						'Rows' = 1,
						'Columns' = 2
					),
					selected = 'Method default'
				),
					
				selectInput(
					inputId = 'transform',
					label = 'Select a standardisation or transformation method. Where applicable, row/column transformation will be over-ridden based on your input above. ',
					choices = c(
						'No transformation' = 'none',
						'Divide values by row totals' = 'total',
						'Divide values by column maxima' = 'max',
						'Take the square root of all values' = 'square.root', # not decostand!
						'Take the logarithm (base 2) of all values and then add one. Zeros left unchanged.' = 'log',
						'Standardise row sums-of-squares to one' = 'normalize',
						'Standardise columns to zero mean and unit variance (z-score)' = 'standardize',
						'Standardise column values to fall within the interval [0,1]. All values in columns with no variation will be set to zero.' = 'range',
						'Convert to presence/absense (1/0) data' = 'pa',
						'Set the average of non-zero entries across columns to one' = 'freq',
						'Wisconsin double standardisation' = 'wisconsin', # not decostand!
						'Chi square standardisation' = 'chi.square',
						'Hellinger transformation' = 'hellinger'
					)
				),
			
			# Should the explanatory data be transformed? Input for decostand()
			
				br(),
				br(),
				h5("Explanatory data"),
				br(),
					
				selectInput(
					inputId = 'expTransformRorC',
					label = 'Would you like to transform the rows or columns of your explanatory data set?',
					choices = c(
						'Method default' = 0,
						'Rows' = 1,
						'Columns' = 2
					),
					selected = 'Method default'
				),
			
			
			selectInput(
				inputId = 'expTransform',
				label = 'If needed, select a transformation for your explanatory variables...',
				choices = c(
						'No transformation' = 'none',
						'Divide values by row totals' = 'total',
						'Divide values by column maxima' = 'max',
						'Take the square root of all values' = 'square.root', # not decostand!
						'Take the logarithm (base 2) of all values and then add one. Zeros left unchanged.' = 'log',
						'Standardise row sums-of-squares to one' = 'normalize',
						'Standardise columns to zero mean and unit variance (z-score)' = 'standardize',
						'Standardise column values to fall within the interval [0,1]. All values in columns with no variation will be set to zero.' = 'range',
						'Convert to presence/absense (1/0) data' = 'pa',
						'Set the average of non-zero entries across columns to one' = 'freq',
						'Wisconsin double standardisation' = 'wisconsin', # not decostand!
						'Chi square standardisation' = 'chi.square',
						'Hellinger transformation' = 'hellinger'
					)
				),
			 
				submitButton("Recalculate!")
			
			), # End Transformations tab
			
	   tabPanel(
		  "PERMANOVA parameters...",
		
		HTML("Click <b>recalculate</b> after you've set new parameters."),
		br(),
		br(),
		
		# Custom formula
		h5("Custom formula"),
		textInput(
			inputId = 'customFormula',
 			label = 'By default, all your explanatory variables will be included in your model with no interactions defined. You may define a different model below by entering your explanatory variable names (as they appear in your uploaded data) and either "+" (additive inclusion, no interaction) or "*" (interactions) between them.',
 			value = "."
		),

		
		 # Select dissimilarity measure;
		h5("Dissimilarity"),
		selectInput(
			inputId = 'dissim',
			label = 'Select a (dis)similarity or association measure.',
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
				'Chao' = 'chao'
			)
		),
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
			
			h5("Number of permutations"),
			numericInput(
				inputId = 'numPermute',
				label = 'Number of permutations to perform to estimate significance.',
				value = 999,
				min = 10,
				step = 1		
			),
		
		 submitButton("Recalculate!")
			
		), 

		
		# Download panel
		tabPanel(
			"Download results...",
			downloadButton('downloadData.transData', 'Download transformed data'),
			br(),
			downloadButton('downloadData.dissMat', 'Download dissimilarity matrix')
			)
		)
	),
			# Main panel defintion
			mainPanel(
				tabsetPanel(
					tabPanel("Test results", verbatimTextOutput("print"))
					)
				) # End main panel def 

			) # End pageWithSidebar
		) # End shinyUI
	