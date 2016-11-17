#runApp('./Diag_normality', launch.browser = FALSE)

library(shiny)

## ui.R
shinyUI(
	pageWithSidebar(
		
		# Header defintion
		headerPanel("Test a dataset for normality..."),
		
		# Sidebar defintion
		sidebarPanel(
			tabsetPanel(
			tabPanel("Data upload",
			
			h5("Description"),
				p("This App will perform univariate Shapiro-Wilk tests on each variable (column) in your data set using the shapiro.test() function from the stats package for R. A multivariate Shapiro-Wilk test is performed on your entire data set using the mshapiro.test() function from the mvnormtest library. Transformations are performed by decostand() and wisconsin() {vegan} as well as standard R functions."),
				p("In general, it is unlikely that ecological data will be normally distributed. Examining boxplots and histograms and using transformations to render the data 'near-normal' (e.g. symmetric, with no major outliers) is often more realistic."),
				
				h5("Example data"),
				p("Tick the box below if you'd like to use the 'mite' dataset included in the vegan package as an example."),
				checkboxInput('useExampleData', 'Use an example dataset', FALSE),
				
				h5("CSV parameters"),
				p("Note that these parameters apply to all files uploaded. If your files are not correctly formatted, errors will result."),
				strong("Please upload a numeric data set. Non-numeric values will result in errors."),
			br(),
			br(),
			
			checkboxInput('header', 'Header', TRUE),
			radioButtons(
				inputId = 'sep',
 				label = 'Separator',
				choices = c(
					Comma = ',',
					Semicolon = ';',
					Tab = '\t'
					)
				),
			
			numericInput(
				inputId = 'rownames',
				value = 1,
				min = 0,
				label = 'Which column contains row labels (enter "0" if there is no such column)?'
			),
			
			radioButtons(
				inputId = 'quote',
				label = 'Quote',
				choices = c(
					'None' = '',
					'Double quotes' = '"',
					'Single quotes' = "'"
				),
				selected = 'Double quotes'
			),
		
			h5("Upload data"),
			fileInput(
				'dataset', 
				'Select a CSV file to upload for analysis...',
				accept = c('text/csv','text/comma-separated-values','.csv')
				)
		
			), # End data upload tab
			
			# 
			
			tabPanel("Boxplots",
			
				#Slider controls which columns are plotted
				p("Move the sliders to select a range of variables to plot."),
				htmlOutput("boxPlotRangeUI")

			),
			
			tabPanel("Histograms",
			
				#Slider controls which columns are plotted
				htmlOutput("histPlotRangeUI")

			),
		
			tabPanel("Quantile-Quantile plots",
			
				#Slider controls which columns are plotted
				htmlOutput("qqPlotRangeUI")

			),
		
			tabPanel("Test parameters...",
			
				p("Set the significance thresholds appropriate for your purposes below."),
				p("Note: no corrections for multiple testing are performed!"),
				br(),
				
				numericInput(
					inputId = 'shapThreshold',
 					label = 'P-value threshold for Shapiro-Wilk tests of each variable',
 					value = 0.05,
					min = 0,
 					max = 1,
					step = 0.01
				),
		
				numericInput(
					inputId = 'mshapThreshold',
 					label = 'P-value threshold for multivariate Shapiro-Wilk test',
 					value = 0.05,
					min = 0,
 					max = 1,
					step = 0.01
				)

			
			),
			
 			tabPanel("Transformations",
				
				p("If needed, select a transformation to apply to your data."),
				p("Note that many of these transformation will be invalid if there are negative values in your data!"),
				br(),
				

				# Should the data be transformed? Input for decostand()
				selectInput(
					inputId = 'transformRorC',
					label = 'Would you like to transform the rows or columns of your data set?',
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
						'Take the square root of all values' = 'square.root',
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
				)
			), # End Transformations tab
		# Download panel
			tabPanel(
				"Download results...",
				br(),
				downloadButton('downloadData.transformedData', 'Download transformed data set...'),
				br(),
				downloadButton('downloadData.shapiroTestsPvals', 'Download p-values associated with univariate Shapiro-Wilk tests...')
			) # End download panel
		)# End tabSetPanel
	), # End sideBarPanel

	# Main panel defintion
	
	mainPanel(
		tabsetPanel(
			tabPanel(
				"Test results",
				p("Note that examining graphs of your data is generally more informative / accurate than simple hypothesis tests."),
 				br(),
				verbatimTextOutput("univarTestResults"),
 				verbatimTextOutput("multivarTestResults")
			),
			tabPanel("Boxplots", plotOutput("boxPlots")),
			tabPanel("Histograms", plotOutput("histPlots")),
			tabPanel("Quantile-Quantile plots", plotOutput("qqPlots"))
		) 
	
	)# End main panel definition

) # End pageWithSidebar
) # End shinyUI