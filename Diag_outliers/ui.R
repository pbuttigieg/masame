#runApp('./Diag_outliers', launch.browser = FALSE)

library(shiny)

## ui.R
shinyUI(
	pageWithSidebar(
		
		# Header defintion
		headerPanel("Screen a data set for outliers"),
		
		# Sidebar defintion
		sidebarPanel(
			tabsetPanel(
			tabPanel("Data upload",
			
			h5("Description"),
				p("This App will screen your data set for univariate and multivariate outliers. Univariate outliers are detected using simple statistics extracted from boxplot() function from the graphics package for R. When possible, the pcout(), sign1(), and sign2() functions from the mvoutlier package are used to detect potential multivariate outliers. Transformations are performed by decostand() and wisconsin() {vegan} as well as standard R functions."),
				p("Univariate outliers are often indicative of the existence of multivariate outliers; however, the absense of the latter does not follow from that of the former."),
				p("Often, any non-zero value in variables dominated by zero values (e.g. abundances of rare species) will be marked as an outlier."),
				p("Please note, that this screen is in no way conclusive. Multivariate data should be inspected with several, appropriate dimension-reduction techniques to investigate outliers."),
				
			br(),
			h5("Example data"),
				p("Tick the box below if you'd like to use the 'varechem' dataset included in the vegan package as an example."),
				checkboxInput('useExampleData', 'Use an example dataset', FALSE),
			br(),
			h5("CSV parameters"),
				p("Note that these parameters apply to all files uploaded. If your files are not correctly formatted, errors will result."),
				strong("Please upload a numeric data set. Non-numeric values will cause multivariate screens to fail and only some variables to be evaluated with univariate tests."),
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
				'Select a CSV file to upload for analysis. Variables should be represented as columns and objects (e.g. samples, sites, experimental units) as rows.',
				accept = c('text/csv','text/comma-separated-values','.csv')
				)
		
			), # End data upload tab
			
			# 
			
			tabPanel("Boxplots",
			
				#Slider controls which columns are plotted
				p("Move the sliders to select a range of variables to plot."),
				p("Selection of any non-numeric variables will result in errors."),
				htmlOutput("boxPlotRangeUI")

			),
			
			tabPanel("Histograms",
			
				#Slider controls which columns are plotted
				p("Move sliders navigate through your variables."),
				p("Selection of any non-numeric variables will result in errors."),
				htmlOutput("histPlotRangeUI")

			),
		
			tabPanel("Detection parameters...",
			
				p("Set the detection thresholds appropriate for your purposes below."),
				br(),
				
				numericInput(
					inputId = 'rangeThreshold',
 					label = 'Set the range threshold for univariate outlier detection below. This number will be multiplied by the interquartile range of each variable. The product will be used as the absolute distance from the median beyond which values will be considered outliers. Larger values will increase the tolerance for outlying values (i.e. they will not be considered outliers). A value of zero will result in no values being considered outliers.',
 					value = 1.5,
					min = 0,
					step = 0.5
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
				downloadButton('downloadData.uniOutliers', 'Download univariate outlier values and variables'),
				br(),
				downloadButton('downloadData.multiOutliers', 'Download multivariate outlier IDs')
			) # End download panel
		)# End tabSetPanel
	), # End sideBarPanel

	# Main panel defintion
	
	mainPanel(
		tabsetPanel(
			tabPanel(
				"Test results",
				p("Note that examining graphs of your data is generally more informative / accurate than simple hypothesis tests."),
 				p("Click on the downloads tab in the sidebar to download 1) a list of variables and the values of their associated, univariate outliers and 2) the names of the multivariate outliers detected through the PCOut and spatial signs procedures."),
				br(),
				h5("Univariate outliers"),
				verbatimTextOutput("boxPlotResults"),
 				h5("Multivariate outliers"),
				verbatimTextOutput("pcoutResults"),
				verbatimTextOutput("sign1Results"),
				verbatimTextOutput("sign2Results")
			),
			tabPanel("Boxplots", plotOutput("boxPlots")),
			tabPanel("Histograms", plotOutput("histPlots"))
		) 
	
	)# End main panel definition

) # End pageWithSidebar
) # End shinyUI