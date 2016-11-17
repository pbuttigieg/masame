#runApp('./Diag_multicollinearity', launch.browser = FALSE)

library(shiny)

## ui.R
shinyUI(
	pageWithSidebar(
		
		# Header defintion
		headerPanel("Screen a dataset for multicollinearity..."),
		
		# Sidebar defintion
		sidebarPanel(
			tabsetPanel(
			tabPanel("Data upload",
				
				h5("Description"),
				p("This App will use the rcorr() function from the Hmisc package for R to screen your data set for multicollinear variables. Transformations are performed by decostand() and wisconsin() from the vegan package as well as several standard functions. Visualisations are included as they are typically more informative than simple statistics."),
				strong("Only numeric variables will be screened. Uploading a data set with non-numeric values will result in errors."),
				
				h5("Example data"),
				p("Tick the box below if you'd like to use the 'varechem' dataset included in the vegan package as an example. This dataset includes 14 numeric variables describing chemical concentrations in soil samples at various sites."),
				checkboxInput('useExampleData', 'Use an example dataset', FALSE),
				
				h5("CSV parameters"),
				p("Note that these parameters apply to all files uploaded. If your files are not correctly formatted, errors will result."),
				
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
			
			tabPanel("Test parameters...",
				h5("Methods & thresholds"),
				p("Set the correlation and significance thresholds appropriate for your purposes below."),
				strong("Note: no corrections for multiple testing are performed!"),
				br(),
				
				radioButtons(
					inputId = 'corrType',
					label = 'What correlation method should be used?',
					choices = c(
						'Pearson (linear)' = 'pearson',
						'Spearman (rank based)' = 'spearman'
					),
					selected = 'Pearson (linear)'
				),
				
				numericInput(
					inputId = 'corrThreshold',
 					label = 'Correlation threshold. Variables with stronger correlations will be identified.',
 					value = 0.80,
					min = 0,
 					max = 1,
					step = 0.01
				),
		
				numericInput(
					inputId = 'pThreshold',
 					label = 'P-value threshold. Correlations (above the threshold above) with smaller p-values will be identified.',
 					value = 0.05,
					min = 0,
 					max = 1,
					step = 0.01
				)

			
			),
		
			tabPanel("Boxplots",
			
				#Slider controls which columns are plotted
				p("Move the sliders to select a range of variables to plot."),
				htmlOutput("boxPlotRangeUI")

			),
			
		
			tabPanel("Scatter plots",
			
				#Slider controls which columns are plotted
				p("Select a set of variables to plot in a scatter plot matrix. The diagonal will display variable names. The lower triangle of the matrix will display plots while the upper triangle will display their associated correlation coefficients."),
				htmlOutput("scatterPlotRangeUI")

			),
			
 			tabPanel("Transformations",
				
				p("If needed, select a transformation to apply to your data."),
				strong("Note that many of these transformation will be invalid if there are negative values in your data!"),
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
				downloadButton('downloadData.corrMatR', 'Download matrix of correlation coefficients...'),			
				br(),
				downloadButton('downloadData.corrMatP', 'Download matrix of p-values associated with correlation coefficients...')	
			) # End download panel
		)# End tabSetPanel
	), # End sideBarPanel

	# Main panel defintion
	
	mainPanel(
		tabsetPanel(
			tabPanel(
				"Test results",
				p("Note that examining graphs of your data is generally more informative / accurate than simple hypothesis tests."),
 				p("The variable pairs listed below were found to have correlations above and p-values below the thresholds specified. You can adjust these thresholds in the 'Test parameters' tab."),
				br(),
				verbatimTextOutput("numCorrVars"),
				verbatimTextOutput("corrVars")
				
			),
			tabPanel("Scatter plots", plotOutput("scatterPlots")),
			tabPanel("Boxplots", plotOutput("boxPlots"))
		) 
	
	)# End main panel definition

) # End pageWithSidebar
) # End shinyUI