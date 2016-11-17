#runApp('./RDA', launch.browser = FALSE)

library(shiny)

## ui.R

# TODO: Add text saying that the data should already be pretreated (e.g. transformed)
shinyUI(
	pageWithSidebar(
		
		# Header defintion
		headerPanel("Perform a Redundancy Analysis..."),
		
		# Sidebar defintion
		sidebarPanel(
			tabsetPanel(
			tabPanel("Data upload", 
				
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
				
				fileInput(
					inputId = 'dataset', 
					label = 'Select a CSV file with a table of objects (sites, samples, etc) as rows and response variables as columns.',
					accept = c('text/csv','text/comma-separated-values','.csv')
					),
				
				
				fileInput(
					'explanatoryVars', 
					'Select a CSV file with a table of objects (sites, samples, etc) as rows and explanatory variables as columns. Factor levels should have at least one non-numeric character. Numeric variables should have values that are solely numbers with no whitespace. Note: all columns will be used as contraints!',
					accept = c('text/csv','text/comma-separated-values','.csv')
					),
				
				fileInput(
					'conditioningVars', 
					'Select a CSV file with a table of objects (sites, samples, etc) as rows and conditioning variables as columns. Factor levels should have at least one non-numeric character. Numeric variables should have values that are solely numbers with no whitespace. All columns will be used as conditioning variables!',
					accept = c('text/csv','text/comma-separated-values','.csv')
					),
				
				# Select the conditioning variables of interest...
				
					htmlOutput("whichCondVarsUI"),
				
				fileInput(
					'strata', 
					'If your objects are stratified (e.g. nested), select the CSV file which specifes which rows belong to each stratum. Strata should be represented by integers.',
					accept = c('text/csv','text/comma-separated-values','.csv')
					)
				
				
				),
			
   tabPanel(
      "RDA parameters...",
					
			# Should the data be transformed? Input for decostand()
			radioButtons(
				inputId = 'transform',
				label = 'Select a transformation if needed...',
				choices = c(
					'No transformation' = 'none',
					'Z score' = 'standardize',
					'Chi square' = 'chi.square',
					'Hellinger' = 'hellinger'
					)
				),
			
			# Should the explanatory data be transformed? Input for decostand()
			radioButtons(
				inputId = 'expTransform',
				label = 'Select a transformation if needed...',
				choices = c(
					'No transformation' = 'none',
					'Z score' = 'standardize',
					'Chi square' = 'chi.square',
					'Hellinger' = 'hellinger'
					)
				),
			
			# Should the conditioning variables be transformed? Input for decostand()
			radioButtons(
				inputId = 'condTransform',
				label = 'Select a transformation if needed...',
				choices = c(
					'No transformation' = 'none',
					'Z score' = 'standardize',
					'Chi square' = 'chi.square',
					'Hellinger' = 'hellinger'
					)
				),
			
			# Scale variables to unit variance?
			checkboxInput('scaleVars', 'Would you like to scale your variables to unit variance?', FALSE),
			
			
			# Type of scaling to use in plot...
			radioButtons(
				inputId = 'scaling',
				label = 'Would you like Type I or Type II scaling used in your plot?',
				choices = c(
					'Type I' = 1,
					'Type II' = 2
				)
			),
			
			# Label points?
			radioButtons(
				inputId = 'labels',
				label = 'Would you like points to be labeled?',
				choices = c(
					'Yes' = "text",
					'No' = "points"
				)
			),
			

			# Objects, variables, or both?
			radioButtons(
				inputId = 'display',
				label = 'Would you like to plot the objects, the response variables, or both?',
				choices = c(
					'both' = "both",
					'objects' = "sites",
					'variables' = "species"
				)
			)
		),
		
		# Download panel
		tabPanel(
			"Download results...",
			downloadButton('downloadData.plot', 'Download ordination...'),
			downloadButton('downloadData.objectCoordinates', 'Download object coordinates...'),			
			downloadButton('downloadData.variableCoordinates', 'Download variable coordinates...')	
			)
		)
	),
			# Main panel defintion
			# TODO: Use tabPanels for plots and numeric result output
			# See if conditionalPanel works for relevant parameters
			mainPanel(
				tabsetPanel(
					tabPanel("Plot", plotOutput("plot")),
					tabPanel("Summary", verbatimTextOutput("print")),
					tabPanel("ANOVA test of significance", verbatimTextOutput("printSig"))
					)
				)

			)
		)
	#)