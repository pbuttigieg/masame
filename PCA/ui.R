library(shiny)

## ui.R

shinyUI(
	pageWithSidebar(
		
		# Header defintion
		headerPanel("Perform a principal components analysis..."),
		
		# Sidebar defintion
		sidebarPanel(
			tabsetPanel(
			tabPanel("Data upload",
 				h5("Description"),
				p("This App will perform a PCA using the rda() function from the vegan package for R. Transformations are performed by decostand(), also from vegan"),
				
				h5("Example data"),
				p("Tick the box below if you'd like to use the 'dune' dataset included in the vegan package as an example."),
				checkboxInput('useExampleData', 'Use an example dataset', FALSE),
				
				
				h5("CSV parameters"),
				p("Note that these parameters apply to all files uploaded. If your files are not correctly formatted, errors will result."),
			
				# Parameters for read.csv...
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
				
				h5("Upload file"),
				fileInput(
					inputId = 'dataset', 
					label = 'Select a CSV file to upload for analysis...',
					accept = c('text/csv','text/comma-separated-values','.csv')
				)
				
				# TODO: See how this can be done
				#fileInput(
					#'metadata', 
					#'If you would like to use additional data for modifying your plot (e.g. colouring points) upload a single column CSV file here...',
					#accept = c('text/csv','text/comma-separated-values','.csv')
					#),
	), # End data upload tab

	tabPanel(
      "Transformations",
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
			)
	),
   
	tabPanel(
      "PCA parameters",
			# Parameters for PCA...
			
			# Type of scaling to use...
			radioButtons(
				inputId = 'scaling',
				label = 'Would you like Type I or Type II scaling used in your biplot?',
				choices = c(
					'Type I' = 1,
					'Type II' = 2
					)
				),
			
			# Label points?
			h5("Graphical parameters"),
			radioButtons(
				inputId = 'labels',
				label = 'Would you like points to be labeled?',
				choices = c(
					'Yes' = "text",
					'No' = "points"
					)
			)
			
		),
		
		tabPanel(
			"Download results...",
			downloadButton('downloadData.plot', 'Download plot...'),
			br(),
			downloadButton('downloadData.objectScores', 'Download object scores...'),
			br(),
			downloadButton('downloadData.variableScores', 'Download variable scores...')
			)
		) # End tabSetPanel
	), # End sidebarPanel

	# Main panel defintion
	mainPanel(
		tabsetPanel(
			tabPanel("Plot", plotOutput("plot")),
			tabPanel("Summary", verbatimTextOutput("print")),
			tabPanel("Eigenvalues", verbatimTextOutput("eigenvals")),
			tabPanel("Object scores", verbatimTextOutput("objectScores")),
			tabPanel("Variable scores", verbatimTextOutput("variableScores"))#,
			#tabPanel("Table", tableOutput("table")
		)
	)

			) # End pageWithSidebar
		) # End shinyUI
	#)