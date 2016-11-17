#runApp('./PCNM', launch.browser = FALSE)

library(shiny)

## ui.R

shinyUI(
	
	pageWithSidebar(
			
		# Header defintion
		headerPanel("Perform a principal coordinates of neighbour matrices (PCNM) analysis..."),
		
		# Sidebar defintion
		sidebarPanel(
			tabsetPanel(
				tabPanel("Data upload...",
					h5("Description"),
					p("This App will perform an PCNM analysis using the pcnm() function from the vegan package for R. Euclidean distances are calculated using the dist() {stats} function."),
					
					h5("Example data"),
					p("Tick the box below if you'd like to use the 'mite.xy' spatial dataset included in the vegan package as an example."),
					checkboxInput('useExampleData', 'Use an example dataset', FALSE),
				
					
					
					h5("CSV parameters"),
					p("Note that these parameters apply to all files uploaded. If your files are not correctly formatted, errors will result."),
					
					
					# Parameters for read.csv...
					checkboxInput('header', 'Header', TRUE),
					
					numericInput(
							inputId = 'rownames',
							value = 1,
							min = 0,
 							label = 'Which column contains row labels (enter "0" if there is no such column)?'
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
					
					# Upload distance file
					h5("Distance data"),
					fileInput(
						inputId = 'distance', 
						label = 'Select a CSV file of spatial or temporal distances between your objects...',
						accept = c('text/csv','text/comma-separated-values','.csv')
					),
			
					## Upload dataset destined for CCA ordination... 
					# NOT IMPLEMENTED, WILL CONSIDER THIS FOR A FUTURE 
					# ENHANCEMENT
					#fileInput(
						#'dataset', 
						#'An additional dataset for ordination through CCA...',
						#accept = c('text/csv','text/comma-separated-values','.csv')
						#),
					
					h5("Weight data"),	
					fileInput(
						inputId = 'weights', 
						label = 'If applicable, select a CSV file with a single column of weights for each object in your distance file to perform a weighted PCNM analysis...',
						accept = c('text/csv','text/comma-separated-values','.csv')
					)
			
					
				), # End data upload tab
					
				tabPanel("PCNM parameters...",	# Parameters for PCNM...
					
					h5("Custom distance threshold"),
					checkboxInput(
						inputId = 'threshold',
 						label = 'Would you like to use a custom distance threshold? By default, the minimum distance required to ensure all points are connected is used to truncate the Euclidean distance matrix used in PCNM. Larger distances are set to four times the truncation distance.',
 						value = FALSE
					),
					
					br(),
					br(),
					
					htmlOutput("pcnmThresholdValueUI")
					
						
				), # End PCNM parameter tab
			
			tabPanel("Graphical parameters...",	# Parameters for plots...
							
					# Select which PCNMs to examine individually...
					strong("Multi-panel plot"),
					p("Select which PCNM you'd like to examine individually in a multi-panel plot..."),
					htmlOutput("pcnmPlotChoiUI1"),
					htmlOutput("pcnmPlotChoiUI2"),
					htmlOutput("pcnmPlotChoiUI3"),
					htmlOutput("pcnmPlotChoiUI4"),
					htmlOutput("pcnmPlotChoiUI5"),
					htmlOutput("pcnmPlotChoiUI6"),
					br(),
					
					# Select which PCNM axis to plot as a surface
					strong("Surface plot"),
					p("Select which PCNM you'd like to plot as a surface over a plot of your coordinates..."),
					htmlOutput("pcnmSurfChoiUI"),
					br(),				
					
					# Select PCNMs to plot against one another
					strong("PCNM vs PCNM plot"),
					p("Select which PCNMs you'd like to plot against one another."),
					p("Scatter plots appear in the lower diagonal while Pearson correlation coefficients are displayed in the upper diagonal."),
					htmlOutput("pcnmVsPlotChoiUI1"),
					htmlOutput("pcnmVsPlotChoiUI2"),
					htmlOutput("pcnmVsPlotChoiUI3"),
					htmlOutput("pcnmVsPlotChoiUI4")
		
		
				), # End Graphical parameter tab
			
				tabPanel(
					"Download results...",
					downloadButton('downloadData.multiPlot', 'Download PCNM multi-panel plot...'),
					br(),
					downloadButton('downloadData.surfPlot', 'Download Surface plot...'),
					br(),
					downloadButton('downloadData.vsPlot', 'Download PCNM vs PCNM plot...'),
					br(),
					downloadButton('downloadData.vectors', 'Download PCNM eigenvectors...'),
					br(),
					downloadButton('downloadData.values', 'Download PCNM eigenvalues...'),
					br(),
					downloadButton('downloadData.distanceMatrix', 'Download truncated distance matrix...')
				) # End Download panel
			
			) # End tabSetPanel
		), # End sidebar panel definition
			
		# Main panel defintion
		# See if conditionalPanel works for relevant parameters
		mainPanel(
			tabsetPanel(
				tabPanel("PCNM multi-panel plot", plotOutput("pcnmPlot")),
				tabPanel("Surface plot", plotOutput("pcnmSurf")),
				tabPanel("PCNM vs PCNM", plotOutput("vsPlot")),
				#tabPanel("Multiscale ordination", plotOutput("msoPlot")), # Not implemented
				tabPanel("PCNM eigenvectors", verbatimTextOutput("vectorsPrint")),
				tabPanel("PCNM eigenvalues", verbatimTextOutput("valuesPrint")),
				tabPanel("Truncated distance matrix", verbatimTextOutput("truncDist"))
	
		) # End tabsetPanel
	) # End main panel definition
	) # End pageWithSidebar
) # End shinyUI function