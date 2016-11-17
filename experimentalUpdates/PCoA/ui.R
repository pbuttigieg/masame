#ejXwCM7
library(shiny)

## ui.R

# TODO: Add transformation functions? 
shinyUI(
  pageWithSidebar(
    
    # Header defintion
    headerPanel("Perform a principal coordinates analysis..."),
    
    # Sidebar defintion
    sidebarPanel(
      tabsetPanel(
        tabPanel("Data upload", 
                 h5("Description"),
                 p("This App will perform a PCoA using the capscale() function from the vegan package for R. Dissimilarities are computed by vegdist() {vegan} or, should a flexible shortest path data transformation be requested, by metaMDSdist() {vegan}."),
                 
                 h5("Example data"),
                 p("Tick the box below if you'd like to use the 'varechem' dataset included in the vegan package as an example."),
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
                 
                 h5("Data upload"),
                 
                 fileInput(
                   inputId = 'dataset', 
                   label = 'Select a CSV file to upload for analysis...',
                   accept = c('text/csv','text/comma-separated-values','.csv')
                 )
                 
                 # TODO: Future enhancement...
                 #fileInput(
                 #'metadata', 
                 #'If you would like to use additional data for modifying your plot (e.g. colouring points) upload a single column CSV file here...',
                 #accept = c('text/csv','text/comma-separated-values','.csv')
                 #),
                 
        ),
        
        # Only show this panel if the 
        tabPanel(
          "PCoA parameters...",
          # Parameters for metaMDS...
          # Select dissimilarity measure
          h5("Dissimilarity"),
          
          radioButtons(
            inputId = 'dissim',
            label = 'Select a dissimilarity measure',
            choices = c(
              'Euclidean' = 'euclidean',
              'Bray-Curtis' = 'bray',
              'Jaccard (presence/absence data)' = 'jaccard' # This will set presAbs to TRUE
            )
          ),
          
          # Presence absence or abundance?
          radioButtons(
            inputId = 'presAbs',
            label = 'Do you have abundance (or other count data) or presence absence data?',
            choices = c(
              'Abundance' = 'FALSE',
              'Presence / Absence' = 'TRUE'
            )
          ),
          
          # Correction method 2 for negative eigenvalues
          h5("Negative eigenvalues"),
          radioButtons(
            inputId = 'correctionMethod2',
            label = 'Should negative eigenvalues be corrected by the addition of a constant to non-diagonal dissimilarities?',
            choices = c(
              'Yes' = TRUE,
              'No' = FALSE
            )
          ),
          
          # metaMDSdist autoscaling
          h5("FSP transformation"),
          radioButtons(
            inputId = 'autoTransform',
            label = 'Should a flexible shortest path data transformation be attempted? This may help estimate dissimilarities between sites with no variables in common, but should be used with caution.',
            choices = c(
              'No'  = FALSE,
              'Yes' = TRUE
            )
          ),
          
          h5("Graphical parameters"),
          # Label points?
          checkboxInput('labels', 'Label points?', FALSE),
          
          #introduce an option where files containing coloring factors can be uploaded
          fileInput(inputId = "colorfile", 
                    label = "Upload the file with the factors according to which PCoA results will be colored. Please make sure the file is in the csv format.",
                    accept = c("text/csv", "text/comma-separated-values", ".csv")),
          
          checkboxInput('header2', 'Header', TRUE),
          
          numericInput(
            inputId = 'rownames2',
            value = 0,
            min = 0,
            label = 'Which column contains row lables from the color- data file (enter "0" if there is no such column)?'
          ),
          
          radioButtons(inputId = "factorType", 
                       label = "Please specify if the chosen variable is numeric or a factor", 
                       choices = c("Numeric", "Factor"), 
                       selected = NULL),
          
          checkboxInput('useExampleDataColor', 'Use an example dataset as coloring variables', FALSE),
          htmlOutput("colorVariable"),
          
          checkboxInput('showPoint', 
                        'Visualize the colored datapoints', 
                        TRUE),
          
          #introduce an option where files containing sizing factors can be uploaded
          fileInput(inputId = "sizefile", 
                    label = "Upload the file with the numeric factors according to which PCoA results will be scaled. Please make sure the file is in the csv format.",
                    accept = c("text/csv", "text/comma-separated-values", ".csv")),
          
          checkboxInput('header3', 'Header', TRUE),
          
          numericInput(
            inputId = 'rownames3',
            value = 0,
            min = 0,
            label = 'Which column contains row lables from the size- data file (enter "0" if there is no such column)?'
          ),
          
          checkboxInput('useExampleDataSize', 'Use an example dataset as sizing variables', FALSE),
          htmlOutput("scalingVariable")
        ),  #ends tabPanel PCoA parameters
        
        # Download panel
        tabPanel(
          "Download results...",
          downloadButton('downloadData.dissMat', 'DOWNLOAD dissimilarity matrix...'),
          br(),
          downloadButton('downloadData.plot', 'Download ordination...'),
          br(),
          downloadButton('downloadData.objectCoordinates', 'Download object coordinates...'),
          br(),			
          downloadButton('downloadData.variableCoordinates', 'Download variable coordinates...')	
        )
      )
    ),
    # Main panel defintion
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Summary", verbatimTextOutput("print"))#,
        #tabPanel("Table", tableOutput("table")
      )
    )
    
  )
)

