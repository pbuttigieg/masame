#n-zjjJRxWbT5plxwN0
#runApp('./db-RDA', launch.browser = FALSE)

library(shiny)
library(vegan)
library(RColorBrewer)
## ui.R

shinyUI(
  pageWithSidebar(
    
    # Header defintion
    headerPanel("Perform a (partial) distance-based redundancy analysis"),
    
    # Sidebar defintion
    sidebarPanel(
      tabsetPanel(
        tabPanel("Data upload",
                 h5("Description"),
                 p("This App will perform a (partial) db-RDA using the capscale() function from the vegan package for R. Transformations are performed by decostand() {vegan} and dissimilarities calculated either by vegdist() {vegan} or, if transformations by extended flexible shortest path dissimilarities are desired, metaMDSdist {vegan}."),
                 
                 h5("CSV parameters"),
                 p("Note that these parameters apply to all files uploaded. If your files are not correctly formatted, errors will result."),
                 
                 
                 # Parameters for read.csv...
                 h5("Example data"),
                 p("Tick the box below if you'd like to use the 'mite' dataset included in the vegan package as an example. The 'mite.env' set of environmental parameters will be used as explanatory variables and as coniditional variables."),
                 checkboxInput('useExampleData', 'Use an example dataset', FALSE),
                 
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
                 
                 # File Upload
                 h5("Upload response data"),
                 strong("This data must be numeric, such as abundance data."),
                 fileInput(
                   inputId = 'dataset', 
                   label = 'Select a CSV file with a table of objects (sites, samples, etc) as rows and response variables as columns.',
                   accept = c('text/csv','text/comma-separated-values','.csv')
                 ),
                 
                 h5("Upload explanatory data"),
                 p("Ensure that the names and order of the objects (rows) are identical to your response data set."),
                 fileInput(
                   'explanatoryVars', 
                   'Select a CSV file with a table of objects (sites, samples, etc) as rows and explanatory variables as columns. Factor levels should have at least one non-numeric character. Numeric variables should have values that are solely numbers with no whitespace. Note: all columns will be used as contraints!',
                   accept = c('text/csv','text/comma-separated-values','.csv')
                 ),
                 
                 h5("Upload conditioning data"),
                 p("Uploading data here will trigger a partial analysis when the db-RDA parameters tab is selected. Ensure that there are no variables shared with your explanatory data."),
                 p("Ensure that the names and order of the objects (rows) are identical to other data sets uploaded."),
                 fileInput(
                   'conditioningVars', 
                   'Select a CSV file with a table of objects (sites, samples, etc) as rows and conditioning variables as columns. Factor levels should have at least one non-numeric character. Numeric variables should have values that are solely numbers with no whitespace.',
                   accept = c('text/csv','text/comma-separated-values','.csv')
                 ),
                 
                 h5("Upload stratification data"),
                 fileInput(
                   'strata', 
                   'If your objects are stratified (e.g. nested), select the CSV file which specifes which rows belong to each stratum. Strata should be represented by integers.',
                   accept = c('text/csv','text/comma-separated-values','.csv')
                 )
        ), 	# End file upload UI
        
        
        
        tabPanel(
          "Data transformations",
          
          
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
          ),   #ends input$transform
          
          
          # Scale variables to unit variance?
          checkboxInput('scaleVars', 'Would you like to scale your response variables to unit variance?', FALSE),
          
          
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
          
          # Should the conditioning variables be transformed? Input for decostand()
          
          br(),
          br(),
          h5("Conditioning data (if applicable)"),
          br(),
          
          selectInput(
            inputId = 'condTransformRorC',
            label = 'Would you like to transform the rows or columns of your conditioning data set?',
            choices = c(
              'Method default' = 0,
              'Rows' = 1,
              'Columns' = 2
            ),
            selected = 'Method default'
          ),
          
          
          selectInput(
            inputId = 'condTransform',
            label = 'If needed, select a transformation for your conditioning variables...',
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
          )#,
          
          #submitButton("Recalculate!")
          
        ), # End Transformations tab
        
        tabPanel(
          "db-RDA parameters...",
          
          
          h5("Dissimilarity"),
          # Parameters for vegdist or metaMDSdist...
          # Select dissimilarity measure
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
          h5("Negative eigenvalue correction"),
          # Correction method 2 for negative eigenvalues
          radioButtons(
            inputId = 'correctionMethod2',
            label = 'Should negative eigenvalues be corrected by the addition of a constant to non-diagonal dissimilarities?',
            choices = c(
              'Yes' = TRUE,
              'No' = FALSE
            )
          ),
          
          h5("FSP transformation"),
          # metaMDSdist autoscaling
          radioButtons(
            inputId = 'autoTransform',
            label = 'Should a flexible shortest path data transformation be attempted? This may help estimate dissimilarities between sites with no variables in common, but should be used with caution.',
            choices = c(
              'No'  = FALSE,
              'Yes' = TRUE
            )
          ),
          
          h5("Conditioning variables"),
          p("Displayed if applicable"),
          # Select the conditioning variables of interest...
          htmlOutput("whichCondVarsUI"),
          
          h5("Graphical parameters"),
          # Type of scaling to use...
          radioButtons(
            inputId = 'scaling',
            label = 'Would you like Type I or Type II scaling used in your biplot?',
            choices = c(
              'Type I' = 1,
              'Type II' = 2
            )),
            
            #introduce an option where files containing coloring factors can be uploaded
            fileInput(inputId = "colorfile", 
                      label = "Upload the file with the factors according to which db-RDA results will be colored. Please make sure the file is in the csv format.",
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
                      label = "Upload the file with the numeric factors according to which db-RDA results will be scaled. Please make sure the file is in the csv format.",
                      accept = c("text/csv", "text/comma-separated-values", ".csv")),
            
            checkboxInput('header3', 'Header', TRUE),
            
            numericInput(
              inputId = 'rownames3',
              value = 0,
              min = 0,
              label = 'Which column contains row lables from the size- data file (enter "0" if there is no such column)?'
            ),
            
            checkboxInput('useExampleDataSize', 'Use an example dataset as sizing variables', FALSE),
            htmlOutput("scalingVariable"),
           
          
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
        ),   #ends db-RDA parameters...
        
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
        )  #end tabPanel Download results...
      ) #ends tabsetPanel
    ),   #ends sidebarPanel
  # Main panel defintion
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Summary", verbatimTextOutput("print")),
      tabPanel("ANOVA test of significance", verbatimTextOutput("printSig"))
    )
  )
  
) #ends pagewithSidebar
)  #ends shinyUI
#)


