library(shiny)
library(collapsibleTree)
library(ggplot2)
library(plotly)

navbarPage(
  title = 'WGA Analysis Tool',
  
  tabPanel(
    'Explore WGA Structure',
    
    #collapsibleTree is a visual representation of the data dimensions, does not control anything but to aid exploration
    
    collapsibleTree(
      WGA,
      hierarchy = c("Level1", "Level2", "Level3","Level4","Level5"),
      width = 1200,
      height=700),
    HTML("<br><br>")
  ),
  tabPanel(
    'Create items',
    
    # uiOutputs are the filters applied to the main data
    sidebarPanel(
      
      #This one recalls filter selections that went into created items but filtering the choices for selected choices in each filter
      uiOutput("def"),
      
      #Level 1 filter, set for one choice only to limit choices shown in other filters
      uiOutput("control1"),
      
      #Level 2 filter, multiple choice, choices depend on previous filter, defaut selection based on 'def' input
      uiOutput("control2"),
      
      #Level 3 filter, multiple choice, choices depend on previous filters, defaut selection based on 'def' input
      uiOutput("control3"),
      
      #Level 4 filter, multiple choice, choices depend on previous filters, defaut selection based on 'def' input
      uiOutput("control4"),
      
      #Level 4 filter, multiple choice, choices depend on previous filters, defaut selection based on 'def' input
      uiOutput("controlL5"),
      
      # Merge button will add filtered subset to the working data set
      actionButton("Merge", "Create aggregate"),
      HTML("<br><br>"),
      
      # Text input for name of filtered subset that will be added to working data set
      textInput("text", "Enter name of new aggregate", value = "Aggregate name"),
      downloadButton('Definitions', 'Download'),
      HTML("<br>")
    ),
    
    
    mainPanel(
      
      
      
      #Table shows the raw data after filters are applies
      DT::dataTableOutput('ImportedDataFiltered'),
      
      #download button to download filtered data
      downloadButton('DownloadFilteredData', 'Download'),
      HTML("<br><br>")
      
    )
    
    #Tab 2 ######################################################################################################################################   
    
  ),
  tabPanel(
    'Create metric',
    
    sidebarPanel(
      
      # Select 1st variable to be used in calculation
      uiOutput("xcol"),
      
      # Select operator for use in calc
      selectInput(
        "operator",
        "operator:",
        c("%", "*", "+", "-")
      ),
      
      # Select 2nd variable  for calc
      uiOutput("ycol"),
      
      # Name calc variable
      
      uiOutput("text2"),
      
      # Run calc
      verbatimTextOutput("metric1"),
      actionButton("button", "Create metric"),
      HTML("<br><br>"),
      
      # uiOutput("Delete"),
      #  Delete last variable
      actionButton("DeleteButton", "Delete metric"),
      downloadButton('testdownload', 'Download')
    ),
    
    
    # show working data set which includes original contextual data, created aggregates and created calculations
    mainPanel(# tableOutput("ImportedDataFiltered"),
      
      DT::dataTableOutput("MergedData"),
      HTML("<br><br>"))
  ),
  
  #tab 3 k-means clustering ######################################################################################
  
  tabPanel(
    'K-Means',
    
    sidebarPanel(width=3,
                 
                 
                 uiOutput('clusters'),
                 sliderInput("range", "Set min/max",
                             min = 2, max = 25, value = c(2,25)),
                 verbatimTextOutput("rangeno"),
                 # not yet implemented, multi choice list for creating dataset with more than 2 variables for kmeans
                 uiOutput("kmeansvariables"),
                 uiOutput("kmeansLAtype"),
                 verbatimTextOutput("kmeans"),
                 verbatimTextOutput("optimalclusters"),
                 verbatimTextOutput("optimalclusters2")
    ),
    
    #Scatter plot of selected x and y variables
    mainPanel(plotOutput('plot1'),
              
              plotOutput('sil'),
              verbatimTextOutput("sill"),
              
              # this is not yet implemented, want it to display data created with kmeansvariables input mentioned above 
              DT::dataTableOutput("MergedData2"))
  ),
  
  # tab 4 charts ###########################################################################################################
  
  tabPanel(
    'Charts',
    
    # select variable to display in charts
    
   
      uiOutput("chartvariable"),
      
      verbatimTextOutput("summary"),
      
      
      
      
      
      fluidRow(
        column(6,
               plotOutput("hist"),
               sliderInput("integer", "Integer:",
                           min=1, max=100, value=30)
        ),
        column(6,
               plotOutput("boxjitter2")
        )
      ),
      
      plotOutput("barchart2"),
      
      
      DT::dataTableOutput("Charts")
      
    )
  )
    
  
  

