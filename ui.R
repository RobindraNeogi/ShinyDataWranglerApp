# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Data Wrangling Tool"),
  
  # 4 filter selections
  sidebarPanel(
    # Select variable for use in calc
    uiOutput("def"),
    uiOutput("control1alt"),
    uiOutput("control1"),
    uiOutput("control2"),
    uiOutput("control3"),
    uiOutput("control4"),
    #uiOutput("control5"),
    
   # Merge will add subset aggregat to contextual data 
    actionButton("Merge", "Create aggregate"),
    HTML("<br><br>"),
   # Text input fot name of aggregate
    textInput("text","Enter name of new aggregate",value = "Aggregate name"),
    HTML("<br>"),
   # Select variable for use in calc
    uiOutput("xcol"),
   # Select operator for use in calc
    selectInput("operator", "operator:",
                c("Divide by","Multiply","Add","Subtract")),
    
   # Select 2nd variable  for calc
   uiOutput("ycol"),
    # Name calc variable
    textInput("text2","Enter name of new metric",value = "Metric Name"),
   # Run calc
    actionButton("button", "Create metric"),
    HTML("<br><br>"),
    # uiOutput("Delete"),
   #  Delete last variable
    actionButton("DeleteButton", "Delete metric")
  ),
  

  mainPanel(
    
    # tableOutput("ImportedDataFiltered"),
    HTML("<br><br>"),
   # ignore for now 
   DT::dataTableOutput('ImportedDataFiltered2'),
    
   # Table shows filtered import data
   DT::dataTableOutput('ImportedDataFiltered'),
    downloadButton('DownloadFilteredData', 'Download'),
    HTML("<br><br>"),
   
   # table shows subset aggregate data
   
    DT::dataTableOutput("SubsetData"),
    HTML("<br><br>"),
   
   # Table shows final output merged data, aggegates and calculations
    DT::dataTableOutput("MergedData"),
    downloadButton('testdownload', 'Download')
  )
  
  
))

