# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Menu Test"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    uiOutput("control1"),
    
    uiOutput("control2"),
    uiOutput("control3"),
    uiOutput("control4"),
    #uiOutput("control5"),
    
    
    actionButton("Merge", "Create aggregate"),
    HTML("<br><br>"),
    textInput("text","Enter name of new aggregate",value = "Aggregate name"),
    HTML("<br>"),
    uiOutput("xcol"),
    selectInput("operator", "operator:",
                c("Divide by","Multiply","Add","Subtract")),
    uiOutput("ycol"),
    
    textInput("text2","Enter name of new metric",value = "Metric Name"),
    actionButton("button", "Create metric"),
    HTML("<br><br>"),
    # uiOutput("Delete"),
    actionButton("DeleteButton", "Delete metric")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    
    # tableOutput("ImportedDataFiltered"),
    HTML("<br><br>"),
    DT::dataTableOutput('ImportedDataFiltered2'),
    DT::dataTableOutput('ImportedDataFiltered'),
    downloadButton('DownloadFilteredData', 'Download'),
    HTML("<br><br>"),
    DT::dataTableOutput("SubsetData"),
    HTML("<br><br>"),
    DT::dataTableOutput("MergedData"),
    downloadButton('testdownload', 'Download')
  )
  
  
))

