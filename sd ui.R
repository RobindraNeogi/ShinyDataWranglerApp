library(shinydashboard)

dashboardPage(
  dashboardHeader(),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              # Sidebar with a slider input for number of observations
              sidebarPanel(
                fluidRow(
                  
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
                  
                )
                
              ),
              
              
              mainPanel(
                
                fluidRow(
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
              )
              
      )
    )
  )
)