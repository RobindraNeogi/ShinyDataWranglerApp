#test new commit test test 
library(shiny)
library(shinydashboard)
## create dummy dataframe

#ImportedData <- Dummydata<-data.frame(ID = c("LA1","LA2","LA3","LA1","LA2","LA3","LA1","LA2","LA3"),
#                          Level2 = c("A","B","C","A","B","C","A","B","C"),
#                        Level3 = c("AA","BA","CA","BA","BB","BC","CA","CB","CA"),
#                         Level4 = c("AAA","BAB","CAC","ABA","BBB","CBC","ACA","BCB","CCC"),
#                       Value = c(1,2,3,4,5,6,7,8,9))

#ImportedData <-read.csv("C:\\Users\\neog968\\Desktop\\wgadata.csv",header=TRUE)
ImportedData <-read.csv("/Users/datascience4/Documents/datatool/wgadata.csv",header=TRUE)
Definitions <-read.csv("/Users/datascience4/Documents/datatool/wgadimensions.csv",header=TRUE)




## ContextualData is the data I want to join the aggregated subsets too. in the final one it would
## have population, indicies of deprivation etc.

#ContextualData <- data.frame(ID = c("LA1","LA2","LA3"),
#   Population = c(1,2,3),
# deprivation=c(4,5,6))

# ContextualData <-read.csv("C:\\Users\\neog968\\Desktop\\contextualdata.csv",header=TRUE)
ContextualData <-read.csv("/Users/datascience4/Documents/datatool/contextualdata.csv",header=TRUE)


shinyServer(function(input, output, session) {
  
  ## Outputs filtered import data filtered by 4 menu selections
  #output$ImportedDataFiltered <- renderTable({
  # ImportedData<-ImportedData[ImportedData$Level1 %in% input$menu1,]
  #ImportedData<-ImportedData[ImportedData$Level2 %in% input$menu2,]
  #ImportedData<-ImportedData[ImportedData$Level3 %in% input$menu3,]
  #ImportedData<-ImportedData[ImportedData$Level4 %in% input$menu4,]
  #})
  
  
  #display definitions table
  output$ImportedDataFiltered2 <- DT::renderDataTable(
    DT::datatable(Mergeddef$df, options = list(searching = FALSE),
                  rownames= FALSE))
  
  
  #display imported data table
  output$ImportedDataFiltered <- DT::renderDataTable(
    DT::datatable(FilteredImportData(), options = list(searching = FALSE),
                  rownames= FALSE))
  
  
  output$DownloadFilteredData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(FilteredImportData$df, file)
    }
  )
  
  ##  Imported data filtered by 4 menus (similar to above but wanted to reuse filtered data)
  
  FilteredImportData<-reactive({FilteredImportData <-ImportedData[ImportedData$Level1 %in% input$menu1,]
  ImportedData<-ImportedData[ImportedData$Level2 %in% input$menu2,]
  ImportedData<-ImportedData[ImportedData$Level3 %in% input$menu3,]
  ImportedData<-ImportedData[ImportedData$Level4 %in% input$menu4,]
  # ImportedData<-ImportedData[ImportedData$Level5 %in% input$menu5,]
  })
  
  ## SubsetData This is aggregating the FilteredImportData, grouping by ID (in this case LA but in
  ## final will be the LA/Year ID). Req input menus to get rid of temporary error message on launch
  
  SubsetData <-
    
    reactive({
      req(input$menu1)
      req(input$menu2)
      req(input$menu3)
      req(input$menu4)
      #req(input$menu5)
      
      aggregate(Value ~ Code, FilteredImportData(), sum)
    })
  
  
  
  SubsetDataDefinition <-
    
    reactive({
      req(input$menu1)
      req(input$menu2)
      req(input$menu3)
      req(input$menu4)
      #req(input$menu5)
      
      unique(FilteredImportData()['ID'])
    })
  

  
  
  ## This inputs aggreagte variable name from text input
  namerev<-reactive({
    as.character(paste(input$text))
  })
  
  ## This replaces the column label 'Value' with the inputted name
  SubsetDataDefinition2<-reactive({
    d <- SubsetDataDefinition()
    colnames(d)[colnames(d)=='ID'] <- namerev()
    d
  })
  
  ## This replaces the column label 'Value' with the inputted name
  SubsetData2<-reactive({
    d <- SubsetData()
    colnames(d)[colnames(d)=='Value'] <- namerev()
    d
  })
  
  
  ## This table displays the SubsetData
  
  output$SubsetData <- DT::renderDataTable(
    DT::datatable(SubsetData(), options = list(searching = FALSE),
                  rownames= FALSE))
  
  
  ## When 'create aggregate' button clicked SubsetData is merged with ContextualData.
  ## Each click adds new column
  ## when 'create ratio' button is clicked a calculated column is created using the two selected columns and operator
  
  MergedContextualDataWithSubset <- reactiveValues()
  MergedContextualDataWithSubset$df<-ContextualData
  
  
  observe({
    if(input$Merge > 0) {
      isolate(
        
        MergedContextualDataWithSubset$df <- merge
        (MergedContextualDataWithSubset$df,SubsetData2(),by="Code",all=TRUE)
      )
    }
  })
  observe({
    
    if(input$button > 0) {
      
      isolate(if(input$operator=="Divide by") {
        
        newvar <- isolate((MergedContextualDataWithSubset$df[[input$xcol]] / MergedContextualDataWithSubset$df[[input$ycol]]))
        isolate(MergedContextualDataWithSubset$df <- cbind(MergedContextualDataWithSubset$df, newvar))
        isolate(colnames(MergedContextualDataWithSubset$df)[colnames(MergedContextualDataWithSubset$df)=='newvar'] <- as.character(paste(input$text2)))
        
        
      })
      
      isolate(if(input$operator=="Multiply") {
        
        newvar <- isolate((MergedContextualDataWithSubset$df[[input$xcol]] * MergedContextualDataWithSubset$df[[input$ycol]]))
        isolate(MergedContextualDataWithSubset$df <- cbind(MergedContextualDataWithSubset$df, newvar))
        isolate(colnames(MergedContextualDataWithSubset$df)[colnames(MergedContextualDataWithSubset$df)=='newvar'] <- as.character(paste(input$text2)))
        
        
      })
      
      isolate(if(input$operator=="Add") {
        
        newvar <- isolate((MergedContextualDataWithSubset$df[[input$xcol]] + MergedContextualDataWithSubset$df[[input$ycol]]))
        isolate(MergedContextualDataWithSubset$df <- cbind(MergedContextualDataWithSubset$df, newvar))
        isolate(colnames(MergedContextualDataWithSubset$df)[colnames(MergedContextualDataWithSubset$df)=='newvar'] <- as.character(paste(input$text2)))
        
        
      })
      
      isolate(if(input$operator=="Subtract") {
        
        newvar <- isolate((MergedContextualDataWithSubset$df[[input$xcol]] - MergedContextualDataWithSubset$df[[input$ycol]]))
        isolate(MergedContextualDataWithSubset$df <- cbind(MergedContextualDataWithSubset$df, newvar))
        isolate(colnames(MergedContextualDataWithSubset$df)[colnames(MergedContextualDataWithSubset$df)=='newvar'] <- as.character(paste(input$text2)))
        
      })
    }
    
  })
  
  
  #merge filtered IDs to master ID file to be used later for definitions
  
  Mergeddef <- reactiveValues()
  Mergeddef$df<-Definitions
  
  observe({
    if(input$Merge > 0) {
      isolate(
        
        Mergeddef$df <- merge
        (Mergeddef$df,SubsetDataDefinition(),by="ID",all=TRUE,type="full")
      )
    }
  })
 
  
  
  
  observe({
    if(input$DeleteButton > 0) {
      isolate(MergedContextualDataWithSubset$df <- MergedContextualDataWithSubset$df[-c(length( names( MergedContextualDataWithSubset$df ) ))]
      )
    }
  })
  
  
  ## Display MergedContextualDataWithSubset
  output$MergedData <- DT::renderDataTable(
    DT::datatable(MergedContextualDataWithSubset$df, options = list(searching = FALSE),
                  rownames= FALSE))
  
  
  #output$MergedData <- renderTable({MergedContextualDataWithSubset$df})
  
  
  output$testdownload <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(MergedContextualDataWithSubset$df, file)
    }
  )
  
  ## UI Elements
  
  ## Lists what has been entered in each menu
  
  output$FilterChoices <- renderText({
    x <- input$menu1
    y <- input$menu2
    z <- input$menu3
    zz<- input$menu4
    zzz<- input$menu5
    
    ## This is left over from the code I borrowed online. It makes the word 'select' appear
    ## if filters are empty, but my version has all valid options selected by default
    
    # if (any(
    #  is.null(x),
    # is.null(y),
    #is.null(z),
    #is.null(zz),
    #is.null(zzz)
    #  ))
    #   return("Select")
    
  })
  
  ## Makes 1st filter appear. options based on unique items in column
  
  output$control1 <- renderUI({
    selectInput("menu1", "Select Level 1", choices = unique(ImportedData$Level1),selected="Assets",multiple=TRUE)
  })
  
  ## Makes 2st filter appear. options based on filtering first column and bringing back options from option 2 column
  
  output$control2 <- renderUI({
    x <- input$menu1
    
    choice2 <- ImportedData[ImportedData$Level1 %in% x,
                            "Level2"]
    selectInput("menu2", "Select Level 2", choices = sort(unique(choice2)),selected=choice2, multiple=TRUE)
  })
  
  ## 3rd filter options based on 1st and 2nd filters results
  
  output$control3 <- renderUI({
    x <- input$menu1
    y <- input$menu2
    choice3 <- ImportedData[ImportedData$Level1 %in% x & ImportedData$Level2 %in% y,
                            "Level3"]
    
    selectInput("menu3", "Select Level 3", choices = sort(unique(choice3)),selected=choice3,multiple=TRUE)
  })
  
  ## 4th filter based on first three
  
  output$control4 <- renderUI({
    x <- input$menu1
    y <- input$menu2
    z <- input$menu3
    choice4 <- ImportedData[ImportedData$Level1 %in% x & ImportedData$Level2 %in% y & ImportedData$Level3 %in% z,
                            "Level4"]
    
    selectInput("menu4", "Select Level 4", choices = sort(unique(choice4)),selected=choice4,multiple=TRUE)
  })
  
  ## 5th filter based on first three
  
  #  output$control5 <- renderUI({
  #   x <- input$menu1
  #  y <- input$menu2
  # z <- input$menu3
  #zz <- input$menu4
  #  if (any(
  #   is.null(x),
  #  is.null(y),
  # is.null(z),
  #is.null(zz)
  #  ))
  #   return("Select")
  
  #  choice5 <- ImportedData[ImportedData$Level1 %in% x & ImportedData$Level2 %in% y & ImportedData$Level3 %in% z
  #                         & ImportedData$Level4 %in% zz,
  #                        "Level5"]
  
  #selectInput("menu5", "Menu5", choices = choice5,selected=choice5,multiple=TRUE)
  #})
  
  
  ## creates a variable select of MergedContextualDataWithSubset
  output$xcol <- renderUI({
    
    choicexcol <- names(MergedContextualDataWithSubset$df)[-(1:4)]
    selectInput("xcol", "Metric part 1", choices = choicexcol)
  })
  output$ycol <- renderUI({
    
    choiceycol <- names(MergedContextualDataWithSubset$df)[-(1:4)]
    selectInput("ycol", "Metric part 2", choices = choiceycol)
    
  })
  output$Delete <- renderUI({
    
    Delete <- names(MergedContextualDataWithSubset$df)[-1]
    selectInput("Delete", "Delete", choices = Delete)
    
    
  })
  output$default <- renderText({ input$xcol;input$ycol })
  
})
