library(shiny)
library(plotly)

# this is the location of the main data file on my windows machine
# ImportedData <-read.csv("C:\\Users\\neog968\\Desktop\\wgadata.csv",header=TRUE)
# this is the location of my main data file
#ImportedData <-read.csv("/Users/datascience4/Documents/datatool/wgadata.csv",header=TRUE)

#create a data frame of unique rows of dimenstions from the same source as import data

DefinitionsData<-ImportedData

#Drop LA code/year column and value column
DefinitionsData<-DefinitionsData[,-c(1,8)]
#Remove duplicate rows so left with just the possible dimensions
DefinitionsData <- unique(DefinitionsData)


#Duplicate definitions data to be filtered in parallel to main import data
DefinitionsData2<-DefinitionsData
#Create duplicate ID field as ID will disappear in merge
DefinitionsData2$ID2<-DefinitionsData2$ID


DefinitionsData$All<-DefinitionsData$ID



# this data are dimensions from the main data (probably should derive it from the one source later)
# the aim is to filter this in parallel to the main data to have a record of the unique dimension IDs
# that are used for aggregate measures created, as a record of what goes into a measure
# it would also be good to use these IDs to filter the default choices of the main data filters
# so a user could select a measure, and the default selection of each input filter will reflect the
# selections made creating that variable. other options would still be available to select as this 
# is defined by the cascading filter logic. (yet to fully implement)

#Definitions <-read.csv("/Users/datascience4/Documents/datatool/wgadimensions.csv",header=TRUE)


# related to above this is because when I merge based on ID the common ID field is not duplicated. 
# I want a list of all possible dimension IDs and to merge it with the filtered list of IDs
# This is so that when merged on ID ID2 will not disapear (not yet implemented)

#Definitions$ID2<-Definitions$ID

# 'Contextual data' is the preloaded data that will include contextual data such as population, deprivation etc
# it also provides the template for the output data set. Aggregates based on filter selections will be merged
# into a table with the contextual data based on code (made up of LA Code and Year)

# Ignore, Windows location, ContextualData <-read.csv("C:\\Users\\neog968\\Desktop\\contextualdata.csv",header=TRUE)

# ContextualData <-read.csv("/Users/datascience4/Documents/datatool/contextualdata.csv",header=TRUE)

# Main Server:

shinyServer(function(input, output, session) {
  
  # Ignore for now this is workin progress on filtering dimensions definitions
  
  #display definitions table
  #output$ImportedDataFiltered2 <- DT::renderDataTable(
  #  DT::datatable(Mergeddef$df, options = list(searching = FALSE),
  #               rownames= FALSE))
  
  
  #This displays the main imported data filtered by input filters
  
  output$ImportedDataFiltered2 <- DT::renderDataTable(
    DT::datatable(MergedDefinitionWithFilteredDefinitions$df, options = list(searching = FALSE),
                  rownames= FALSE))
  
  
  output$ImportedDataFiltered <- DT::renderDataTable(
    
    DT::datatable(FilteredImportData(), options = list(searching = FALSE),
                  rownames= FALSE)
  )
  
  
  
  
  
  
  # This is the download handler for the filtered imported data, currently doesnt create filename correctly
  output$DownloadFilteredData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(FilteredImportData$df, file)
    }
  )
  
  # This filters the main imported data by the four input filters
  
  FilteredImportData<-reactive({FilteredImportData <-
    ImportedData<-ImportedData[ImportedData$Level1 %in% input$menu1,]
  ImportedData<-ImportedData[ImportedData$Level2 %in% input$menu2,]
  ImportedData<-ImportedData[ImportedData$Level3 %in% input$menu3,]
  ImportedData<-ImportedData[ImportedData$Level4 %in% input$menu4,]
  ImportedData<-ImportedData[ImportedData$Level5 %in% input$menu5,]
  })
  
  # This filters the definitions data in parralel to main imported data by the four input filters
  
  FilteredDefinitionsData<-reactive({
    DefinitionsData2 <-DefinitionsData2[DefinitionsData2$Level1 %in% input$menu1,]
    DefinitionsData2<-DefinitionsData2[DefinitionsData2$Level2 %in% input$menu2,]
    DefinitionsData2<-DefinitionsData2[DefinitionsData2$Level3 %in% input$menu3,]
    DefinitionsData2<-DefinitionsData2[DefinitionsData2$Level4 %in% input$menu4,]
    DefinitionsData2<-DefinitionsData2[DefinitionsData2$Level5 %in% input$menu5,]
  })
  
  
  
  
  
  
  # This aggregates the imported data value filed based on filter selection down to 'code' level
  # (this a a unique code for an LA in a particular year). 
  # As the input filters are themselves rendered outputs an error message will
  # appear unless these 'req' conditions are in place
  
  SubsetData <-
    
    reactive({
      req(input$def)
      req(input$menu1)
      req(input$menu2)
      req(input$menu3)
      req(input$menu4)
      
      
      aggregate(Value ~ Code, FilteredImportData(), sum)
    })
  
  
  # Ignore this for now, it is for the dimension definitions part I am working on
  # SubsetDataDefinition <-
  
  # reactive({
  #    req(input$menu1)
  #    req(input$menu2)
  #    req(input$menu4)
  #    req(input$menu3)
  #    #req(input$menu5)
  
  #   unique(FilteredImportData()['ID'])
  #  })
  
  ## This takes the name inputed in the text input to be used later as an aggregate name
  namerev<-reactive({
    as.character(paste(input$text))
  })
  
  
  # ignore for now, for dimension definitions
  #SubsetDataDefinition2<-reactive({
  #d <- SubsetDataDefinition()
  #colnames(d)[colnames(d)=='ID'] <- namerev()
  #d
  #})
  
  
  
  ## This replaces the column label 'Value' with the inputted name
  SubsetData2<-reactive({
    d <- SubsetData()
    colnames(d)[colnames(d)=='Value'] <- namerev()
    d
  })
  
  # This replaced ID2 with the name of the created metric, to later be merged into the definitions file
  
  FilteredDefinitionsData2<-reactive({
    d <- FilteredDefinitionsData()
    colnames(d)[colnames(d)=='ID2'] <- namerev()
    d
  })
  
  
  
  ## This table displays the SubsetData (aggregates based on filter selection)
  
  output$SubsetData <- DT::renderDataTable(
    DT::datatable(SubsetData(), options = list(searching = FALSE),
                  rownames= FALSE))
  
  
  # Creates output dataset of contextual data with derived aggregates and ratios
  MergedContextualDataWithSubset <- reactiveValues()
  MergedContextualDataWithSubset$df<-ContextualData
  
  # merges subsetdata aggegate with the contextual dataframe when 'Merge' is clicked.
  # A new column is created for each click
  observe({
    if(input$Merge > 0) {
      isolate(
        
        MergedContextualDataWithSubset$df <- merge
        (MergedContextualDataWithSubset$df,SubsetData2(),by="Code",all=TRUE)
      )
    }
  })
  
  #This merges the filtered IDs into all the possible IDs, to give a definition file
  
  MergedDefinitionWithFilteredDefinitions <- reactiveValues()
  MergedDefinitionWithFilteredDefinitions$df<-DefinitionsData
  
  observe({
    if(input$Merge > 0) {
      isolate(
        
        MergedDefinitionWithFilteredDefinitions$df <- merge
        (MergedDefinitionWithFilteredDefinitions$df,FilteredDefinitionsData2()[6:7],by="ID",all=TRUE)
      )
    }
  })
  
  
  
  
  
  # If 'divide' selected in operator input one selected variable is divided by another selected variable
  observe({
    
    if(input$button > 0) {
      
      isolate(if(input$operator=="Divide by") {
        
        newvar <- isolate((MergedContextualDataWithSubset$df[[input$xcol]] / MergedContextualDataWithSubset$df[[input$ycol]]))
        isolate(MergedContextualDataWithSubset$df <- cbind(MergedContextualDataWithSubset$df, newvar))
        isolate(colnames(MergedContextualDataWithSubset$df)[colnames(MergedContextualDataWithSubset$df)=='newvar'] <- as.character(paste(input$text2)))
        
        
      })
      
      # If 'multiply' selected in operator input one selected variable is divided by another selected variable  
      isolate(if(input$operator=="Multiply") {
        
        newvar <- isolate((MergedContextualDataWithSubset$df[[input$xcol]] * MergedContextualDataWithSubset$df[[input$ycol]]))
        isolate(MergedContextualDataWithSubset$df <- cbind(MergedContextualDataWithSubset$df, newvar))
        isolate(colnames(MergedContextualDataWithSubset$df)[colnames(MergedContextualDataWithSubset$df)=='newvar'] <- as.character(paste(input$text2)))
        
        
      })
      
      # If 'Add' selected in operator input one selected variable is divided by another selected variable
      
      isolate(if(input$operator=="Add") {
        
        newvar <- isolate((MergedContextualDataWithSubset$df[[input$xcol]] + MergedContextualDataWithSubset$df[[input$ycol]]))
        isolate(MergedContextualDataWithSubset$df <- cbind(MergedContextualDataWithSubset$df, newvar))
        isolate(colnames(MergedContextualDataWithSubset$df)[colnames(MergedContextualDataWithSubset$df)=='newvar'] <- as.character(paste(input$text2)))
        
        
      })
      
      # If 'Subtract' selected in operator input one selected variable is divided by another selected variable
      
      isolate(if(input$operator=="Subtract") {
        
        newvar <- isolate((MergedContextualDataWithSubset$df[[input$xcol]] - MergedContextualDataWithSubset$df[[input$ycol]]))
        isolate(MergedContextualDataWithSubset$df <- cbind(MergedContextualDataWithSubset$df, newvar))
        isolate(colnames(MergedContextualDataWithSubset$df)[colnames(MergedContextualDataWithSubset$df)=='newvar'] <- as.character(paste(input$text2)))
      })
    }
    
    # Need another operator for option for turning negatives to absolute values
    
    
  })
  
  
  
  
  
  
  # Deletes last column from output dataframe (based on counting number of columns with length)
  # Would like to replace this with the ability to select variable to be delted
  observe({
    if(input$DeleteButton > 0) {
      isolate(MergedContextualDataWithSubset$df <- MergedContextualDataWithSubset$df[-c(length( names( MergedContextualDataWithSubset$df ) ))]
      )
    }
  })
  
  
  # Display the final output data of contextual data, aggregates and calculations
  output$MergedData <- DT::renderDataTable(
    DT::datatable(MergedContextualDataWithSubset$df, options = list(searching = FALSE),
                  rownames= FALSE))
  
 
  # Download handler for final output data
  
  output$testdownload <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(MergedContextualDataWithSubset$df, file)
    }
  )
  
  ## UI Elements
  
  
  
  
  
  output$control1 <- renderUI({
    req(input$def)
    
    X2<- MergedDefinitionWithFilteredDefinitions$df[[input$def]]
    choicesAlt<-ImportedData[ImportedData$ID %in% X2,
                             "Level1"]
    
    #unique(MergedDefinitionWithFilteredDefinitions$df[['Level1']])
    
    
    selectInput("menu1", "Select Level 1", choices = unique(ImportedData$Level1),
                selected= choicesAlt['2'])
  })
  
  
  ## Makes 2st filter appear. choices based on Level2 options after Level1 filter applied
  
  output$control2 <- renderUI({
    req(input$def)
    
    x <- input$menu1
    X2<- MergedDefinitionWithFilteredDefinitions$df[[input$def]]
    choice2 <- ImportedData[ImportedData$Level1 %in% x,
                            "Level2"]
    choicesAlt2<-ImportedData[ImportedData$ID %in% X2,
                              "Level2"]
    selectInput("menu2", "Select Level 2", choices = sort(unique(choice2)),selected=choicesAlt2, multiple=TRUE)
  })
  
  ## Makes 3rd filter appear. choices based on previous filters
  
  output$control3 <- renderUI({
    req(input$def)
    
    x <- input$menu1
    y <- input$menu2
    X2<- MergedDefinitionWithFilteredDefinitions$df[[input$def]]
    choicesAlt3<-ImportedData[ImportedData$ID %in% X2,
                              "Level3"]
    choice3 <- ImportedData[ImportedData$Level1 %in% x & ImportedData$Level2 %in% y,
                            "Level3"]
    
    selectInput("menu3", "Select Level 3", choices = sort(unique(choice3)),selected=choicesAlt3,multiple=TRUE)
  })
  
  ## 4th filter based on first three
  
  output$control4 <- renderUI({
    req(input$def)
    
    x <- input$menu1
    y <- input$menu2
    z <- input$menu3
    X2<- MergedDefinitionWithFilteredDefinitions$df[[input$def]]
    choicesAlt4<-ImportedData[ImportedData$ID %in% X2,
                              "Level4"]
    choice4 <- ImportedData[ImportedData$Level1 %in% x & ImportedData$Level2 %in% y & ImportedData$Level3 %in% z,
                            "Level4"]
    
    selectInput("menu4", "Select Level 4", choices = sort(unique(choice4)),selected=choicesAlt4,multiple=TRUE)
  })
  
  output$controlL5 <- renderUI({
    req(input$def)
    
    x <- input$menu1
    y <- input$menu2
    z <- input$menu3
    new <- input$menu4
    X2<- MergedDefinitionWithFilteredDefinitions$df[[input$def]]
    choicesAlt5<-ImportedData[ImportedData$ID %in% X2,
                              "Level5"]
    choice5 <- ImportedData[ImportedData$Level1 %in% x & ImportedData$Level2 %in% y & ImportedData$Level3 %in% z
                            & ImportedData$Level4 %in% new,
                            "Level5"]
    
    selectInput("menu5", "Select Level 5", choices = sort(unique(choice5)),selected=choicesAlt5,multiple=TRUE)
  })
  
  
  
  
  ## 5th filter based on first three, dont think I need to filter on this column and too many options so deactivated it
  
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
  
  
  # select a variable to use in calculation. set so first 4 are not selectable as these are factors not numerical
  output$xcol <- renderUI({
    
    choicexcol <- names(MergedContextualDataWithSubset$df)[-(1:4)]
    selectInput("xcol", "Metric part 1", choices = choicexcol)
  })
  
  output$def <- renderUI({
    
    choicedef <- names(MergedDefinitionWithFilteredDefinitions$df)[-(1:6)]
    
    selectInput("def", "Default filters based on:", choices = choicedef,selected="ID")
  })
  
  # select 2nd variable to use in calculation. set so first 4 are not selectable as these are factors not numerical
  
  output$ycol <- renderUI({
    
    choiceycol <- names(MergedContextualDataWithSubset$df)[-(1:4)]
    selectInput("ycol", "Metric part 2", choices = choiceycol)
    
  })
  
  # Delete button, select variable to delete, not yet implemented
  output$Delete <- renderUI({
    
    Delete <- names(MergedContextualDataWithSubset$df)[-1]
    selectInput("Delete", "Delete", choices = Delete)
    
    
  })
  
  output$kmeansvariables <- renderUI({
    
    choiceycol <- names(workingdata)[-(1:5)]
    selectInput("Kxcol", "Choose variables for clustering", choices = choiceycol, multiple=TRUE, selected=names(workingdata)[-(1:7)])
    
  })
  
  
  output$kmeansLAtype <- renderUI({
    
    selectInput("kmeansLAtype", "kmeansLAtype", choices = unique(workingdata$Type))
  })
  
  
  # from shiny gallery k-means clustering need to adapt
  
  kmeansdata <- workingdata
  
  KselectedData <- reactive({
    kmeansdata[kmeansdata$Type %in% input$kmeansLAtype, c(input$Kxcol)]
  })
  
  KselectedData2 <- reactive({
    cbind(kmeansdata[kmeansdata$Type %in% input$kmeansLAtype, c('Area',input$Kxcol)],clusters()$cluster)
    
 })
  
  
  clusters <- reactive({
    kmeans(KselectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(KselectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  # https://stackoverflow.com/questions/32570693/make-silhouette-plot-legible-for-k-means
  output$sil <- renderPlot({
    X <- KselectedData()
    D <- daisy(X)
    plot(silhouette(clusters()$cluster, D),col=unique(clusters()$cluster), border=NA)
  })
  
  
  # Display text for k tb
  
  output$kmeans <- renderPrint({
    dataset <- clusters()
    summary(dataset)
  })
  
  output$MergedData2 <- DT::renderDataTable(
    DT::datatable(KselectedData2(), options = list(searching = FALSE),
                  rownames= FALSE))
  
  
  
  
  
  
  
  
  # Fill in the spot we created for a plot
  
  output$barselect <- renderUI({
    
    choiceycol <- names(workingdata[ -c(1:5) ])
    selectInput("barselect", "Metric part 2", choices = choiceycol)
    
  })
  
  # Fill in the spot we created for a plot
  output$barchart <- renderPlotly({
    
    # Render a barplot
    # Basic barplot
    p<-ggplot(data=workingdata, aes
              (x=Area, y=workingdata[input$region])) +
      geom_bar(stat="identity")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    p
    
  })
  
  
  # Fill in the spot we created for a plot
  output$boxjitter <- renderPlotly({
    
    # Render a boxplot
    p <- ggplot(workingdata, aes(Type, workingdata[,input$region]))
    p + geom_boxplot()+ geom_jitter(width = 0.2)
    
  })
  
  output$chartvariable <- renderUI({
    selectInput("region", "Region:", 
                choices=colnames(workingdata)[-c(1:5)])
  })
  
  #temp[order(temp[, 1]),]
  
  
  data<-reactive({barchartdata$df[,input$region]
  })
  
  
})


