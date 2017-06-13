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


DefinitionsData<-Definitions_10_



myDataFrame <-
  read.csv("/Users/datascience4/Desktop/Definitions11.csv",
           header=TRUE, sep=",", stringsAsFactors=FALSE)


DefinitionsData<-myDataFrame[,-1]

myDataFrame <-
  read.csv("/Users/datascience4/Desktop/Definitions.csv",
           header=TRUE, sep=",", stringsAsFactors=FALSE)



