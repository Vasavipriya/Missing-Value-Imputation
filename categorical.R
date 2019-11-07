library(forcats)
factor(c("NA","0","b","x"))
data<-read.csv(file.choose(),stringsAsFactors = TRUE,header = F,na.strings=c("","NA"))
data
summary(data)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)
library(mice)
md.pattern(data)
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
tempData <- mice(data,m=5,maxit=5,meth='polyreg')
summary(tempData)
tempData$imp$V6
tempData$meth
completedData1 <- complete(tempData,1)
completedData2 <- complete(tempData,2)
completedData3 <- complete(tempData,3)
completedData4 <- complete(tempData,4)
completedData5 <- complete(tempData,5)
write.table(completedData1, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/Sonar/Sonar_NW_20/Sonar_NW_20a.csv",sep=",",row.names=FALSE,col.names=FALSE)
write.table(completedData2, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/Sonar/Sonar_NW_20/Sonar_NW_20b.csv",sep="," ,row.names=FALSE,col.names=FALSE)
write.table(completedData3, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/Sonar/Sonar_NW_20/Sonar_NW_20c.csv",sep="," ,row.names=FALSE,col.names=FALSE)
write.table(completedData4, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/Sonar/Sonar_NW_20/Sonar_NW_20d.csv",sep="," , row.names=FALSE,col.names=FALSE)
write.table(completedData5, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/Sonar/Sonar_NW_20/Sonar_NW_20e.csv",sep="," , row.names=FALSE,col.names=FALSE)
obs<-original<-read.csv(file.choose(),stringsAsFactors = TRUE,header = F)
sim<-completedData1
library(hydroGOF)
library(Metrics)
abs(sim-obs)
mae(sim,obs,na.rm=TRUE)
library(plyr)
count(original,v1)
table()
 

