
data<-read.csv(file.choose(),header=T)
data
summary(data)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)

library(mice)
md.pattern(data)

library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(data,m=5,maxit=5,meth='pmm')
summary(tempData)

tempData$imp$X1
tempData$meth

completedData <- complete(tempData,1)

xyplot(tempData,X0.2 ~X5.1+X3.5 ,pch=18,cex=1)
densityplot(tempData)
stripplot(tempData, pch = 20, cex = 1.2)

modelFit1 <- with(tempData,lm(X0.2 ~X5.1+X3.5))
summary(modelFit1)

summary(pool(modelFit1))

tempData2 <- mice(data,m=5,maxit=5)
modelFit2 <- with(tempData2,lm(X3 ~X1+X2))
summary(modelFit2)
summary(pool(modelFit2))

completedData