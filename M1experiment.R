#M1 experiment
rm(list=ls(all=TRUE)) # clear the workspace of anything else

# a script to examine the process from the beginning
library(forecastHelpR)
library(tidyverse)
library(readxl)
library(abind)
library(forecast)
library(Mcomp)

# parameters
skipList <- c(51, 217, 218, 493, 494, 223, 442, 548, 549, 550, 551,
              552, 553, 554, 555, 556, 557, 558, 559)
nObjects <- 1001
proposedMaxLength <- 10000
numGen <- 5
maxCluster <- 2
stopNow <- 100
corThreshold <- 0.2
numberGroupTests <- 1
currentGen <- 1
numFitSources<- 7
weakLinkPercent <- 0.1
#freqList <- c("annual", "quarterly", "monthly")
freqList <- tibble(
            freq = c(1, 4, 12),
            named = c("annual", "quarterly", "monthly")
)

# list of things in the metadata for each series

IC <- c("AIC", "AICc", "BIC", "HQ", "RSS")
measures <- c("MAPE", "MASE", "MdAPE", "MdASE")
features <- c("frequency", "trend", "seasonal", "autocorrelation",
              "non-linear", "skewness",
              "kurtosis", "Hurst", "Lyapunov",
              "dc autocorrelation", "dc non-linear", "dc skewness",
              "dc kurtosis")
meta <- c("clusterNumber", "rank", "gensSinceImprovement",
          "modelChoice")

features <- c(IC, measures, features, meta)

lastSlot <- length(features)


data(M1)


endDates <- findMinMaxDates(M1, freqList, nObjects, skipList)
          # skipList has objects that don't have a formalised start date


# Plan, to create an array with slots from minimum date to maximum date and insert
# each time series into that slot. Like an excel sheet.
# next step, get this into a data frame adding NAs to pad beginning and end of series


minDate <- endDates[[1]]

maxDate <- endDates[[2]]

tsArray <- buildTSArray(M1, nObjects,  minDate, maxDate, freqList, skipList)

# assess frequency membership
## TO DO create a function that will give a list of objects in each frequency type

metaY <-data.frame(NA, nrow = nObjects, ncol = 3)
colnames(metaY) <- c("index", "frequency", "group")
freqMembership <- list()

for (k in 1: nrow(freqList)){
  name <- as.character(freqList[k,2])
  y <- t(tsArray[,k,])
  groupMembership <- {}
  for (i in 1:nObjects){
    metaY[i,1] <- i
    if(sum(!is.na(y[,i]))!=0){
      groupMembership <- c(groupMembership, i)
      metaY[i,2] <- k
    }
  }
  freqMembership[[length(freqMembership) + 1]] <- list(groupMembership)
}

## assess features


metaDataFeatures <- array(data = NA, dim = c(nObjects,length(features),nrow(freqList)))


for (k in 1: nrow(freqList)){
  y <- t(tsArray[,k,])
  objectsFreq <- unlist(freqMembership[[k]])
  y <- y[,objectsFreq]
  for (i in 1:ncol(y)){
      metaDataFeatures[i,,k] <- featureCalc(y[,i], freqList[k,1])

  }
}

## assess clusters
# remember we only want to pass metaDataFeatures where there is not all NAs.
groups <-list()

for (i in 1:nrow(freqList)){
    objectsFreq <- as.numeric(unlist(freqMembership[[i]]))
    lenObjects <- length(objectsFreq)
    y <- metaDataFeatures[1:lenObjects,,i]


    sortGroup <- assessClusters(y, maxCluster)
    metaY[which(metaY$frequency == i),3] <- sortGroup

}






  ## if > 1 series in any cluster, check correlation matrix between them.
  # on detrended/deseasonalised data!
  # do they all start and end at the same time? this is going to be an issue.
  # only check correlations between the core series.

fit <- array(NA, c(ncol(y), numFitSources, numGen))
bestFit <- matrix(nObjects, nrow = ncol(y), ncol=1) # ridiculously high fitness to come down from


 for (k in 1:nrow(freqList)){
   print ("K:")
   print(k)
   for (i in 1:maxCluster){
     print("i:")
     print(i)
     y <- t(tsArray[,k,])
     index <- which((metaY$frequency == k) & metaY$group == i)

    ## next decide how to model each series.
    # Plan sample randomly a number from each cluster, estimate
    # in all the ways contended, compare the MAPSE for each and decided on the best
    # in each group

    choiceModel <- assessInitialModel(y[, index])

    output <- estimateModel(index, choiceModel, y, fit,
                            bestFit, currentGen,lastSlot, metaDataFeatures)

    metaDataFeatures <- output$MD
    bestFit <- output$BF
    fit <- output$Fi


    ## Calculate forecasting criteria -> if we have a new best estimate, keep in best estimates
    # otherwise abandon


  }  # end group loops
}    # end frequency loops

