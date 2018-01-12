#M1 experiment
rm(list=ls(all=TRUE)) # clear the workspace of anything else
set.seed(1234)
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
numGen <- 10
maxCluster <- 2
stopNow <- 100
corThreshold <- 0.2
numberGroupTests <- 1
currentGen <- 1
numFitSources<- 7
weakLinkPercent <- 0.1
permuteParam <- 0.7

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
modelList <- c("arima", "ann")

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

initialObjects <- initialiseModels(freqList, tsArray, nObjects,
                                   features,numFitSources, numGen)

metaY <- initialObjects$MY
fit <- initialObjects$Fi
bestFit <- initialObjects$BF

for (p in 1:numGen){

  for (i in 1:nObjects){
      freqPosition <- metaY[i,2]
      z <- na.omit(tsArray[i,freqPosition,])
      if (length(unique(z)) < 1){
        z <- getYData(M1[[i]]) ##### NOTE THAT THERE IS A REFERENCE TO M1 HERE
        z <- z$d
        metaY[i,4] <- "arima" ## Just going to default to arima here.
      }
      overallFit <- permuteModel(z, metaY[i,4],
                                 permuteParam, modelList, bestFit, fit, i,
                                 p)
      bestFit <- overallFit$BF
      fit <- overallFit$Fi
      metaY <- overallFit$MY
  }


}

chartData <- fit[5,4,]
gens <- as.vector(seq(1,numGen, 1))
chartData <- as.data.frame(cbind(chartData, gens))


ggplot(chartData)+
  geom_line(aes(x = gens, y = chartData))+
  theme_light()

