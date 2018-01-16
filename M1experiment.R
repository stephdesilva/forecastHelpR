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

data(M1)
data <- M1
parameters <- setParameters()

retrieval <- retrieveDates(source, parameters)

tsArray <- buildTSArray(data, parameters$nObjects,  retrieval$minDate,
                        retrieval$maxDate, parameters$freqList,
                        parameters$skipList)


initialObjects <- initialiseModels(parameters$freqList,
                                   tsArray,
                                   parameters$nObjects,
                                   parameters$features,
                                   parameters$numFitSources,
                                   parameters$numGen,
                                   parameters$modelList,
                                   parameters$h,
                                   parameters$maxCluster)

metaY <- initialObjects$MY
fit <- initialObjects$Fi
bestFit <- initialObjects$BF

outcomes <- evolutionaryStage(data,
                              parameters$numGen,
                              parameters$nObjects,
                              metaY,
                              tsArray,
                              parameters$permuteParam,
                              parameters$modelList,
                              bestFit,
                              fit,
                              parameters$h)

wL <- weakLinks(parameters$weakLinkPercent, outcomes$MY, outcomes$BF)

paramList <- list("weakLinks" = wL,
                  "source" = data,
                  "MY" = outcomes$MY,
                  "BF" = outcomes$BF,
                  "Fi" = outcomes$Fi
                  )

visualiseWeakLinks(paramList)


newModel <- list("arima", "arima", "arima", "arima",
                 "arima","arima","arima","holtDamped",
                 "hwDSeasonalAdditive", "arima", "arima")

usedBC <- c(0, 1, 1, 0,
            0, 1, 1,0,
            1, 0, 0 )

endOutcomes <- fixWeakLinks(paramList, newModel, parameters, usedBC, data)



## Creating a visual

visualiseEndOutcomes(parameters, endOutcomes)





