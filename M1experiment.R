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
library(patchwork)
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
              "source" = data)

manageWeakLinks(paramList)


## Creating a visual

gens <- as.vector(seq(1,parameters$numGen, 1))
object <- rep(1, parameters$numGen)
objData <- as.data.frame(cbind(object, outcomes$Fi[1,4,], gens, metaY[1,3]))
chartData <- objData



for (i in 2:parameters$nObjects){
  object <- rep(i, parameters$numGen)
  objData <- as.data.frame(cbind(object, outcomes$Fi[i,4,],gens, metaY[i,3]))
  chartData <- rbind(chartData, objData)

}

colnames(chartData) <- c("Object #", "Fit", "Generation", "Group")


ggplot(chartData)+
  geom_line(aes(x = Generation, y = Fit, colour= Group))+
  theme_light()+
  facet_wrap(~Group)

# Determine weak links



