# this is just the testing script

# get some data


library(tidyverse)
library(readxl)
library(abind)
library(forecast)

# parameters
numGen <- 5
maxCluster <- 2
stopNow <- 100
corThreshold <- 0.2
numberGroupTests <- 1
currentGen <- 1
numFitSources<- 7
weakLinkPercent <- 0.1

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

productivity<- read_xls("data/52600550022016.xls", sheet="Table 4",
                        col_names = FALSE, skip=10, n_max = 17)

# the data is sideways and has difficult variable names - let's fix that!
productivity<-t(productivity) %>%
  as.data.frame()
colnames(productivity)<-c("LabourHW",
                          "LabourQA",
                          "Capital",
                          "Mutlifactor.Productivity.Null" ,
                          "MultifactorHW",
                          "MultifactorQA")

# Getting rid of empty space and unnecessary bits
productivity <- productivity[-1,]
productivity <- productivity[,c(1:3,5:6)]


# A few things going on here:
# 1. Replacing the ABS "na" with NA for R compatibility
# 2. Read the data in as numeric
# 3. Convert to data frame
# 4. Reindex to 2013-14 year
# 5. Convert to a time series object
productivity <- productivity %>%
  map(function(x) replace(x, x == "na", NA)) %>%
  map(function(x) x = as.numeric(as.character(x))) %>%
  as.data.frame()%>%
  sapply(function(vec){return(vec/vec[31])*100},
         simplify=TRUE) %>%
  ts(start = 1973)
# Reindex to 2003-2004 which is row 31 in data frame

productivity <- productivity[,-c(2,5)]
y <- productivity

#Hyndman et al 2016
#fit <- baggedETS(y)
#> fcast <- forecast(fit)
#> plot(fcast)

## Generate metadata matrix

metaDataFeatures <- array(data = NA, dim = c(ncol(y),length(features),numGen))

## assess features

assessFeatures <- function(x, position){
  features <- featureCalc(x)
}

for (i in 1: ncol(y)){
  metaDataFeatures[i,,1] <- assessFeatures(y[,i],i)
}

## assess clusters

hc <- hclust(dist(metaDataFeatures[,,1], method = "euclidean"), method="ward.D")

groups <- cutree(hc,maxCluster)

## if > 1 series in any cluster, check correlation matrix between them.
# on detrended/deseasonalised data!
# do they all start and end at the same time? this is going to be an issue.
# only check correlations between the core series.

fit <- array(NA, c(ncol(y), numFitSources, numGen))
bestFit <- array(10000, c(ncol(y), numFitSources))


for (i in 1:max(groups)){
  index <- which(groups == i)

  if (length(index) >= 2){
    dataGroup <- y[,index]
    dataDetrend <- {}
    for (j in 1:length(index)){
        dataDetrend <- cbind(dataDetrend,decomp(dataGroup[,j])$remainder)
    }

    groupCorrelation <- cor(dataDetrend)
    groupCorrelation <- diag(groupCorrelation)
    panelMembership <- which(groupCorrelation >= corThreshold)

    groupPanel <- dataGroup[,panelMembership]
    excessPanel <- dataGroup[,-panelMembership]
  } else {
    dataGroup <- y[,index]
  }


## next decide how to model each series.
# Plan sample randomly a number from each cluster, estimate
# in all the ways contended, compare the MAPSE for each and decided on the best
# in each group

  for (k in 1: numberGroupTests){
    if (is.null(ncol(dataGroup))){
      pick <- 1
      pick <- dataGroup
    } else {
      pick <- as.integer(runif(1, 1, ncol(dataGroup) + 1))
      pick <- dataGroup[,pick]
    }

    fitARIMA.pick <- accuracy(auto.arima(pick)) # this should be a functional of some kind
    fitANN.pick <- accuracy(nnetar(pick))

    if (fitARIMA.pick[4] <= fitANN.pick[4]){
      choiceModel <- "arima"
    } else {
      choiceModel <- "ann"
    }

  }

  # can we use panel techniques?
  # plm package


  # Maybe depends on the technique
# don't worry about this right now


# ????

### Estimate - need to work out the nested data frame

  for (j in 1:length(index)){
    if (choiceModel == "arima"){
      accFit <- accuracy(auto.arima(y[,index[j]]))
    } else {
      accFit <- accuracy(nnetar(y[,index[j]]))
    }

    if (accFit[4] < bestFit[index[j], 4]){
      fit[index[j], ,currentGen] <- accFit
      bestFit[index[j], ] <- accFit
      metaDataFeatures[index[j], lastSlot, currentGen] <- choiceModel
    }
  }





## Calculate forecasting criteria -> if we have a new best estimate, keep in best estimates
# otherwise abandon


}  # end group loops
## Assess weak links and perturb

weakLinks <- quantile(bestFit, weakLinkPercent, na.rm = TRUE)
weakLinks <-which(bestFit[,4] <= weakLinks)

for (i in weakLinks){
  ##test which model for each one
  fitARIMA.pick <- accuracy(auto.arima(y[,i])) # this should be a functional of some kind
  fitANN.pick <- accuracy(nnetar(y[,i]))

  picks <- cbind(fitARIMA.pick, fitANN.pick)
  bestPick <- which.min(picks[4])

  if (picks[bestPick] < bestFit[index[j], 4]){
    fit[index[i], ,currentGen] <- picks[bestPick]
    bestFit[index[i], ] <- picks[bestPick]
  }

}




# weak links in groups

# weak links across groups



# reassess weak links and update models go back to forecasting criteria

## Are we doing a manual reassessment? If so, how many series?

## Manual reassessments: show plots and current workings

# back up to clustering for the next generation. Perturb the residuals a bit??

