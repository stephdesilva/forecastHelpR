#' Initialise Models
#'
#' Initialise models using start points clustered on metadata.
#' 1. Work out which group each time series belongs to. They are grouped by frequency type.
#' 2. Estimate metafeatures for each time series in each group
#' 3. Cluster time series up to maximum number of clusters
#' 4. In each cluster, assess an initial model and estimate for all
#' 5. Return information o fit and model.
#'
#' @param freqList a tibble with frequency information
#' @param tsArray the time series array with all the data, organised by frequency types
#' @param nObjects number of objects in total to be estimated
#' @param features features to be estimated
#' @param numFitSources number of sources for measures of fit
#' @param numGen the total number of generations to be used in the experiment
#' @param modelList list of models to be considered
#' @param h forecast horizon
#' @param maxCluster maximum number of clusters allowed
#'
#' @return "MDF" = metaDataFeatures - estimates of features on each time series,
#' "BF" = bestFit so far obtained for each time series.
#' "Fi" = fit, current fit for each time series as well as previous efforts
#' "MY" = metaY metadata on each series, including best model choice so far.
#' @export
#'
#' @examples
#'
#' initialObjects <- initialiseModels(parameters$freqList,
#'       tsArray,
#'      nObjects,
#'      features,
#'      numFitSources,
#'      numGen,
#'      modelList,
#'      h,
#'      maxCluster)

#'
initialiseModels <- function(freqList,
                             tsArray,
                             nObjects,
                             features,
                             numFitSources,
                             numGen,
                             modelList,
                             h,
                             maxCluster){
  metaY <-matrix(NA, nrow = nObjects, ncol = 6)
  metaY <- as.data.frame(metaY)
  colnames(metaY) <- c("index", "frequency", "group", "bestModel", "bestBoxCox", "bestModelUsedBC")
  freqMembership <- list()
  metaY[,5] = rep(1,nrow(metaY))
  metaY[,6] = rep(0,nrow(metaY))

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


  fit <- array(NA, c(nObjects, numFitSources, numGen))
  bestFit <- matrix(1000000, nrow = nObjects, ncol=1) # ridiculously high fitness to come down from


  for (k in 1:nrow(freqList)){
    for (i in 1:maxCluster){
       y <- t(tsArray[,k,])
      index <- which((metaY$frequency == k) & metaY$group == i)

      ## next decide how to model each series.
      # Plan sample randomly a number from each cluster, estimate
      # in all the ways contended, compare the MAPSE for each and decided on the best
      # in each group

      choiceModel <- assessInitialModel(y[, index], modelList, h)


      output <- estimateModel(metaY, index, choiceModel, y, fit,
                              bestFit, 1,lastSlot, metaDataFeatures,
                              modelList, h)


      bestFit <- output$BF
      fit <- output$Fi
      metaY <- output$MY
      metaY[index, 4] <- choiceModel


      ## Calculate forecasting criteria -> if we have a new best estimate, keep in best estimates
      # otherwise abandon


    }  # end group loops
  }    # end frequency loops

  returnList <- list("MDF" = metaDataFeatures, "BF" = bestFit, "Fi" = fit,
                     "MY" = metaY)
  return(returnList)
}
