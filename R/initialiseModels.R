initialiseModels <- function(freqList, tsArray, nObjects, features,numFitSources, numGen){
  metaY <-matrix(NA, nrow = nObjects, ncol = 4)
  metaY <- as.data.frame(metaY)
  colnames(metaY) <- c("index", "frequency", "group", "bestModel")
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

      choiceModel <- assessInitialModel(y[, index])


      output <- estimateModel(metaY, index, choiceModel, y, fit,
                              bestFit, currentGen,lastSlot, metaDataFeatures)


      bestFit <- output$BF
      fit <- output$Fi
      metaY <- output$MY
      metaY[index, 4] <- choiceModel


      ## Calculate forecasting criteria -> if we have a new best estimate, keep in best estimates
      # otherwise abandon


    }  # end group loops
  }    # end frequency loops

  returnList <- list("MDF" = metaDataFeatures, "BF" = bestFit, "Fi" = fit, "MY" = metaY)
  return(returnList)
}
