evolutionaryStage <- function(data, numGen, nObjects, metaY, tsArray,
                              permuteParam, modelList, bestFit, fit, h){

    for (i in 1:nObjects){

      freqPosition <- metaY[i,2]
      z <- na.omit(tsArray[i,freqPosition,])
      if (length(unique(z)) < 1){
        z <- getYData(data[[i]])
        z <- z$d
        metaY[i,4] <- "arima" ## Just going to default to arima here.
      }

      for (p in 1:numGen){

        overallFit <- permuteModel(z, metaY[i,4],
                                 permuteParam, modelList, bestFit, fit, i,
                                 p, h, metaY)
        bestFit <- overallFit$BF
        fit <- overallFit$Fi
        metaY <- overallFit$MY
    }


  }

  return(list("BF" = bestFit, "Fi" = fit, "MY" = metaY))

}
