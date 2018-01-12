estimateModel <- function(metaY, index, choiceModel, y, fit,
                          bestFit, currentGen,lastSlot, metaDataFeatures){
  for (j in 1:length(index)){
    if (is.na(choiceModel) | is.null(choiceModel)){
      choiceModel <- "arima" # default to arima
    }

    estY <-na.omit(y[,index[[j]]])

    accFit <- estModel(choiceModel, estY)

    returnList <- decisionRule(accFit, bestFit, fit, metaY, index[j], choiceModel, currentGen)
  }

  return(returnList)
}
