estimateModel <- function(metaY, index, choiceModel, y, fit,
                          bestFit, currentGen,lastSlot, metaDataFeatures,
                          modelList, h){
  for (j in 1:length(index)){


    estY <-na.omit(y[,index[[j]]])

    accFit <- estModel(modelList, choiceModel, estY,h)

    returnList <- decisionRule(accFit, bestFit, fit, metaY, index[j],
                               choiceModel, currentGen, 1, 0)
  }

  return(returnList)
}
