estimateModel <- function(index, choiceModel, y, fit,
                          bestFit, currentGen,lastSlot, metaDataFeatures){
  for (j in 1:length(index)){
    if (choiceModel == "arima"){
      accFit <- accuracy(auto.arima(y[,index[j]]))
    } else if (choiceModel == "ann") {
      accFit <- accuracy(nnetar(y[,index[j]]))
    } else {
      print ("Arrgggh don't have that model")
      break
    }

    if (accFit[4] < bestFit[index[j]]){
      fit[index[j], ,currentGen] <- accFit
      bestFit[index[j]] <- accFit[,4]
      metaDataFeatures[index[j], lastSlot, currentGen] <- choiceModel
    }
  }

  return(list("MD" = metaDataFeatures, "BF" = bestFit, "Fi" = fit))
}
