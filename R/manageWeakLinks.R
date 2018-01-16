manageWeakLinks <- function(data, paramList, i, newModel, modelList, h,
                            numGen, usedBC){
  metaY <- paramList$MY
  bestFit <- paramList$BF
  fit <- paramList$Fi
  y <- data

  if (usedBC == 1){
    lambda <- BoxCox.lambda(y, method = "guerrero")
    yBC <- BoxCox(y, lambda)
    accFit <- estModel(modelList, newModel, yBC, h)
  } else {
    accFit <- estModel(modelList, newModel, y,h)
    lambda <- 0
  }

  returnList <- decisionRule(accFit, bestFit, fit, metaY, i,
                             newModel, numGen, lambda, usedBC)



  return(returnList)

}
