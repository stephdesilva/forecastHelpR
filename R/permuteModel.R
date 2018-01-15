permuteModel <- function(y, choiceModel, permuteParam, modelList, bestFit,
                         fit, i, currentGen, h, metaY){
  if (runif(1, min = 0, max = 1) < permuteParam[1]){ # Possible model switch
    perturbModel <- floor(runif(1, min = 1, max = (length(modelList) + 1)))
    choiceModel <- names(modelList)[perturbModel]
    accFit <- estModel(modelList, choiceModel, y, h)
    usedBC <- 0
    sendLambda <- 0
  } else if (runif(1, min = 0, max = 1) < permuteParam[2]){ # Box Cox transformation
    yBC <- BoxCox(y, metaY[i,5])
    lambda <- vector("numeric", length = 3)
    lambda[1] <- 1
    lambda[2] <- metaY[i,5]

    bcFitInitial <- estModel(modelList, choiceModel, yBC, h) # current BC param
    bcFit <- matrix(NA, nrow = 3, ncol = length(bcFitInitial))
    bcFit[1,] <- bcFitInitial
    bcFit[2,] <- estModel(modelList, choiceModel, y, h) # no BC param
    lambda[3] <- runif(1, 0, 2)
    ynewBC <- BoxCox(y, lambda[3])
    bcFit[3,] <- estModel(modelList, choiceModel, ynewBC, h) #
    winner <- which.min(bcFit[,4])
    accFit <- bcFit[winner,]
    sendLambda <- lambda[winner]
    usedBC <- 1

  } else {
    accFit <- estModel(modelList, choiceModel, y, h)
    usedBC <- 0
    sendLambda <- 0
  }



  returnList <- decisionRule(accFit, bestFit, fit, metaY, i, choiceModel,
                             currentGen, sendLambda, usedBC)
  return(returnList)

}
