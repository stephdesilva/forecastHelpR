permuteModel <- function(y, choiceModel, permuteParam, modelList, bestFit,
                         fit, i, currentGen){
  if (runif(1, min = 0, max = 1) < permuteParam){
    perturbModel <- floor(runif(1, min = 1, max = (length(modelList) + 1)))
    choiceModel <- modelList[perturbModel]
  }

  accFit <- estModel(choiceModel, y)
  returnList <- decisionRule(accFit, bestFit, fit, metaY, i, choiceModel, currentGen)
  return(returnList)

}
