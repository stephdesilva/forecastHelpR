decisionRule <- function(accFit, bestFit, fit, metaY, j, choiceModel, currentGen){
  if (accFit[4] < bestFit[j]){
    bestFit[j] <- accFit[,4]
    metaY[j,4] <- choiceModel
    fit[j, ,currentGen] <- accFit
  } else {
    fit[j, ,currentGen] <- accFit
  }


  return(list("BF" = bestFit, "Fi" = fit, "MY" = metaY))
}
