decisionRule <- function(accFit, bestFit, fit, metaY, j,
                         choiceModel, currentGen, lambda, usedBC){
  if (accFit[4] < bestFit[j]){
    bestFit[j] <- accFit[4]
    metaY[j,4] <- choiceModel
    metaY[j,5] <- lambda
    metaY[j,6] <- usedBC
    if (length(accFit) < 7){
      fit[j,1:length(accFit),currentGen] <- accFit
    } else {
      fit[j, ,currentGen] <- accFit
    }

  } else {
    if (length(accFit) < 7){
      fit[j,1:length(accFit),currentGen] <- accFit
    } else {
      fit[j, ,currentGen] <- accFit
    }
  }


  return(list("BF" = bestFit, "Fi" = fit, "MY" = metaY))
}
