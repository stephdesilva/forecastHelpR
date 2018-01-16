fixWeakLinks <- function(paramList, newModel, parameters, usedBC, data){

  wL <- paramList$weakLinks

  for (i in 1:length(wL)){

    j <- as.numeric(wL[i])
    y <- data[[j]]
    y <- y$x
    endOutcomes <- manageWeakLinks(y,
                                   paramList,
                                   as.numeric(wL[i]),
                                   newModel[i],
                                   parameters$modelList,
                                   parameters$h,
                                   parameters$numGen,
                                   usedBC[i])

    paramList <- list("weakLinks" = wL,
                      "source" = data,
                      "MY" = endOutcomes$MY,
                      "BF" = endOutcomes$BF,
                      "Fi" = endOutcomes$Fi
                      )


  }
  return(paramList)
}
